;;; (json parser) --- Guile JSON implementation.

;; Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-json.
;;
;; guile-json is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-json. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (json parser)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:export (json->scm
            json-string->scm))

;;
;; Miscellaneuos helpers
;;

(define (json-exception port)
  (throw 'json-invalid port))

(define (digit? c)
  (case c
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) #t)
    (else #f)))

(define (whitespace? c)
  (case c
    ((#\ht #\lf #\cr #\sp) #t)
    (else #f)))

(define (skip-whitespaces port)
  (match (peek-char port)
    ((? eof-object?) *unspecified*)
    ((? whitespace?)
     (read-char port)
     (skip-whitespaces port))
    (_ *unspecified*)))

(define (expect-string port expected return)
  (let loop ((n 0))
    (cond
     ((= n (string-length expected)) return)
     ((eqv? (read-char port) (string-ref expected n))
      (loop (+ n 1)))
     (else (json-exception port)))))

(define (expect-delimiter port delimiter)
  (match (read-char port)
    ((? eof-object?) (json-exception port))
    (ch (unless (eqv? ch delimiter) (json-exception port)))))

;;
;; Number parsing helpers
;;

;; Read + or -. If something different is found, return empty string.
(define (read-sign port string-port)
  (match (peek-char port)
    ((or #\+ #\-) (put-char string-port (read-char port)))
    (_ *unspecified*)))

;; Read digits [0..9].
(define (read-digits port string-port)
  (match (peek-char port)
    ((? eof-object?) *unspecified*)
    ((? digit?)
     (put-char string-port (read-char port))
     (read-digits port string-port))
    (_ *unspecified*)))

(define (read-exponent port string-port)
  (match (peek-char port)
    ((or #\e #\E)
     (read-char port)
     (put-char string-port #\e)
     (read-sign port string-port)
     (read-digits port string-port))
    (_ *unspecified*)))

(define (read-fraction port string-port)
  (match (peek-char port)
    (#\.
     (read-char port)
     (put-char string-port #\.)
     (read-digits port string-port))
    (_ *unspecified*)))

(define (read-positive-number port string-port)
  (read-digits port string-port)
  (read-fraction port string-port)
  (read-exponent port string-port))

(define (read-number port string-port)
  (match (peek-char port)
    ((? eof-object?) (json-exception port))
    (#\-
     (read-char port)
     (put-char string-port #\-)
     (read-positive-number port string-port))
    (#\0
     (read-char port)
     (put-char string-port #\0)
     (read-fraction port string-port)
     (read-exponent port string-port))
    ((? digit?)
     (read-positive-number port string-port))
    (_ (json-exception port))))

(define (json-read-number port)
  (string->number
   (call-with-output-string
     (lambda (string-port) (read-number port string-port)))))

;;
;; Object parsing helpers
;;

(define (read-pair port)
  ;; Read key.
  (let ((key (json-read-string port)))
    (skip-whitespaces port)
    (match (peek-char port)
      ((? eof-object?) (json-exception port))
      ;; Skip colon and read value.
      (#\:
       (read-char port)
       (cons key (json-read port)))
      (_ (json-exception port)))))

(define (json-read-object port)
  (expect-delimiter port #\{)
  (let loop ((pairs '()) (added #t))
    (skip-whitespaces port)
    (match (peek-char port)
      ((? eof-object?) (json-exception port))
      ;; End of object.
      (#\}
       (read-char port)
       (cond
        (added pairs)
        (else (json-exception port))))
      ;; Read one pair and continue.
      (#\"
       (let ((pair (read-pair port)))
         (loop (cons pair pairs) #t)))
      ;; Skip comma and read more pairs.
      (#\,
       (read-char port)
       (cond
        (added (loop pairs #f))
        (else (json-exception port))))
      ;; Invalid object.
      (_ (json-exception port)))))

;;
;; Array parsing helpers
;;

(define (json-read-array port)
  (expect-delimiter port #\[)
  (let loop ((values '()) (added #t))
    (skip-whitespaces port)
    (match (peek-char port)
      ((? eof-object?) (json-exception port))
      ;; Handle comma (make sure we added an element).
      (#\,
       (read-char port)
       (cond
        (added (loop values #f))
        (else (json-exception port))))
      ;; End of array (make sure we added an element).
      (#\]
       (read-char port)
       (cond
        (added (list->vector values))
        (else (json-exception port))))
      ;; This can be any JSON object.
      (_
       (let ((value (json-read port)))
         (loop (append values (list value)) #t))))))

;;
;; String parsing helpers
;;

(define (read-hex-digit->integer port)
  (match (read-char port)
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    ((or #\A #\a) 10)
    ((or #\B #\b) 11)
    ((or #\C #\c) 12)
    ((or #\D #\d) 13)
    ((or #\E #\e) 14)
    ((or #\F #\f) 15)
    (_ (json-exception port))))

;; Characters in Guile match the JSON representation so we just need to parse
;; the hexadecimal values into an integer.
;;
;;     codepoint = 16^3 * ch + 16^2 * ch + 16 * ch + ch
;;
;; https://www.gnu.org/software/guile/manual/html_node/Characters.html
(define (read-unicode-char port)
  (integer->char
   (+ (* 4096 (read-hex-digit->integer port))
      (* 256 (read-hex-digit->integer port))
      (* 16 (read-hex-digit->integer port))
      (read-hex-digit->integer port))))

(define (read-control-char port)
  (match (read-char port)
    (#\" #\")
    (#\\ #\\)
    (#\/ #\/)
    (#\b #\bs)
    (#\f #\ff)
    (#\n #\lf)
    (#\r #\cr)
    (#\t #\ht)
    (#\u (read-unicode-char port))
    (_ (json-exception port))))

(define (json-read-string port)
  (expect-delimiter port #\")
  (let loop ((chars '()))
    (match (read-char port)
      ((? eof-object?) (json-exception port))
      (#\" (reverse-list->string chars))
      (#\\ (loop (cons (read-control-char port) chars)))
      (ch (loop (cons ch chars))))))

;;
;; Booleans and null parsing helpers
;;

(define (json-read-true port)
  (expect-string port "true" #t))

(define (json-read-false port)
  (expect-string port "false" #f))

(define (json-read-null port)
  (expect-string port "null" #nil))

;;
;; Main parser functions
;;

(define (json-read port)
  (skip-whitespaces port)
  (match (peek-char port)
    ;; If we reach the end we might have an incomplete document.
    ((? eof-object?) (json-exception port))
    ;; Read JSON values.
    (#\t (json-read-true port))
    (#\f (json-read-false port))
    (#\n (json-read-null port))
    (#\{ (json-read-object port))
    (#\[ (json-read-array port))
    (#\" (json-read-string port))
    ;; Anything else should be a number.
    (_ (json-read-number port))))

;;
;; Public procedures
;;

(define* (json->scm #:optional (port (current-input-port)))
  "Parse a JSON document into native. Takes one optional argument,
@var{port}, which defaults to the current input port from where the JSON
document is read."
  (let ((value (json-read port)))
    ;; Skip any trailing whitespaces.
    (skip-whitespaces port)
    (cond
     ;; If we reach the end the parsing succeeded.
     ((eof-object? (peek-char port)) value)
     ;; If there's anything else other than the end, parser fails.
     (else (json-exception port)))))

(define* (json-string->scm str)
  "Parse a JSON document into native. Takes a string argument,
@var{str}, that contains the JSON document."
  (call-with-input-string str (lambda (p) (json->scm p))))

;;; (json parser) ends here
