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
  #:use-module (ice-9 textual-ports)
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
  (let ((ch (peek-char port)))
    (cond
     ((whitespace? ch)
      (read-char port)
      (skip-whitespaces port))
     (else *unspecified*))))

(define (expect-string port expected return)
  (let loop ((n 0))
    (cond
     ;; All characters match.
     ((= n (string-length expected)) return)
     ;; Go to next characters.
     ((eqv? (read-char port) (string-ref expected n))
      (loop (+ n 1)))
     ;; Anything else is an error.
     (else (json-exception port)))))

(define (expect-delimiter port delimiter)
  (let ((ch (read-char port)))
    (cond
     ((not (eqv? ch delimiter)) (json-exception port))
     ;; Unexpected EOF.
     ((eof-object? ch) (json-exception port)))))

;;
;; Number parsing helpers
;;

;; Read + or -. If something different is found, return empty string.
(define (read-sign port string-port)
  (let ((ch (peek-char port)))
    (cond
     ((or (eqv? ch #\+) (eqv? ch #\-))
      (put-char string-port (read-char port)))
     (else *unspecified*))))

;; Read digits [0..9].
(define (read-digits port string-port)
  (let ((ch (peek-char port)))
    (cond
     ((digit? ch)
      (put-char string-port (read-char port))
      (read-digits port string-port))
     (else *unspecified*))))

(define (read-exponent port string-port)
  (let ((ch (peek-char port)))
    (cond
     ((or (eqv? ch #\e) (eqv? ch #\E))
      (read-char port)
      (put-char string-port #\e)
      (read-sign port string-port)
      (read-digits port string-port))
     (else *unspecified*))))

(define (read-fraction port string-port)
  (let ((ch (peek-char port)))
    (cond
     ((eqv? ch #\.)
      (read-char port)
      (put-char string-port #\.)
      (read-digits port string-port))
     (else *unspecified*))))

(define (read-positive-number port string-port)
  (read-digits port string-port)
  (read-fraction port string-port)
  (read-exponent port string-port))

(define (read-number port string-port)
  (let ((ch (peek-char port)))
    (cond
     ;; Negative numbers.
     ((eqv? ch #\-)
      (read-char port)
      (put-char string-port #\-)
      (read-positive-number port string-port))
     ;; Numbers starting with 0.
     ((eqv? ch #\0)
      (read-char port)
      (put-char string-port #\0)
      (read-fraction port string-port)
      (read-exponent port string-port))
     ;; Positive numbers.
     ((digit? ch)
      (read-positive-number port string-port))
     ;; Anything else is an error.
     (else (json-exception port)))))

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
    (let ((ch (peek-char port)))
      (cond
       ;; Skip colon and read value.
       ((eqv? ch #\:)
        (read-char port)
        (cons key (json-read port)))
       ;; Anything other than colon is an error.
       (else (json-exception port))))))

(define (json-read-object port)
  (expect-delimiter port #\{)
  (let loop ((pairs '()) (added #t))
    (skip-whitespaces port)
    (let ((ch (peek-char port)))
      (cond
       ;; End of object.
       ((eqv? ch #\})
        (read-char port)
        (cond
         (added pairs)
         (else (json-exception port))))
       ;; Read one pair and continue.
       ((eqv? ch #\")
        (let ((pair (read-pair port)))
          (loop (cons pair pairs) #t)))
       ;; Skip comma and read more pairs.
       ((eqv? ch #\,)
        (read-char port)
        (cond
         (added (loop pairs #f))
         (else (json-exception port))))
       ;; Invalid object.
       (else (json-exception port))))))

;;
;; Array parsing helpers
;;

(define (json-read-array port)
  (expect-delimiter port #\[)
  (let loop ((values '()) (added #t))
    (skip-whitespaces port)
    (let ((ch (peek-char port)))
      (cond
       ;; Unexpected EOF.
       ((eof-object? ch) (json-exception port))
       ;; Handle comma (make sure we added an element).
       ((eqv? ch #\,)
        (read-char port)
        (cond
         (added (loop values #f))
         (else (json-exception port))))
       ;; End of array (make sure we added an element).
       ((eqv? ch #\])
        (read-char port)
        (cond
         (added (list->vector (reverse! values)))
         (else (json-exception port))))
       ;; This can be any JSON object.
       (else
        (let ((value (json-read port)))
          (loop (cons value values) #t)))))))

;;
;; String parsing helpers
;;

(define (read-hex-digit->integer port)
  (let ((ch (read-char port)))
    (cond
     ((eqv? ch #\0) 0)
     ((eqv? ch #\1) 1)
     ((eqv? ch #\2) 2)
     ((eqv? ch #\3) 3)
     ((eqv? ch #\4) 4)
     ((eqv? ch #\5) 5)
     ((eqv? ch #\6) 6)
     ((eqv? ch #\7) 7)
     ((eqv? ch #\8) 8)
     ((eqv? ch #\9) 9)
     ((or (eqv? ch #\A) (eqv? ch #\a)) 10)
     ((or (eqv? ch #\B) (eqv? ch #\b)) 11)
     ((or (eqv? ch #\C) (eqv? ch #\c)) 12)
     ((or (eqv? ch #\D) (eqv? ch #\d)) 13)
     ((or (eqv? ch #\E) (eqv? ch #\e)) 14)
     ((or (eqv? ch #\F) (eqv? ch #\f)) 15)
     (else (json-exception port)))))

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
  (let ((ch (read-char port)))
    (cond
     ((eqv? ch #\") #\")
     ((eqv? ch #\\) #\\)
     ((eqv? ch #\/) #\/)
     ((eqv? ch #\b) #\bs)
     ((eqv? ch #\f) #\ff)
     ((eqv? ch #\n) #\lf)
     ((eqv? ch #\r) #\cr)
     ((eqv? ch #\t) #\ht)
     ((eqv? ch #\u) (read-unicode-char port))
     (else (json-exception port)))))

(define (json-read-string port)
  (expect-delimiter port #\")
  (let loop ((chars '()) (ch (read-char port)))
    (cond
     ;; Unexpected EOF.
     ((eof-object? ch) (json-exception port))
     ;; End of string.
     ((eqv? ch #\") (reverse-list->string chars))
     ;; Escaped characters.
     ((eqv? ch #\\)
      (loop (cons (read-control-char port) chars) (read-char port)))
     ;; All other characters.
     (else
      (loop (cons ch chars) (read-char port))))))

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
  (let ((ch (peek-char port)))
    (cond
     ;; Unexpected EOF.
     ((eof-object? ch) (json-exception port))
     ;; Read JSON values.
     ((eqv? ch #\t) (json-read-true port))
     ((eqv? ch #\f) (json-read-false port))
     ((eqv? ch #\n) (json-read-null port))
     ((eqv? ch #\{) (json-read-object port))
     ((eqv? ch #\[) (json-read-array port))
     ((eqv? ch #\") (json-read-string port))
     ;; Anything else should be a number.
     (else (json-read-number port)))))

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
