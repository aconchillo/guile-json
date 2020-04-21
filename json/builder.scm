;;; (json builder) --- Guile JSON implementation.

;; Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
;; Copyright (C) 2015,2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (json builder)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:export (scm->json
            scm->json-string))

;;
;; String builder helpers
;;

(define (unicode->string unicode)
  (format #f "\\u~4,'0x" unicode))

(define (char->unicode-string c)
  (let ((unicode (char->integer c)))
    (if (< unicode 32)
        (unicode->string unicode)
        (string c))))

(define (u8v-2->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1)))
    (+ (ash (logand bv0 #b00011111) 6)
       (logand bv1 #b00111111))))

(define (u8v-3->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1))
        (bv2 (bytevector-u8-ref bv 2)))
    (+ (ash (logand bv0 #b00001111) 12)
       (ash (logand bv1 #b00111111) 6)
       (logand bv2 #b00111111))))

(define (build-char-string c)
  (let* ((bv (string->utf8 (string c)))
         (len (bytevector-length bv)))
    (cond
     ;; A single byte UTF-8
     ((eq? len 1) (char->unicode-string c))
     ;; If we have a 2 or 3 byte UTF-8 we need to output it as \uHHHH
     ((or (eq? len 2) (eq? len 3))
      (let ((unicode (if (eq? len 2)
                         (u8v-2->unicode bv)
                         (u8v-3->unicode bv))))
        (unicode->string unicode)))
     ;; A 4 byte UTF-8 needs to output as \uHHHH\uHHHH
     ((eq? len 4)
      (let ((bv4 (string->utf16 (string c))))
        (string-append
         (unicode->string (+ (ash (bytevector-u8-ref bv4 0) 8)
                             (bytevector-u8-ref bv4 1)))
         (unicode->string (+ (ash (bytevector-u8-ref bv4 2) 8)
                             (bytevector-u8-ref bv4 3))))))
     ;; Anything else should wrong, hopefully.
     (else (throw 'json-invalid (string c))))))

;;
;; Object builder functions
;;

(define (build-object-pair p port escape unicode pretty level)
  (put-string port (indent-string pretty level))
  (json-build-string (car p) port escape unicode)
  (build-space port pretty)
  (put-string port ":")
  (build-space port pretty)
  (json-build (cdr p) port escape unicode pretty level))

(define (build-newline port pretty)
  (cond (pretty (newline port))))

(define (build-space port pretty)
  (cond (pretty (put-string port " "))))

(define (indent-string pretty level)
  (if pretty (format #f "~v_" (* 2 level)) ""))

;;
;; Main builder functions
;;

(define (json-build-null port)
  (put-string port "null"))

(define (json-build-boolean scm port)
  (put-string port (if scm "true" "false")))

(define (json-build-number scm port)
  (if (and (rational? scm) (not (integer? scm)))
      (put-string port (number->string (exact->inexact scm)))
      (put-string port (number->string scm))))

(define (->string x)
  (cond ((char? x) (make-string 1 x))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        (else x)))

(define (json-build-string scm port escape unicode)
  (put-string port "\"")
  (put-string
   port
   (list->string
    (fold-right append '()
                (map
                 (lambda (c)
                   (case c
                     ((#\" #\\) `(#\\ ,c))
                     ((#\bs) '(#\\ #\b))
                     ((#\ff) '(#\\ #\f))
                     ((#\lf) '(#\\ #\n))
                     ((#\cr) '(#\\ #\r))
                     ((#\ht) '(#\\ #\t))
                     ((#\/) (if escape `(#\\ ,c) (list c)))
                     (else (if unicode (string->list (build-char-string c)) (list c)))))
                 (string->list (->string scm))))))
  (put-string port "\""))

(define (json-build-array scm port escape unicode pretty level)
  (put-string port "[")
  (unless (null? scm)
    (vector-for-each (lambda (i v)
                       (if (> i 0) (put-string port ","))
                       (build-space port pretty)
                       (json-build v port escape unicode pretty (+ level 1)))
                     scm))
  (put-string port "]"))

(define (json-build-object scm port escape unicode pretty level)
  (cond ((> level 0)
         (build-newline port pretty)))
  (simple-format port "~A{" (indent-string pretty level))
  (build-newline port pretty)
  (let ((pairs scm))
    (unless (null? pairs)
      (build-object-pair (car pairs) port escape unicode pretty (+ level 1))
      (for-each (lambda (p)
                  (put-string port ",")
                  (build-newline port pretty)
                  (build-object-pair p port escape unicode pretty (+ level 1)))
                (cdr pairs))))
  (build-newline port pretty)
  (simple-format port "~A}" (indent-string pretty level)))

(define (json-number? number)
  (and (number? number) (eqv? (imag-part number) 0) (finite? number)))

(define (json-build scm port escape unicode pretty level)
  (cond
   ((eq? scm #nil) (json-build-null port))
   ((boolean? scm) (json-build-boolean scm port))
   ((json-number? scm) (json-build-number scm port))
   ((symbol? scm) (json-build-string (symbol->string scm) port escape unicode))
   ((string? scm) (json-build-string scm port escape unicode))
   ((vector? scm) (json-build-array scm port escape unicode pretty level))
   ((or (pair? scm) (null? scm))
    (json-build-object scm port escape unicode pretty level))
   (else (throw 'json-invalid scm))))

(define (json-key? scm)
  (or (symbol? scm) (string? scm)))

(define (json-valid? scm)
  (cond
   ((eq? scm #nil) #t)
   ((boolean? scm) #t)
   ((json-number? scm) #t)
   ((symbol? scm) #t)
   ((string? scm) #t)
   ((vector? scm) (vector-every json-valid? scm))
   ((pair? scm)
    (every (lambda (entry)
	     (and (pair? entry)
		  (json-key? (car entry))
		  (json-valid? (cdr entry))))
	   scm))
   ((null? scm) #t)
   (else (throw 'json-invalid scm))))

;;
;; Public procedures
;;

(define* (scm->json scm
                    #:optional (port (current-output-port))
                    #:key (escape #f) (unicode #f) (pretty #f) (validate #t))
  "Creates a JSON document from native. The argument @var{scm} contains the
native value of the JSON document. Takes one optional argument, @var{port},
which defaults to the current output port where the JSON document will be
written. It also takes a few key arguments: @var{escape}: if true, the
slash (/ solidus) character will be escaped, @{unicode} : if true, unicode
characters will be escaped when needed and @{pretty}: if true, the JSON
document will be pretty printed.

Note that when using alists to build JSON objects, symbols or numbers might be
used as keys and they both will be converted to strings.
"
  (cond
   ((and validate (json-valid? scm))
    (json-build scm port escape unicode pretty 0))
   (else
    (json-build scm port escape unicode pretty 0))))

(define* (scm->json-string scm #:key
                           (escape #f) (unicode #f)
                           (pretty #f) (validate #f))
  "Creates a JSON document from native into a string. The argument @var{scm}
contains the native value of the JSON document."
  (call-with-output-string
   (lambda (p)
     (scm->json scm p
                #:escape escape #:unicode unicode
                #:pretty pretty #:validate validate))))

;;; (json builder) ends here
