;;; (json builder) --- Guile JSON implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo at gmail dot com>
;;
;; This file is part of guile-json.
;;
;; guile-json is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (json builder)
  #:export (scm->json))

;;
;; Main builder functions
;;

(define (json-build-null)
  "null")

(define (json-build-boolean scm)
  (if scm "true" "false"))

(define (json-build-number scm)
  (number->string scm))

(define (json-build-string scm)
  (string-append "\"" scm "\""))

(define (json-build-array scm)
  (string-append "[" (string-join (map json-build scm) ", ") "]"))

(define (json-build-object scm)
  (string-append
   "{"
   (string-join (hash-map->list
                 (lambda (k v)
                   (string-append (json-build-string k)
                                  " : "
                                  (json-build v)))
                 scm) ", ")
   "}"))

(define (json-build scm)
  (cond
   ((eq? scm #nil) (json-build-null))
   ((boolean? scm) (json-build-boolean scm))
   ((number? scm) (json-build-number scm))
   ((string? scm) (json-build-string scm))
   ((list? scm) (json-build-array scm))
   ((hash-table? scm) (json-build-object scm))
   (else (throw 'json-invalid))))

;;
;; Public procedures
;;

(define* (scm->json scm)
  "Creates a JSON document from native."
  (json-build scm))

;;; (json builder) ends here
