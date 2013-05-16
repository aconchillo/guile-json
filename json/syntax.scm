;;; (json syntax) --- Guile JSON implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-json.
;;
;; guile-json is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-json; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (json syntax)
  #:export (json))

;;
;; Public procedures
;;

(define-syntax json
  (lambda (x)
    (syntax-case x (unquote array object)
      ((_ val) (or (string? (syntax->datum #'val))
                   (number? (syntax->datum #'val))
                   (boolean? (syntax->datum #'val))
                   (null? (syntax->datum #'val)))
       #'val)

      ((_ (unquote val))
       #'val)

      ((_ (array x ...))
       #'(list (json x) ...))

      ((_ (object (k v) ...))
       #'(let ((pairs (make-hash-table)))
           (hash-set! pairs (syntax->datum #'k) (json v)) ...
           pairs)))))

;;; (json syntax) ends here
