;;; (tests test-goops) --- Guile JSON implementation.

;; Copyright (C) 2020-2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
;; Copyright (C) 2025 Iakob Davitis Dze Gogichaishvili <iakob.gogichaishvili@gmail.com>
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

;; Unit tests the JSON GOOPS mapping

;;; Code:

(define-module (tests test-goops)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-43)
  #:use-module (json)
  #:use-module (json goops)
  #:use-module (tests runner))

;; Define a comparable superclass for use with `test-equal', because classes
;; can't be compared with `equal?' by default.
(define-class <comparable> ())

(define-method (equal? (x <comparable>) (y <comparable>))
  (and (eq? (class-of x) (class-of y))
       (let* ((slots (class-slots (class-of x)))
              (slot-names (map slot-definition-name slots)))
         (every (lambda (slot)
                  (false-if-exception
                   (equal? (slot-ref x slot)
                           (slot-ref y slot))))
                slot-names))))

(test-runner-factory json:test-runner)

(test-begin "test-goops")

;; Class WITHOUT specific field names.
(define-class <account> (<comparable>)
  (id       #:init-keyword #:id       #:accessor !id)
  (username #:init-keyword #:username #:accessor !username))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\"}"
  (object->json (make <account> #:id "11111" #:username "jane")))

(define test-json-account
  "{\"id\":\"22222\",\"username\":\"john\"}")

(define test-account (json->object <account> test-json-account))

(test-equal "22222" (!id test-account))
(test-equal "john" (!username test-account))

;; Class WITH specific field names.
(define-class <account> (<comparable>)
  (id       #:init-keyword #:id       #:json-key "account_id")
  (username #:init-keyword #:username #:json-key "account_username"))

(test-equal "{\"account_id\":\"11111\",\"account_username\":\"user\"}"
  (object->json (make <account> #:id "11111" #:username "user")))

;; Class WITH conversion functions.
(define-class <account> (<comparable>)
  (id       #:init-keyword #:id #:accessor !id)
  (username #:init-keyword #:username #:accessor !username
            #:json-deserializer string-upcase
            #:json-serializer string-downcase))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\"}"
  (object->json (make <account> #:id "11111" #:username "JANE")))

(define test-json-account
  "{\"id\":\"22222\",\"username\":\"john\"}")

(define test-account (json->object <account> test-json-account))
(test-equal "22222" (!id test-account))
(test-equal "JOHN" (!username test-account))

;; Class WITH conversion functions and unbound values.
(define-class <account> (<comparable>)
  (id       #:accessor !id)
  (username #:accessor !username)
  (omitted  #:accessor !omitted)
  (boolean  #:accessor !boolean))

(define test-json-account
  "{\"id\":\"11111\",\"username\":\"jane\",\"boolean\":false}")

(define test-account (json->object <account> test-json-account))
(test-equal "11111" (!id test-account))
(test-equal "jane" (!username test-account))
(test-equal #f (slot-bound? test-account 'omitted))
(test-equal #f (!boolean test-account))

;; Check idempotence
(define test-account-idem (json->object <account> (object->json test-account)))
(test-equal "11111" (!id test-account-idem))
(test-equal "jane" (!username test-account-idem))
(test-equal #f (slot-bound? test-account-idem 'omitted))
(test-equal #f (!boolean test-account-idem))

;; Class with nested record

(define-json-mapping <link-record>
  make-link
  link?
  json->link <=> link->json <=> scm->link <=> link->scm
  (type link-type)
  (url link-url))

(define-class <account> (<comparable>)
  (id       #:init-keyword #:id       #:accessor !id)
  (username #:init-keyword #:username #:accessor !username)
  (links    #:init-keyword #:links    #:accessor !links
            #:json-serializer (lambda (v) (list->vector (map link->scm v)))
            #:json-deserializer (lambda (v) (map scm->link (vector->list v)))))

(define test-account
  (make <account> #:id "11111" #:username "jane"
        #:links (list (make-link "test" "http://guile.json"))))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"links\":[{\"type\":\"test\",\"url\":\"http://guile.json\"}]}"
  (object->json test-account))

;; Check idempotence
(test-equal (make <account> #:id "11111" #:username "jane"
                  #:links (list (make-link "test" "http://guile.json")))
  (json->object <account> (object->json test-account)))

;; Class with nested class (de)serializer

(define-class <link> (<comparable>)
  (type #:init-keyword #:type)
  (url  #:init-keyword #:url))

(define-class <account> (<comparable>)
  (id       #:init-keyword #:id       #:accessor !id)
  (username #:init-keyword #:username #:accessor !username)
  (links    #:init-keyword #:links    #:accessor !links
            #:json-serializer (lambda (v) (list->vector (map object->scm v)))
            #:json-deserializer (lambda (v) (map (lambda (i) (scm->object <link> i))
                                                 (vector->list v)))))

(define test-account
  (make <account> #:id "11111" #:username "jane"
        #:links (list (make <link> #:type "test" #:url "http://guile.json"))))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"links\":[{\"type\":\"test\",\"url\":\"http://guile.json\"}]}"
  (object->json test-account))

;; Check idempotence
(test-equal test-account
  (json->object <account> (object->json test-account)))

;; Class with nested class type

(define-class <link-type> (<comparable>)
  (type #:init-keyword #:type #:accessor !type)
  (url  #:init-keyword #:url  #:accessor !url))

(define-class <account-type> (<comparable>)
  (id       #:init-keyword #:id       #:accessor !id)
  (username #:init-keyword #:username #:accessor !username)
  (link     #:init-keyword #:link     #:accessor !link #:type <link-type>))

(define test-account-type
  (make <account-type>
    #:id "11111"
    #:username "jane"
    #:link (make <link-type> #:type "test" #:url "http://guile.json")))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"link\":{\"type\":\"test\",\"url\":\"http://guile.json\"}}"
  (object->json test-account-type))

;; Check idempotence
(test-equal test-account-type
  (json->object <account-type> (object->json test-account-type)))

;; Class with vector type

(define-class <account-type> (<comparable>)
  (id       #:init-keyword #:id       #:accessor !id)
  (username #:init-keyword #:username #:accessor !username)
  (links    #:init-keyword #:link     #:accessor !link #:type (vector <link-type>)))

(define test-account-type
  (make <account-type> #:id "11111" #:username "jane"
        #:link (vector (make <link-type> #:type "test" #:url "http://guile.json"))))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"links\":[{\"type\":\"test\",\"url\":\"http://guile.json\"}]}"
  (object->json test-account-type))

;; Check idempotence with vectors
(test-equal test-account-type
  (json->object <account-type> (object->json test-account-type)))

;; Class with list type

(define-class <account-type> (<comparable>)
  (id       #:init-keyword #:id       #:accessor !id)
  (username #:init-keyword #:username #:accessor !username)
  (links    #:init-keyword #:link     #:accessor !link #:type (list <link-type>)))

(define test-account-type
  (make <account-type> #:id "11111" #:username "jane"
        #:link (list (make <link-type> #:type "test" #:url "http://guile.json"))))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"links\":[{\"type\":\"test\",\"url\":\"http://guile.json\"}]}"
  (object->json test-account-type))

;; Check idempotence with lists
(test-equal test-account-type
  (json->object <account-type> (object->json test-account-type)))

(let ((fail-count (test-runner-fail-count (test-runner-current))))
  (test-end "test-record")
  (exit (zero? fail-count)))

;;; (tests test-record) ends here
