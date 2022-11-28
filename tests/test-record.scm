;;; (tests test-record) --- Guile JSON implementation.

;; Copyright (C) 2020-2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; Unit tests the JSON record mapping

;;; Code:

(define-module (tests test-record)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-43)
  #:use-module (json)
  #:use-module (tests runner))

(test-runner-factory json:test-runner)

(test-begin "test-record")

;; Record (only JSON->RECORD).
(define-json-mapping <account>
  make-account
  account?
  json->account
  (id       account-id)
  (username account-username))

(define test-json-account
  "{\"id\":\"11111\",\"username\":\"jane\"}")

(define test-account (json->account test-json-account))
(test-equal "11111" (account-id test-account))
(test-equal "jane" (account-username test-account))

;; Record WITHOUT specific field names.
(define-json-mapping <account>
  make-account
  account?
  json->account <=> account->json
  (id       account-id)
  (username account-username))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\"}"
  (account->json (make-account "11111" "jane")))

(define test-json-account
  "{\"id\":\"22222\",\"username\":\"john\"}")

(define test-account (json->account test-json-account))
(test-equal "22222" (account-id test-account))
(test-equal "john" (account-username test-account))

;; Record WITH specific field names.
(define-json-mapping <account>
  make-account
  account?
  json->account <=> account->json
  (id       account-id "account_id")
  (username account-username "account_username"))

(test-equal "{\"account_id\":\"11111\",\"account_username\":\"user\"}"
  (account->json (make-account "11111" "user")))

;; Record WITH conversion functions.
(define-json-mapping <account>
  make-account
  account?
  json->account <=> account->json
  (id       account-id)
  (username account-username "username"
            (lambda (u) (string-upcase u)) ;; json->value
            (lambda (u) (string-downcase u)))) ;; value->json

(test-equal "{\"id\":\"11111\",\"username\":\"jane\"}"
  (account->json (make-account "11111" "JANE")))

(define test-json-account
  "{\"id\":\"22222\",\"username\":\"john\"}")

(define test-account (json->account test-json-account))
(test-equal "22222" (account-id test-account))
(test-equal "JOHN" (account-username test-account))

;; Record WITH conversion functions and *unspecified* values.
(define-json-mapping <account>
  make-account
  account?
  json->account <=> account->json
  (id       account-id)
  (username account-username)
  (omitted  account-omitted)
  (boolean  account-boolean))

(define test-json-account
  "{\"id\":\"11111\",\"username\":\"jane\",\"boolean\":false}")

(define test-account (json->account test-json-account))
(test-equal "11111" (account-id test-account))
(test-equal "jane" (account-username test-account))
(test-equal *unspecified* (account-omitted test-account))
(test-equal #f (account-boolean test-account))

;; Check idempotence
(define test-account-idem (json->account (account->json test-account)))
(test-equal "11111" (account-id test-account-idem))
(test-equal "jane" (account-username test-account-idem))
(test-equal *unspecified* (account-omitted test-account-idem))
(test-equal #f (account-boolean test-account-idem))

;; Nested records

(define-json-mapping <link>
  make-link
  link?
  json->link <=> link->json <=> scm->link <=> link->scm
  (type link-type)
  (url link-url))

(define-json-mapping <account>
  make-account
  account?
  json->account <=> account->json
  (id       account-id)
  (username account-username)
  (links    account-links "links"
            (lambda (v) (map scm->link (vector->list v)))
            (lambda (v) (list->vector (map link->scm v)))))

(define test-account
  (make-account "11111" "jane" (list (make-link "test" "http://guile.json"))))
(test-equal "11111" (account-id test-account))
(test-equal "jane" (account-username test-account))
(test-equal (make-link "test" "http://guile.json") (car (account-links test-account)))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"links\":[{\"type\":\"test\",\"url\":\"http://guile.json\"}]}"
  (account->json test-account))

;; Check idempotence
(test-equal (make-account "11111" "jane" (list (make-link "test" "http://guile.json")))
  (json->account (account->json test-account)))

;; Check JSON types

(define-json-type <account-type>
  (id "id")
  (username))

(define test-json-account-type
  "{\"id\":\"11111\",\"username\":\"jane\"}")

(define test-account-type (json->account-type test-json-account-type))
(test-equal "11111" (account-type-id test-account-type))
(test-equal "jane" (account-type-username test-account-type))

;; Check JSON types with nested objects.

(define-json-type <link-type>
  (type)
  (url))

(define-json-type <account-type>
  (id)
  (username)
  (link "link" <link-type>))

(define test-account-type
  (make-account-type "11111" "jane" (make-link-type "test" "http://guile.json")))
(test-equal "11111" (account-type-id test-account-type))
(test-equal "jane" (account-type-username test-account-type))
(test-equal (make-link-type "test" "http://guile.json")
  (account-type-link test-account-type))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"link\":{\"type\":\"test\",\"url\":\"http://guile.json\"}}"
  (account-type->json test-account-type))

;; Check idempotence
(test-equal (make-account-type "11111" "jane" (make-link-type "test" "http://guile.json"))
  (json->account-type (account-type->json test-account-type)))

;; Check JSON types with optional nested objects.

(define-json-type <omitted-type>
  (name))

(define-json-type <account-type>
  (id)
  (username)
  (omitted "omitted" <omitted-type>))

(define test-json-account
  "{\"id\":\"11111\",\"username\":\"jane\"}")

(define test-account-type (json->account-type test-json-account))

(test-equal "11111" (account-type-id test-account-type))
(test-equal "jane" (account-type-username test-account-type))
(test-equal *unspecified* (account-type-omitted test-account-type))

(test-equal (make-account-type "11111" "jane" *unspecified*)
  (json->account-type (account-type->json test-account-type)))

(test-equal (make-account-type "11111" "jane" *unspecified*)
  (scm->account-type (account-type->scm test-account-type)))

;; Check JSON types with vectors.

(define-json-type <account-type>
  (id)
  (username)
  (links "links" #(<link-type>)))

(define test-account-type
  (make-account-type "11111" "jane" (list (make-link-type "test" "http://guile.json"))))
(test-equal "11111" (account-type-id test-account-type))
(test-equal "jane" (account-type-username test-account-type))
(test-equal (make-link-type "test" "http://guile.json")
  (car (account-type-links test-account-type)))

(test-equal "{\"id\":\"11111\",\"username\":\"jane\",\"links\":[{\"type\":\"test\",\"url\":\"http://guile.json\"}]}"
  (account-type->json test-account-type))

;; Check idempotence with vectors
(test-equal (make-account-type "11111" "jane" (list (make-link-type "test" "http://guile.json")))
  (json->account-type (account-type->json test-account-type)))

(let ((fail-count (test-runner-fail-count (test-runner-current))))
  (test-end "test-record")
  (exit (zero? fail-count)))

;;; (tests test-record) ends here
