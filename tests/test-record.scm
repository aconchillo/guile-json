;;; (tests test-record) --- Guile JSON implementation.

;; Copyright (C) 2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
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
  #:use-module (json)
  #:use-module (tests runner))

(test-runner-factory json:test-runner)

(test-begin "test-record")

;; Record WITHOUT specific field names.
(define-json-mapping <account>
  make-account
  account?
  json->account
  account->json
  (id       account-id)
  (username account-username))

(test-equal "{\"id\":\"11111\",\"username\":\"aleix\"}"
  (account->json (make-account "11111" "aleix")))

(define test-json-account
  "{\"id\":\"22222\",\"username\":\"john\"}")
(define test-account (json->account test-json-account))

(test-equal "22222" (account-id test-account))
(test-equal "john" (account-username test-account))

;; Record WITH specific field names.
(define-json-mapping <account>
  make-account
  account?
  json->account
  account->json
  (id       account-id "account_id")
  (username account-username "account_username"))

(test-equal "{\"account_id\":\"11111\",\"account_username\":\"user\"}"
  (account->json (make-account "11111" "user")))

;; Record WITH conversion functions.
(define-json-mapping <account>
  make-account
  account?
  json->account
  account->json
  (id       account-id)
  (username account-username "username"
            (lambda (u) (string-upcase u)) ;; json->value
            (lambda (u) (string-downcase u)))) ;; value->json

(test-equal "{\"id\":\"11111\",\"username\":\"aleix\"}"
  (account->json (make-account "11111" "ALEIX")))

(define test-json-account
  "{\"id\":\"22222\",\"username\":\"john\"}")
(define test-account (json->account test-json-account))

(test-equal "22222" (account-id test-account))
(test-equal "JOHN" (account-username test-account))

(exit (if (test-end "test-record") 0 1))

;;; (tests test-record) ends here
