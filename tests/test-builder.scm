;;; (tests test-builder) --- Guile JSON implementation.

;; Copyright (C) 2018-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; Unit tests the JSON builder

;;; Code:

(define-module (tests test-builder)
  #:use-module (json)
  #:use-module (srfi srfi-64)
  #:use-module (tests runner))

(test-runner-factory json:test-runner)

(test-begin "test-builder")

;; Numbers
(test-equal "1234" (scm->json-string 1234))
(test-equal "-1234" (scm->json-string -1234))
(test-equal "-54.897" (scm->json-string -54.897))
(test-equal "1000.0" (scm->json-string 1e3))
(test-equal "0.001" (scm->json-string 1e-3))
(test-equal "0.5" (scm->json-string 1/2))
(test-equal "0.75" (scm->json-string 3/4))
(test-error #t (scm->json-string 1+2i))
(test-error #t (scm->json-string +inf.0))
(test-error #t (scm->json-string -inf.0))
(test-error #t (scm->json-string +nan.0))

;; Strings
(test-equal "\"hello guile!\"" (scm->json-string "hello guile!"))
(test-equal "\"你好 guile!\"" (scm->json-string "你好 guile!"))
(test-equal "\"\\u4f60\\u597d guile!\"" (scm->json-string "你好 guile!" #:unicode #t))
(test-equal "\"guile powers music \\ud834\\udd1e!\"" (scm->json-string "guile powers music 𝄞!" #:unicode #t))
(test-equal "\"</script>\"" (scm->json-string "</script>"))
(test-equal "\"<\\/script>\"" (scm->json-string "</script>" #:solidus #t))

;; Strings (special characters)
(test-equal "\"quotation mark:\\\"\"" (scm->json-string "quotation mark:\""))
(test-equal "\"reverse solidus:\\\\\"" (scm->json-string "reverse solidus:\\"))
(test-equal "\"solidus:/\"" (scm->json-string "solidus:/"))
(test-equal "\"solidus:\\/\"" (scm->json-string "solidus:/" #:solidus #t))
(test-equal "\"backspace:\\b\"" (scm->json-string "backspace:"))
(test-equal "\"form feed:\\f\"" (scm->json-string "form feed:"))
(test-equal "\"line feed:\\n\"" (scm->json-string "line feed:
"))
(test-equal "\"carriage return:\\r\"" (scm->json-string "carriage return:\r"))
(test-equal "\"horizontal tab:\\t\"" (scm->json-string "horizontal tab:	"))

;; Boolean
(test-equal "true" (scm->json-string #t))
(test-equal "false" (scm->json-string #f))

;; Null
(test-equal "null" (scm->json-string 'null))
(test-equal "null" (scm->json-string #nil #:null #nil))

;; Arrays
(test-equal "[]" (scm->json-string #()))
(test-equal "[1,2,3,4]" (scm->json-string #(1 2 3 4)))
(test-equal "[1,2,[3,4],[5,6,[7,8]]]" (scm->json-string #(1 2 #(3 4) #(5 6 #(7 8)))))
(test-equal "[1,\"two\",3,\"four\"]" (scm->json-string #(1 "two" 3 "four")))

;; Arrays (pretty)
(test-equal "[]" (scm->json-string #() #:pretty #t))
(test-equal "[\n  1,\n  2,\n  3,\n  4\n]" (scm->json-string #(1 2 3 4) #:pretty #t))
(test-equal "[\n  1,\n  2,\n  [\n    3,\n    4\n  ],\n  [\n    5,\n    6,\n    [\n      7,\n      8\n    ]\n  ]\n]" (scm->json-string #(1 2 #(3 4) #(5 6 #(7 8))) #:pretty #t))
(test-equal "[\n  1,\n  \"two\",\n  3,\n  \"four\"\n]" (scm->json-string #(1 "two" 3 "four") #:pretty #t))

;; Objects
(test-equal "{\"foo\":\"bar\"}" (scm->json-string '((foo . bar))))
(test-equal "{\"foo\":\"bar\"}" (scm->json-string '(("foo" . "bar"))))
(test-equal "{\"foo\":[1,2,3]}" (scm->json-string '((foo . #(1 2 3)))))
(test-equal "{\"foo\":{\"bar\":[1,2,3]}}" (scm->json-string '((foo . ((bar . #(1 2 3)))))))
(test-equal "{\"foo\":[1,{\"two\":\"three\"}]}" (scm->json-string '((foo . #(1 (("two" . "three")))))))
(test-equal "{\"title\":\"A book\",\"author\":\"An author\",\"price\":29.99}"
  (scm->json-string '((title . "A book")
                      (author . "An author")
                      (price . 29.99))))
;; Empty objects
(test-equal "{}" (scm->json-string '()))
(test-equal "{\"top-level\":{\"second-level\":{}}}"
  (scm->json-string '(("top-level" ("second-level")))))

;; Objects (pretty)
(test-equal "{\n  \"foo\": \"bar\"\n}" (scm->json-string '((foo . bar)) #:pretty #t))
(test-equal "{\n  \"foo\": \"bar\"\n}" (scm->json-string '(("foo" . "bar")) #:pretty #t))
(test-equal "{\n  \"foo\": [\n    1,\n    2,\n    3\n  ]\n}" (scm->json-string '((foo . #(1 2 3))) #:pretty #t))
(test-equal "{\n  \"foo\": {\n    \"bar\": [\n      1,\n      2,\n      3\n    ]\n  }\n}" (scm->json-string '((foo . ((bar . #(1 2 3))))) #:pretty #t))
(test-equal "{\n  \"foo\": [\n    1,\n    {\n      \"two\": \"three\"\n    }\n  ]\n}" (scm->json-string '((foo . #(1 (("two" . "three"))))) #:pretty #t))
(test-equal "{\n  \"title\": \"A book\",\n  \"author\": \"An author\",\n  \"price\": 29.99\n}"
  (scm->json-string '((title . "A book")
                      (author . "An author")
                      (price . 29.99))
                    #:pretty #t))
;; Empty objects (pretty)
(test-equal "{}" (scm->json-string '() #:pretty #t))
(test-equal "{\n  \"top-level\": {\n    \"second-level\": {}\n  }\n}"
  (scm->json-string '(("top-level" ("second-level"))) #:pretty #t))

;; Invalid objects
(test-error #t (scm->json (vector 1 2 3 #u8(1 2 3))))
(test-error #t (scm->json #u8(1 2 3)))
(test-error #t (scm->json #(1 +inf.0 3)))
(test-error #t (scm->json '((foo . +nan.0))))

;; Sequences
(test-equal "\x1e[]\n" (scm->json-seq-string '(#())))
(test-equal "\x1enull\n" (scm->json-seq-string '(null)))
(test-equal "\x1enull\n" (scm->json-seq-string '(ball) #:null 'ball))
(test-equal "\x1e1\n\x1e2\n\x1e3\n\x1e[1,2,3]\n" (scm->json-seq-string '(1 2 3 #(1 2 3))))
(test-equal "\x1e{\"foo\":{\"bar\":{\"baz\":true}}}\n" (scm->json-seq-string '(((foo . (("bar" . ((baz . #t)))))))))

(test-equal "\x1e\"\\u001e\"\n" (scm->json-seq-string (list "\x1e")))

(test-error #t (scm->json-seq (list (vector 1 2 3 #u8(1 2 3)))))
(test-error #t (scm->json-seq '(#u8(1 2 3))))
(test-error #t (scm->json-seq '(#(1 +inf.0 3))))
(test-error #t (scm->json-seq '(((foo . +nan.0)))))

(let ((fail-count (test-runner-fail-count (test-runner-current))))
  (test-end "test-builder")
  (exit (zero? fail-count)))

;;; (tests test-builder) ends here
