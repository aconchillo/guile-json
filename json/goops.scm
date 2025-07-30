;;; (json goops) --- Guile JSON implementation.

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

;; JSON module for Guile

;;; Code:

(define-module (json goops)
  #:use-module (json builder)
  #:use-module (json parser)
  #:use-module (oop goops)
  #:use-module (rnrs base)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (scm->object!
            scm->object
            json->object!
            json->object

            object->scm
            object->json

            slot-json-serializable?
            slot-json-deserializable?
            slot-json-key
            slot-json-serializer
            slot-json-deserializer
            slot-definition-type))

;;
;; Helper Macros/Procedures/Variables
;;

(define-syntax-rule (define-generic-with-docs name docs)
  "Define a generic function with documentation."
  (begin
    (define-generic name)
    (set-procedure-property! name 'documentation docs)))

(define protected-scm-types
  (list <string>
        <vector>
        <symbol>
        <boolean>
        <real>))

(define-generic type->serializer)
(define-generic type->deserializer)

;;
;; Class Introspection
;;

;; NOTE: These procedures are called `slot-{field}` and not
;; `slot-definition-{field}` because if the option doesn't exist on the slot,
;; they subtitute in a default value, which isn't how the `slot-definition-*`
;; procedures behave.
(define (slot-json-serializable? slot)
  (get-keyword #:json-serializable? (slot-definition-options slot) #t))
(define (slot-json-deserializable? slot)
  (get-keyword #:json-deserializable? (slot-definition-options slot) #t))

(define (slot-json-key slot)
  (get-keyword #:json-key (slot-definition-options slot)
               (symbol->string (slot-definition-name slot))))

(define (slot-definition-type slot)
  (get-keyword #:type (slot-definition-options slot) #f))

(define (slot-json-serializer slot)
  (or (get-keyword #:json-serializer (slot-definition-options slot) #f)
      (and=> (slot-definition-type slot)
             type->serializer)
      identity))
(define (slot-json-deserializer slot)
  (or (get-keyword #:json-deserializer (slot-definition-options slot) #f)
      (and=> (slot-definition-type slot)
             type->deserializer)
      identity))

;;
;; Document -> Object
;;

(define (scm->object! object scm)
  "Read into object @var{object} the data in the Scheme-formatted JSON document
contained in @var{scm}.

The slot definition options that control the changes made to the class object
from the JSON document are identical to what's used by @code{scm->object}."
  (for-each (lambda (slot)
              (when (slot-json-deserializable? slot)
                (let* ((slot-name (slot-definition-name slot))
                       (key (slot-json-key slot))
                       (deserialize (slot-json-deserializer slot))
                       (assoc-value (assoc key scm)))
                  (when (pair? assoc-value)
                    (slot-set! object slot-name
                               (deserialize (cdr assoc-value)))))))
            (class-slots (class-of object)))
  object)

(define-generic-with-docs scm->object
  "Create an object of @var{class} from the Scheme-formatted JSON document
contained in @var{scm}.

The following slot definition options control the creation of the class object
from the JSON document:
@itemize
@item @code{#:json-deserializable?}: Whether to insert the corresponding value
from the document into the slot.
@item @code{#:json-deserializer}: The procedure that will be applied to the value
in the document before it is inserted into the slot.
@item @code{#:json-key}: A different object field instead of the slot name in the
document that the slot value will be retrieved from.
@item @code{#:type}: A class type that the value from the document will be
deserialized into. If the value in this slot definition option is a list/vector
of the type, then the value in the document will be deserialized into a
list/vector of values of that type. If both @code{#:json-deserializer} and
@code{#:type} are present, the former will take precedence.
@end")

(define-method (scm->object (class <class>) (scm <list>))
  (scm->object! (make class) scm))

;; For the types present in a JSON document other than alists, let the value pass
;; through.
(for-each (lambda (type)
            (add-method! scm->object
                         (make <method>
                           #:specializers (list <class> type)
                           #:procedure (lambda (a b) b))))
          protected-scm-types)

;; (slot-name ... #:type <some-type>)
(define-method (type->deserializer (type <class>))
  (lambda (scm)
    (scm->object type scm)))

;; (slot-name ... #:type (vector <some-type>))
(define-method (type->deserializer (type <vector>))
  (define class (vector-ref type 0))
  (lambda (scm)
    (vector-map (lambda (element)
                  (scm->object class element))
                scm)))

;; (slot-name ... #:type (list <some-type>))
(define-method (type->deserializer (type <list>))
  (define class (car type))
  (lambda (scm)
    (vector->list
     (vector-map (lambda (element)
                   (scm->object class element))
                 scm))))

(define-generic-with-docs json->object!
  "Read into class object @var{object} the data in the JSON document provided by
@var{input}, which can be a string or a port containing a JSON document, or a
JSON document in parsed Scheme format.

The slot definition options control the changes made to the class object from
the JSON document are identical to what's used by @code{json->object}.")

(define-method (json->object! object (input <port>))
  (scm->object! object (json->scm input)))
(define-method (json->object! object (str <string>))
  (scm->object! object (json-string->scm str)))
(define-method (json->object! object (scm <list>))
  (scm->object! object scm))

(define (json->object class input)
  "Create an instance @var{class} with data from the JSON document provided by
@var{input}, which can be a string or a port containing a JSON document, or a
JSON document in parsed Scheme format.

The following slot definition options control the creation of the class object
from the JSON document:
@itemize
@item @code{#:json-deserializable?}: Whether to insert the corresponding value
from the document into the slot.
@item @code{#:json-deserializer}: The procedure that will be applied to the value
in the document before it is inserted into the slot.
@item @code{#:json-key}: A different object field instead of the slot name in the
document that the slot value will be retrieved from.
@item @code{#:type}: A class type that the value from the document will be
deserialized into. If the value in this slot definition option is a list/vector
of the type, then the value in the document will be deserialized into a
list/vector of values of that type. If both @code{#:json-deserializer} and
@code{#:type} are present, the former will take precedence.
@end"
  (json->object! (make class) input))

;;
;; Object -> Document
;;

(define-generic-with-docs object->scm
  "Create a JSON document in Scheme format from the data of object @var{object}.

The following slot definition options control the creation of the document from
the class object:
@itemize
@item @code{#:json-serializable?}: Whether to insert the slot into the document.
@item @code{#:json-serializer}: The procedure that will be applied to the value
of the slot before it is serialized and inserted into the document.
@item @code{#:json-key}: A different field in the document instead of the slot
name that the slot value will be inserted into.
@item @code{#:type}: A class type that the value in the document will be
serialized from. If the value in this slot definition option is a list/vector
of the type, then the value in the document will be serialized from a list/vector
of values of that type contained in the slot. If both @code{#:json-serializer}
and @code{#:type} are present, the former will take precedence.
@end")

(define-method (object->scm (object <object>))
  (define class (class-of object))
  (reverse!
   (fold (lambda (slot table)
           (if (and (slot-bound? object (slot-definition-name slot))
                    (slot-json-serializable? slot))
               (let* ((slot-name (slot-definition-name slot))
                      (key (slot-json-key slot))
                      (serialize (slot-json-serializer slot))
                      (serialized-value (serialize (slot-ref object slot-name))))
                 (cons `(,key . ,serialized-value)
                       table))
               table))
         '() (class-slots class))))

;; For the types present in a JSON document other than alists, let the value pass
;; through.
(for-each (lambda (type)
            (add-method! object->scm
                         (make <method>
                           #:specializers (list type)
                           #:procedure identity)))
          protected-scm-types)

;; (slot-name ... #:type <some-type>)
(define-method (type->serializer (type <class>))
  object->scm)

;; (slot-name ... #:type (vector <some-type>))
(define-method (type->serializer (type <vector>))
  (lambda (vec)
    (vector-map object->scm vec)))

;; (slot-name ... #:type (list <some-type>))
(define-method (type->serializer (type <list>))
  (lambda (vec)
    (list->vector
     (map object->scm vec))))

(define* (object->json object #:optional (port #f))
  "Create a JSON document from the data of object @var{object}. Takes one
optional argument @var{port}, which defaults to @code{#f} meaning that the
document will be returned as a string. If a port is supplied instead, the
document will be written to that port.

The following slot definition options control the serialization of the JSON
document from the class object:
@itemize
@item @code{#:json-serializable?}: Whether to insert the slot into the document.
@item @code{#:json-serializer}: The procedure that will be applied to the value
of the slot before it is serialized and inserted into the document.
@item @code{#:json-key}: A different field in the document instead of the slot
name that the slot value will be inserted into.
@item @code{#:type}: A class type that the value in the document will be
serialized from. If the value in this slot definition option is a list/vector
of the type, then the value in the document will be serialized from a list/vector
of values of that type contained in the slot. If both @code{#:json-serializer}
and @code{#:type} are present, the former will take precedence.
@end"
  (define scm (object->scm object))
  (if port
      (scm->json scm port)
      (scm->json-string scm)))

;;; (json goops) ends here
