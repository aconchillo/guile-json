
# guile-json

[![GNU Guile 2.2](https://github.com/aconchillo/guile-json/actions/workflows/guile2.2.yml/badge.svg)](https://github.com/aconchillo/guile-json/actions/workflows/guile2.2.yml)
[![GNU Guile 3.0](https://github.com/aconchillo/guile-json/actions/workflows/guile3.0.yml/badge.svg)](https://github.com/aconchillo/guile-json/actions/workflows/guile3.0.yml)

guile-json is a JSON module for Guile. It supports parsing and building JSON
documents according to the http://json.org specification.

- Complies with http://json.org specification.

- Supports JSON Text Sequences (RFC 7464).

- Supports parsing concatenated JSON documents.

- Builds JSON documents programmatically using scheme data types.

- Allows JSON pretty printing.


# Installation

Download the latest tarball and untar it:

- [guile-json-4.7.3.tar.gz](https://download.savannah.gnu.org/releases/guile-json/guile-json-4.7.3.tar.gz)

If you are cloning the repository make sure you run this first:

    $ autoreconf -vif

Then, run the typical sequence:

    $ ./configure --prefix=<guile-prefix>
    $ make
    $ sudo make install

Where `<guile-prefix>` should preferably be the same as your system Guile
installation directory (e.g. /usr).

If everything installed successfully you should be up and running:

    $ guile
    scheme@(guile-user)> (use-modules (json))
    scheme@(guile-user)> (scm->json #(1 2 3))
    [1,2,3]

It might be that you installed guile-json somewhere differently than your
system's Guile. If so, you need to indicate Guile where to find guile-json,
for example:

    $ GUILE_LOAD_PATH=/usr/local/share/guile/site guile


# Usage

guile-json provides a few procedures to parse and build a JSON document. A
JSON document is transformed into or from native Guile values according to the
following table:

| JSON   | Guile  |
|--------|--------|
| string | string |
| number | number |
| object | alist  |
| array  | vector |
| true   | #t     |
| false  | #f     |
| null   | 'null  |

Why are JSON arrays converted to vectors and JSON objects to alists? See this
[discussion](https://lists.gnu.org/archive/html/guile-user/2018-12/msg00039.html)
for details.

By default the value of JSON "null" is mapped to the symbol 'null. However,
all guile-json functions allow changing the default null value by specifying
the #:null keyword argument with another value. This other value needs to be
recognized by *eq?*.

To start using guile-json procedures and macros you first need to load
the module:

    scheme@(guile-user)> (use-modules (json))


## Reading JSON documents

- (**json->scm** #:optional port #:key null ordered concatenated) : Reads a JSON
  document from the given port, or from the current input port if none is given.

  Optional arguments:

  - *port* : is optional, it defaults to the current input port.

  Keyword arguments:

  - *null* : value for JSON's null, it defaults to the 'null symbol.

  - *ordered* : indicate whether JSON objects order should be preserved or not
    (the default).

  - *concatenated* : if true it tells the parser that more JSON documents might
    be present after a properly parsed document, otherwise the parser will fail
    if additional data is present after the first document (this is the
    default).

- (**json-string->scm** str #:key null ordered) : Reads a JSON document from the
  given string.

  Keyword arguments:

  - *null* : value for JSON's null, it defaults to the 'null symbol.

  - *ordered* : indicate whether JSON objects order should be preserved or not
    (the default).


## Building JSON documents

- (**scm->json** native #:optional port #:key solidus unicode null validate
  pretty) : Creates a JSON document from the given native Guile value. The JSON
  document is written into the given port, or to the current output port if non
  is given.

  Optional arguments:

  - *port* : it defaults to the current output port.

  Keyword arguments:

  - *solidus* : if true, the slash (/ solidus) character will be escaped
    (defaults to false).

  - *unicode* : if true, additional to control characters, non-ASCII
    characters will be escaped as well (defaults to false).

  - *null* : value for JSON's null (defaults to the 'null symbol).

  - *validate* : if true, the native value will be validated before starting
    to print the JSON document (defaults to true).

  - *pretty* : if true, the JSON document will be pretty printed (defaults to
    false).

- (**scm->json-string** native #:key solidus unicode null validate pretty) :
  Creates a JSON document from the given native Guile value into a string.

  See keyword arguments for *scm->json*.

  Note that when using alists to build JSON objects, symbols or numbers might
  be used as keys and they both will be converted to strings.


## Reading JSON Text Sequences

- (**json-seq->scm** #:optional port #:key null ordered handle-truncate
  truncated-object) : Reads a stream of JSON documents from the given port, or
  from the current input port if none is given.

  Optional arguments:

  - *port* : is optional, it defaults to the current input port.

  Keyword arguments:

  - *null* : value for JSON's null, it defaults to the 'null symbol.

  - *ordered* : indicate whether JSON objects order should be preserved or not
    (the default).

  - *handle-truncate* : defines how to handle data loss. Possible values:

    - *'throw*: throw an exception.
    - *'stop*: stop parsing and end the stream.
    - *'skip*: skip corrupted fragment and return next object (default).
    - *'replace*: skip corrupted fragment and return object specific by
      *truncated-object*.

  - *truncated-object* : use this object if an object could not be parsed (to be
    used when setting *handle-truncate* to *'replace* value).

- (**json-seq-string->scm** str #:key null ordered handle-truncate
  truncated-object) : Reads a stream of JSON documents from the given string.

  See keyword arguments for *json-seq->scm*.


## Building JSON Text Sequences

- (**scm->json-seq** objects #:optional port #:key solidus null validate) :
  Creates a JSON document sequence from the given list of native Guile
  objects. The JSON document sequence is written into the given port, or to the
  current output port if non is given.

  Optional arguments:

  - *port* : it defaults to the current output port.

  Keyword arguments:

  - *solidus* : if true, the slash (/ solidus) character will be escaped
    (defaults to false).

  - *null* : value for JSON's null (defaults to the 'null symbol).

  - *validate* : if true, the native value will be validated before starting
    to print the JSON document (defaults to true).

- (**scm->json-seq-string** objects #:key solidus null validate) : Creates a
  JSON document sequence from the given list of native Guile objects into a
  string.

  See keyword arguments for *scm->json-seq*.


## Exceptions

A *json-invalid* exception is thrown if an error is found during the JSON
parsing with a single port argument. The line or column where the error
occured can be easily obtained from the port by calling *port-line* or
*port-column*.

When building a JSON document from a native type a *json-invalid* exception
might be thrown with the offending value as an argument (see table above for
supported types).


## JSON Objects and Records

guile-json 4.5.0 introduces JSON types, a new feature that allows converting
JSON objects into record types and vice versa in a very straight forward
way. This was built on top of *define-json-mapping* which was introduced in
version 4.2.0.

Let's take a look at an example. Imagine we have the following user account
information:

```
{
  "id": 1234,
  "username": "jane"
}
```

We can easily create a
[record](https://www.gnu.org/software/guile/manual/html_node/Records.html)
representing that data with *define-json-type* by simply doing:

```
> (define-json-type <account>
    (id)
    (username))
```

This will define the record constructor, the predicate and conversion procedures
like *json->account* or *account->json* (see *define-json-type* for more
details).

We can now create a new account and check its contents as with regular records:

```
> (define account (make-account "1234" "jane"))
> (account-id account)
"1234"
> (account-username account)
"jane"
```

Or we can use the auto-generated *scm->account* to create the account:

```
> (define account (scm->account '(("id" . "1234") ("username" . "jane"))))
```

It is also possible to convert the record to a JSON string:

```
> (account->json account)
"{\"id\":\"1234\",\"username\":\"jane\"}"
```

Or from a JSON string to a new record:

```
> (define json-account "{\"id\":\"1234\",\"username\":\"jane\"}")
> (json->account json-account)
#<<account> id: "1234" username: "jane">
```

We could also create a list of accounts:

```
> (define-json-type <accounts-list>
    (accounts "accounts" #(<account)))
```

In which case we would do:

```
> (scm->accounts-list '(("accounts" . #((("id" . "1234") ("username" . "jane"))
                                        (("id" . "4321") ("username" . "joe"))))))
#<<accounts-list> accounts: (#<<account> id: "1234" username: "jane"> #<<account> id: "4321" username: "joe">)>
```

Note how the `accounts` field is stored as a list inside the record field, this
is to simplify creating records and working with field getters. For example, to
create the same but directly using records we would use:

```
> (make-accounts-list (list (make-account "1234" "jane") (make-account "4321" "joe")))
```


### Macros

- (**define-json-type** rtd (field key type) ...) : Define a new mapping between
  a JSON object and a record type. This will automatically define the record
  constructor, the predicate, all field getters and record to/from JSON
  conversions. For more control use *define-json-mapping*.

  - *rtd* : the name of the record type.

  - *((field key type) ...)* : a series of field specifications.

    - *field* : the name of a JSON object field.

    - *key* : a different name for the field of this JSON object. If given, this
      name will be used instead of the field name when serializing or
      deserializing.

    - *type* : indicates that this field contains a record type. It is also
      possible to indicate that the field contains an array of objects of the
      same record type by using the vector syntax *#(type)*. However, note that
      in order to simplify things a list will be stored in the record field (see
      examples).

- (**define-json-mapping** rtd ctor pred json->record [<=> record->json [<=>
  scm->record]] (field getter key ...) ...) : Define a new mapping between a
  JSON object and a record type, à la SRFI-9.

  - *rtd* : the name of the record type.

  - *ctor* : the name for the record constructor procedure.

  - *pred* : a predicate procedure to check if a given argument holds a record
    of this type.

  - *json->record* : the name of the procedure to convert a JSON object, read
    from a port, string, or alist, into a record of this type.

  - *<=> record->json* : optional name of the procedure to convert a record of
    this type to a JSON string.

  - *<=> scm->record* : optional name of the procedure to convert a JSON object,
    represented as an alist, into a record of this type. This is equivalent to
    *json->record* when an alist is passed.

  - *<=> record->scm* : optional name of the procedure to convert a record of
    this type to a JSON objected represented as an alist.

  - *((field getter ...) ...)* : a series of field specifications.

    - *field* : the name of a JSON object field.

    - *getter* : the name of the procedure to get the value of this field
      given a record of this type.

    - *key* : a different name for the field of this JSON object. If given, this
      name will be used instead of the field name when serializing or
      deserializing.

    - *scm->value* : an optional procedure that will be used to convert native
      values supported by guile-json to the value contained in the record. Used
      when reading JSON.

    - *value->scm* : an optional procedure that will be used to convert the
      value contained in the record to a native value supported by guile-json
      Used when writing JSON.

### Records and null fields

When serializing a record to a JSON object it is possible to set a field to the
\*unspecified\* value in order to omit it from serialization. Also, when
deserializing a JSON object to a record, missing record fields in the JSON
object will be set to \*unspecified\* in the record.


# Examples

- Build the string "hello world":

```
> (scm->json "hello world")
"hello world"
```

- Build the [1, 2, 3] array:

```
> (scm->json #(1 2 3))
[1,2,3]
```

- Build the object { "project" : "foo", "author" : "bar" } using an alist:

```
> (scm->json '(("project" . "foo") ("author" . "bar")))
{"project":"foo","author":"bar"}
```

- Build the same object but this time using symbols:

```
> (scm->json '((project . foo) ("author" . "bar")))
{"project":"foo","author":"bar"}
```

- Build the object { "values" : [ 234, 98.56 ] }:

```
> (scm->json '(("values" . #(234 98.56))))
{"values":[234,98.56]}
```

- Build the object { "values" : [ 234, 98.56 ] } again, this time using
  a variable:

```
> (define values #(234 98.56))
> (scm->json `(("values" . ,values)))
{"values":[234,98.56]}
```

- Default null value is the 'null symbol:

```
> (scm->json 'null)
null
```

- The default null value can be changed to something else:

```
> (scm->json #nil #:null #nil)
null
```

# License

Copyright (C) 2013-2022 Aleix Conchillo Flaque <aconchillo@gmail.com>

guile-json is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

guile-json is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with guile-json. If not, see https://www.gnu.org/licenses/.
