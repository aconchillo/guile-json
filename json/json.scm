(define-module (json)
  #:use-module (ice-9 rdelim)
  #:export (json->scm))

;;
;; Number parsing helpers
;;

;; Read + or -. . If something different is found, return empty string.
(define (read-sign port)
  (let loop ((c (peek-char port)) (s ""))
    (case c
      ((#\+ #\-)
       (let ((ch (read-char port)))
         (string-append s (string ch))))
      (else s))))

;; Read digits [0..9]. If something different is found, return empty
;; string.
(define (read-digits port)
  (let loop ((c (peek-char port)) (s ""))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let ((ch (read-char port)))
         (loop (peek-char port)
               (string-append s (string ch)))))
      (else s))))

(define (read-exp-part port)
  (let ((c (peek-char port)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\ht #\vt #\lf #\cr #\sp) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ;; We might have the exponential part
      ((#\e #\E)
       (let ((ch (read-char port)) ; current char
             (sign (read-sign port))
             (digits (read-digits port)))
         ;; If we don't have sign or digits, we have an invalid
         ;; number.
         (if (not (and (string-null? sign)
                       (string-null? digits)))
             (string-append s (string ch) sign digits)
             #f)))
      ;; If we have a character different than e or E, we have an
      ;; invalid number.
      (else #f))))

(define (read-real-part port)
  (let ((c (peek-char port)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\ht #\vt #\lf #\cr #\sp) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ;; If we read . we might have a real number
      ((#\.)
       (let ((ch (read-char port))
             (digits (read-digits port)))
         ;; If we have digits, try to read the exponential part,
         ;; otherwise we have an invalid number.
         (cond
          ((not (string-null? digits))
           (let ((exp (read-exp-part port)))
             (cond
              (exp (string-append s (string ch) digits exp))
              (else #f))))
          (else #f))))
      ;; If we have a character different than . we might continue
      ;; processing.
      (else #f))))

(define (read-number port)
  (let loop ((c (peek-char port)) (s ""))
    (case c
      ;; Stop parsing if whitespace found.
      ((#\ht #\vt #\lf #\cr #\sp) s)
      ;; We might be in an array or object, so stop here too.
      ((#\, #\] #\}) s)
      ((#\-)
       (let ((ch (read-char port)))
         (loop (peek-char port)
               (string-append s (string ch)))))
      ((#\0)
       (let ((ch (read-char port)))
         (string-append s
                        (string ch)
                        (or (read-real-part port)
                            (throw 'json-invalid)))))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let ((ch (read-char port)))
         (string-append s
                        (string ch)
                        (read-digits port)
                        (or (read-real-part port)
                            (read-exp-part port)
                            (throw 'json-invalid)))))
      (else (throw 'json-invalid)))))

;;
;; Object parsing helpers
;;

(define (read-pair port)
  ;; Read string key
  (let ((key (json-read-string port)))
    (let loop ((c (peek-char port)))
      (case c
      ;; Skip whitespaces
      ((#\ht #\vt #\lf #\cr #\sp)
       (read-char port)
       (loop (peek-char port)))
      ;; Skip colon and read value
      ((#\:)
       (read-char port)
       (cons (string->symbol key) (json-read port)))
      ;; invalid object
      (else (throw 'json-invalid))))))

(define (read-object port)
  (let loop ((c (peek-char port))
             (pairs (make-hash-table)))
    (case c
      ;; Skip whitespaces
      ((#\ht #\vt #\lf #\cr #\sp)
       (read-char port)
       (loop (peek-char port) pairs))
      ;; end of object
      ((#\})
       (read-char port)
       pairs)
      ;; Read one pair and continue
      ((#\")
       (let ((pair (read-pair port)))
         (hashq-set! pairs (car pair) (cdr pair))
         (loop (peek-char port) pairs)))
      ;; Skip comma and read more pairs
      ((#\,)
       (read-char port)
       (loop (peek-char port) pairs))
      ;; invalid object
      (else (throw 'json-invalid)))))

;;
;; Array parsing helpers
;;

(define (read-array port)
  (let loop ((c (peek-char port)) (values '()))
    (case c
      ;; Skip whitespace and comma
      ((#\ht #\vt #\lf #\cr #\sp #\,)
       (read-char port)
       (loop (peek-char port) values))
      ;; end of array
      ((#\])
       (read-char port)
       values)
      ;; this can be any json object
      (else
       (let ((value (json-read port)))
         (loop (peek-char port)
               (append values (list value))))))))

;;
;; String parsing helpers
;;

(define (expect ch expected)
  (if (not (char=? ch expected))
      (throw 'json-invalid)
      ch))

(define (expect-string port expected)
  (list->string
   (map (lambda (ch) (expect ch (read-char port)))
        (string->list expected))))

(define (read-hex-digit port)
  (let ((c (read-char port)))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
        #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) c)
      (else (throw 'json-invalid)))))

(define (read-control-char port)
  (let ((c (read-char port)))
    (case c
      ((#\" #\\ #\/) (string c))
      ((#\b) (string #\bs))
      ((#\f) (string #\ff))
      ((#\n) (string #\lf))
      ((#\r) (string #\cr))
      ((#\t) (string #\ht))
      ((#\u)
       (let* ((utf1 (string (read-hex-digit port)
                            (read-hex-digit port)))
              (utf2 (string (read-hex-digit port)
                            (read-hex-digit port)))
              (vu8 (list (string->number utf1 16)
                         (string->number utf2 16)))
              (utf (u8-list->bytevector vu8)))
         (utf16->string utf)))
      (else #f))))

(define (read-string port)
  ;; Read characters until \ or " are found.
  (let loop ((result "")
             (current (read-delimited "\\\"" port 'split)))
    (case (cdr current)
      ((#\")
       (string-append result (car current)))
      ((#\\)
       (let ((ch (read-control-char port)))
         (if ch
             (loop (string-append result (car current) ch)
                   (read-delimited "\\\"" port 'split))
             (throw 'json-invalid))))
      (else
       (throw 'json-invalid)))))

;;
;; Main parser functions
;;

(define (json-read-true port)
  (expect-string port "true")
  #t)

(define (json-read-false port)
  (expect-string port "false")
  #f)

(define (json-read-null port)
  (expect-string port "null")
  #nil)

(define (json-read-number port)
  (string->number (read-number port)))

(define (json-read-object port)
  (let loop ((c (read-char port)))
    (case c
      ;; skip whitespace
      ((#\ht #\vt #\lf #\cr #\sp) (loop (peek-char port)))
      ;; read array values
      ((#\{) (read-object port))
      (else (throw 'json-invalid)))))

(define (json-read-array port)
  (let loop ((c (read-char port)))
    (case c
      ;; skip whitespace
      ((#\ht #\vt #\lf #\cr #\sp) (loop (peek-char port)))
      ;; read array values
      ((#\[) (read-array port))
      (else (throw 'json-invalid)))))

(define (json-read-string port)
  (let loop ((c (read-char port)))
    (case c
      ;; skip whitespace
      ((#\ht #\vt #\lf #\cr #\sp) (loop (peek-char port)))
      ;; read string contents
      ((#\") (read-string port))
      (else (throw 'json-invalid)))))

(define (json-read port)
  (let loop ((c (peek-char port)))
    (case c
      ;; skip whitespaces
      ((#\ht #\vt #\lf #\cr #\sp)
       (read-char port)
       (loop (peek-char port)))
      ;; read json values
      ((#\t) (json-read-true port))
      ((#\f) (json-read-false port))
      ((#\n) (json-read-null port))
      ((#\{) (json-read-object port))
      ((#\[) (json-read-array port))
      ((#\") (json-read-string port))
      ;; anything else should be a number
      (else (json-read-number port)))))

;;
;; Public procedures
;;

(define (json->scm s)
  (json-read (open-input-string s)))
