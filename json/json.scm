(define-module (json)
  #:use-module (ice-9 rdelim)
  #:export (json->scm))

(define (expect ch expected)
  (if (not (char=? ch expected))
    (error 'read "expected: ~v, got: ~a" expected ch))
  ch)

(define (expect-string port expected)
  (list->string (map (lambda (ch) (expect ch (read-char port)))
                     (string->list expected))))

;;
;; Number parsing helpers
;;

;; Read + or -. . If something different is found, return empty string.
(define (read-sign port)
  (let loop ((c (peek-char port)) (s ""))
    (case c
      ((#\+ #\-)
       (let ((ch (read-char port))) ; current char
         (string-append s (string ch))))
      (else s))))

;; Read digits [0..9]. If something different is found, return empty
;; string.
(define (read-digits port)
  (let loop ((c (peek-char port)) (s ""))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let ((ch (read-char port))) ; current char
         (loop (peek-char port)     ; next char
               (string-append s (string ch)))))
      (else s))))

(define (read-exp-part port)
  (let loop ((c (peek-char port)) (s ""))
    ;; Return string when reaching enf of string.
    (if (eof-object? c)
        s
        (case c
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
          (else #f)))))

(define (read-real-part port)
  (let loop ((c (peek-char port)) (s ""))
    ;; Return string when reaching enf of string.
    (if (eof-object? c)
        s
        (case c
          ((#\.)
           (let ((ch (read-char port)) ; current char
                 (digits (read-digits port)))
             ;; If we have digits read the exponential part, otherwise
             ;; we have an invalid number.
             (if (not (string-null? digits))
                 (let ((exp (read-exp-part port)))
                   (if exp
                       (string-append s (string ch) digits exp)
                       #f))
                 #f)))
          ;; If we have a character different than ., we might continue
          ;; processing.
          (else #f)))))

(define (read-number port)
  (let loop ((c (peek-char port)) (s ""))
    (case c
      ((#\-)
       (let ((ch (read-char port))) ; current char
         (loop (peek-char port)     ; next char
               (string-append s (string ch)))))
      ((#\0)
       (let ((ch (read-char port)))
         (string-append s
                        (string ch)
                        (or (read-real-part port)
                            (throw 'json-invalid-number)))))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let ((ch (read-char port))) ; current char
         (string-append s
                        (string ch)
                        (read-digits port)
                        (or (read-real-part port)
                            (read-exp-part port)
                            (throw 'json-invalid-number)))))
      (else (throw 'json-invalid-number)))))

;;
;; String parsing helpers
;;

(define (read-hex-digit port)
  (let ((c (read-char port)))
    (case c
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
        #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f) c)
      (else (throw 'json-invalid-hex-digit)))))

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

(define (json-read-array port)
  (let loop ((values '()))
    (case (peek-char port)
      ;; skip comma and continue
      ((#\,)
       (read-char port)
       (loop values))
      ;; end of array
      ((#\]) values)
      ;; this can be any json object
      (else (loop (append values (list (json-read port))))))))

(define (json-read-string port)
  ;; Read string until \ or " are found.
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
             (throw 'json-invalid-string))))
      (else
       (throw 'json-invalid-string)))))

(define (json-read port)
  (let skipwhitespace ((c (peek-char port)))
    (case c
      ((#\t) (json-read-true port))
      ((#\f) (json-read-false port))
      ((#\n) (json-read-null port))
      ;; skip bracket and read array
      ((#\[)
       (read-char port)
       (json-read-array port))
      ;; skip double quote and read string
      ((#\")
       (read-char port)
       (json-read-string port))
      ;; anything else must be a number
      (else (json-read-number port)))))

(define (json->scm s)
  (json-read (open-input-string s)))
