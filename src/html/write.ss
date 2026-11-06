(define-record-type (html-value make-html-value html?)
  (nongenerative)
  (sealed #t)
  (opaque #t)
  (fields writer document?))

;; Quoting an annotation into code strips it, so the annotation is rebuilt
;; here, on the error path only.
(define (raise-value-error message value location)
  (raise
    (apply condition
      (make-assertion-violation)
      (make-who-condition 'html)
      (make-message-condition message)
      (make-irritants-condition (list value))
      (if location
          (let ([datum (vector-ref location 0)])
            (list (make-source-condition
                    (make-annotation datum (vector-ref location 1) datum))))
          '()))))

(define (raise-changed value)
  (assertion-violation 'html "value changed after the form was evaluated" value))

(include "html/escape.ss")

(define (write-digits port n)
  (when (fx>= n 10) (write-digits port (fxquotient n 10)))
  (put-u8 port (fx+ 48 (fxremainder n 10))))

;; (most-negative-fixnum) has no fixnum negation.
(define (write-integer port n)
  (cond
    [(and (fixnum? n) (fx>= n 0)) (write-digits port n)]
    [(and (fixnum? n) (fx> n (most-negative-fixnum)))
     (put-u8 port 45)
     (write-digits port (fx- 0 n))]
    [else (put-bytevector port (string->utf8 (number->string n)))]))

(define (exact-integer? v) (and (integer? v) (exact? v)))

(define (check-html-child v location)
  (if (html-value-document? v)
      (raise-value-error "a document, an html form whose child is :html, cannot be inserted in other markup" v location)
      v))

;; A list that contains itself through car passes list? and would be
;; descended forever.
(define (walk-child-list visit v path location)
  (unless (list? v) (raise-value-error "child list is not a proper list" v location))
  (when (memq v path) (raise-value-error "child list contains itself" v location))
  (let ([path (cons v path)])
    (for-each (lambda (x) (visit x path location)) v)))

(define (check-child v location) (check-child-in v '() location))
(define (check-child-in v path location)
  (cond
    [(or (string? v) (fixnum? v)) v]
    [(html? v) (check-html-child v location)]
    [(pair? v) (walk-child-list check-child-in v path location) v]
    [(or (null? v) (exact-integer? v)) v]
    [else (raise-value-error "child is not a string, exact integer, html value or list" v location)]))

(define (check-text-child v location) (check-text-child-in v '() location))
(define (check-text-child-in v path location)
  (cond
    [(or (string? v) (exact-integer? v)) v]
    [(pair? v) (walk-child-list check-text-child-in v path location) v]
    [(null? v) v]
    [(html? v) (raise-value-error "this element holds only text, not an html value" v location)]
    [else (raise-value-error "child is not a string, exact integer or list of them" v location)]))

;; The parser moves text out of these contexts, so only html values and the
;; empty string, which helpers return for nothing, are accepted.
(define (check-table-child v location) (check-table-child-in v '() location))
(define (check-table-child-in v path location)
  (cond
    [(html? v) (check-html-child v location)]
    [(pair? v) (walk-child-list check-table-child-in v path location) v]
    [(or (null? v) (and (string? v) (fx= (string-length v) 0))) v]
    [(or (string? v) (exact-integer? v))
     (raise-value-error "text here would be moved by the parser; only html values, \"\" and lists of them can go here" v location)]
    [else (raise-value-error "child is not an html value, \"\" or list of them" v location)]))

(define (check-attribute-value v location)
  (if (or (string? v) (boolean? v) (exact-integer? v))
      v
      (raise-value-error "attribute value is not a string, exact integer, #t or #f" v location)))

(define (check-attribute-part v location)
  (if (or (string? v) (exact-integer? v))
      v
      (raise-value-error "attribute value part is not a string or exact integer" v location)))

(define (check-class-value v location)
  (cond
    [(or (string? v) (not v) (exact-integer? v)) v]
    [(eq? v #t) (raise-value-error "class cannot be #t" v location)]
    [else (raise-value-error "class is not a string, exact integer or #f" v location)]))

;; A value that fails a writer's test passed its check at evaluation and was
;; mutated since. A list mutated into a cycle is not detected and is written
;; forever: detecting it would cost every write.

(define (write-child port v)
  (cond
    [(string? v) (write-text port v)]
    [(fixnum? v) (write-integer port v)]
    [(html? v)
     (if (html-value-document? v) (raise-changed v) ((html-value-writer v) port))]
    [(pair? v)
     (let loop ([v v])
       (cond
         [(pair? v) (write-child port (car v)) (loop (cdr v))]
         [(not (null? v)) (raise-changed v)]))]
    [(null? v) (void)]
    [(exact-integer? v) (write-integer port v)]
    [else (raise-changed v)]))

(define (write-text-child port v)
  (cond
    [(string? v) (write-text port v)]
    [(exact-integer? v) (write-integer port v)]
    [(pair? v)
     (let loop ([v v])
       (cond
         [(pair? v) (write-text-child port (car v)) (loop (cdr v))]
         [(not (null? v)) (raise-changed v)]))]
    [(null? v) (void)]
    [else (raise-changed v)]))

(define (write-table-child port v)
  (cond
    [(html? v)
     (if (html-value-document? v) (raise-changed v) ((html-value-writer v) port))]
    [(pair? v)
     (let loop ([v v])
       (cond
         [(pair? v) (write-table-child port (car v)) (loop (cdr v))]
         [(not (null? v)) (raise-changed v)]))]
    [(or (null? v) (and (string? v) (fx= (string-length v) 0))) (void)]
    [else (raise-changed v)]))

(define (write-attribute-part port v)
  (if (string? v) (write-attribute-text port v) (write-integer port v)))

(define (write-attribute port name-bytes v)
  (cond
    [(string? v)
     (put-bytevector port name-bytes)
     (put-u8 port 61)
     (put-u8 port 34)
     (write-attribute-text port v)
     (put-u8 port 34)]
    [(eq? v #t) (put-bytevector port name-bytes)]
    [(not v) (void)]
    [(exact-integer? v)
     (put-bytevector port name-bytes)
     (put-u8 port 61)
     (put-u8 port 34)
     (write-integer port v)
     (put-u8 port 34)]
    [else (raise-changed v)]))

;; The class writers thread a flag saying whether ` class="` is written yet,
;; because whether any class remains is known only once the values are.

(define (start-class-entry port started?)
  (if started?
      (put-u8 port 32)
      (put-bytevector port '#vu8(32 99 108 97 115 115 61 34))))

(define (write-class-bytes port started? bytes)
  (start-class-entry port started?)
  (put-bytevector port bytes)
  #t)

(define (write-class-value port started? v)
  (cond
    [(or (not v) (and (string? v) (fx= (string-length v) 0))) started?]
    [else
     (start-class-entry port started?)
     (write-attribute-part port v)
     #t]))

(define (write-class-parts port started? parts)
  (cond
    [(for-all (lambda (p) (and (string? p) (fx= (string-length p) 0))) parts) started?]
    [else
     (start-class-entry port started?)
     (for-each (lambda (p) (write-attribute-part port p)) parts)
     #t]))

(define (write-class-end port started?)
  (when started? (put-u8 port 34)))

(define (write-nothing port) (void))

(define (write-writers port writers)
  (let loop ([ws writers])
    (unless (null? ws)
      ((car ws) port)
      (loop (cdr ws)))))

;; Recursion rather than appending to a tail, so a continuation captured in
;; a body and re-entered later cannot change a list already returned.
(define (for-each-writers proc l location)
  (unless (list? l) (raise-value-error "for-each list is not a proper list" l location))
  (let loop ([l l])
    (if (null? l)
        '()
        (let ([w (proc (car l))])
          (cons w (loop (cdr l)))))))

(define (for-each-writers* proc ls locations)
  (for-each
    (lambda (l location)
      (unless (list? l) (raise-value-error "for-each list is not a proper list" l location)))
    ls locations)
  (let ([n (length (car ls))])
    (for-each
      (lambda (l location)
        (unless (fx= (length l) n)
          (raise-value-error "for-each lists have different lengths" l location)))
      (cdr ls) (cdr locations)))
  (let loop ([ls ls])
    (if (null? (car ls))
        '()
        (let ([w (apply proc (map car ls))])
          (cons w (loop (map cdr ls)))))))

;; Raw text is collected into one bytevector so that an end tag split across
;; pieces is caught. Literal pieces were checked when the form was compiled,
;; so only a pattern that starts in a value's bytes, or in the bytes just
;; before them, needs a scan.

(define-record-type raw-text-piece
  (nongenerative)
  (sealed #t)
  (fields bytes value location))

(define (raw-text-value v location)
  (let-values ([(port extract) (open-bytevector-output-port)])
    (let write-piece ([v v] [path '()])
      (cond
        [(string? v) (write-raw-text port v)]
        [(exact-integer? v) (write-integer port v)]
        [(pair? v) (walk-child-list (lambda (x path location) (write-piece x path)) v path location)]
        [(null? v) (void)]
        [(html? v) (raise-value-error "script and style hold only text, not an html value" v location)]
        [else (raise-value-error "child is not a string, exact integer or list of them" v location)]))
    (make-raw-text-piece (extract) v location)))

(define (pattern-at? bytes i pattern)
  (let ([n (string-length pattern)])
    (and (fx<= (fx+ i n) (bytevector-length bytes))
         (let loop ([k 0])
           (or (fx= k n)
               (let ([b (bytevector-u8-ref bytes (fx+ i k))])
                 (and (fx= (if (fx<= 65 b 90) (fx+ b 32) b)
                           (char->integer (string-ref pattern k)))
                      (loop (fx+ k 1)))))))))

(define (concatenate-bytevectors bvs)
  (let ([result (make-bytevector (fold-left (lambda (n bv) (fx+ n (bytevector-length bv))) 0 bvs))])
    (let loop ([bvs bvs] [i 0])
      (unless (null? bvs)
        (let ([n (bytevector-length (car bvs))])
          (bytevector-copy! (car bvs) 0 result i n)
          (loop (cdr bvs) (fx+ i n)))))
    result))

(define (piece-bytes p) (if (raw-text-piece? p) (raw-text-piece-bytes p) p))

(define (raw-text-bytes kind patterns pieces)
  (let ([bytes (concatenate-bytevectors (map piece-bytes pieces))]
        [lookback (fx- (apply max (map string-length patterns)) 1)])
    (define (scan! from to piece)
      (do ([i (fxmax 0 from) (fx+ i 1)])
          ((fx>= i to))
        (for-each
          (lambda (pattern)
            (when (pattern-at? bytes i pattern)
              (raise-value-error
                (format "~a text would contain ~a, which ends the element or changes how it is parsed" kind pattern)
                (raw-text-piece-value piece) (raw-text-piece-location piece))))
          patterns)))
    (let loop ([pieces pieces] [start 0])
      (unless (null? pieces)
        (let* ([p (car pieces)]
               [end (fx+ start (bytevector-length (piece-bytes p)))])
          (when (raw-text-piece? p) (scan! (fx- start lookback) end p))
          (loop (cdr pieces) end))))
    bytes))

(define (write-html h port)
  (unless (html? h) (assertion-violationf 'write-html "~s is not an html value" h))
  (unless (and (output-port? port) (binary-port? port))
    (if (output-port? port)
        (assertion-violationf 'write-html "~s is a textual port; write-html needs a binary output port, and html->string gives a string" port)
        (assertion-violationf 'write-html "~s is not a binary output port" port)))
  ((html-value-writer h) port))

(define (html-bytes who h)
  (unless (html? h) (assertion-violationf who "~s is not an html value" h))
  (let-values ([(port extract) (open-bytevector-output-port)])
    ((html-value-writer h) port)
    (extract)))

(define (html->bytevector h) (html-bytes 'html->bytevector h))

(define (html->string h) (utf8->string (html-bytes 'html->string h)))

(define (raw-html x)
  (let ([bytes (cond
                 [(string? x) (string->utf8 x)]
                 [(bytevector? x) x]
                 [else (assertion-violationf 'raw-html "~s is not a string or a bytevector" x)])])
    (make-html-value (lambda (port) (put-bytevector port bytes)) #f)))
