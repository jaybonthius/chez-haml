(import (chezscheme) (check) (support) (haml))

(define (bytes-of h)
  (let-values ([(port extract) (open-bytevector-output-port)])
    (write-html h port)
    (extract)))

(test "raw-html writes a string's UTF-8 unchecked"
  (let ([h (raw-html "<b>é</b>\r")])
    (check (html? h))
    (check-equal? "<b>é</b>\r" (html->string h))
    (check-equal? (string->utf8 "<b>é</b>\r") (html->bytevector h))))

(test "raw-html takes a bytevector of UTF-8"
  (check-equal? "<i>x</i>" (html->string (raw-html (string->utf8 "<i>x</i>")))))

(test "raw-html encodes a string once, when called"
  (let* ([s (string-copy "<b>a</b>")] [h (raw-html s)])
    (string-set! s 4 #\z)
    (check-equal? "<b>a</b>" (html->string h))))

(test "html->bytevector returns a fresh bytevector each time"
  (let ([h (haml (:p "a"))])
    (check (not (eq? (html->bytevector h) (html->bytevector h))))))

(test "raw-html checks its argument"
  (check-raises (raises-with? 'raw-html "not a string or a bytevector") (raw-html 'x))
  (check-raises (raises-with? 'raw-html "not a string or a bytevector") (raw-html 5)))

(test "write-html writes to a binary output port"
  (check-equal? (string->utf8 "<p>a</p>") (bytes-of (haml (:p "a"))))
  (check-equal? (string->utf8 "<p>a</p><p>a</p>")
    (let-values ([(port extract) (open-bytevector-output-port)])
      (let ([h (haml (:p "a"))])
        (write-html h port)
        (write-html h port))
      (extract))))

(test "write-html refuses a textual port and names html->string"
  (check-raises (raises-with? 'write-html "html->string")
    (let-values ([(port extract) (open-string-output-port)])
      (write-html (haml (:p "a")) port))))

(test "write-html refuses what is not a port"
  (check-raises (raises-with? 'write-html "not a binary output port")
    (write-html (haml (:p "a")) 'port))
  (check-raises (raises-with? 'write-html "not a binary output port")
    (write-html (haml (:p "a")) (open-bytevector-input-port #vu8()))))

(test "the public procedures check for an html value under their own names"
  (check-raises (raises-with? 'write-html "not an html value")
    (let-values ([(port extract) (open-bytevector-output-port)]) (write-html "<p>" port)))
  (check-raises (raises-with? 'html->bytevector "not an html value") (html->bytevector "<p>"))
  (check-raises (raises-with? 'html->string "not an html value") (html->string '())))

(test "html? is true only for html values, documents included"
  (check (html? (haml)))
  (check (html? (haml (:html))))
  (check (not (html? "x")))
  (check (not (html? (lambda (port) (void)))))
  (check (not (html? (vector)))))

(test "an empty form writes nothing"
  (check-equal? "" (html->string (haml))))

(test "an html value cannot be opened or forged through the record inspector"
  (define (from-record-rtd? c)
    (and (assertion-violation? c) (eq? (condition-who c) 'record-rtd)))
  (check-raises from-record-rtd? (record-rtd (haml (:p "x"))))
  (check-raises from-record-rtd? (record-rtd (raw-html "x")))
  (check (not (record? (haml (:p "x"))))))

(test "the record and writers are not importable"
  (for-each
    (lambda (name)
      (check-raises syntax-violation? (eval name (environment '(haml)))))
    '(make-html-value html-value-writer html-value-document? write-child check-child
      write-text write-attribute raw-text-bytes html-context compile-in-context)))

(test "html->string decodes the UTF-8 bytes"
  (check-equal? "<p>π 😀</p>" (html->string (haml (:p "π 😀")))))

(test "integers are written as decimal digits"
  (for-each
    (lambda (n)
      (check-equal? (format "<p>~a</p>" n) (html->string (haml (:p n))))
      (check-equal? (format "<p title=\"~a\"></p>" n) (html->string (haml (:p ([title n]))))))
    (list 0 7 10 -1 -48 -10 (most-positive-fixnum) (most-negative-fixnum)
          (+ (most-positive-fixnum) 1) (- (most-negative-fixnum) 1) (expt 10 30) (- (expt 10 30)))))

(test "integer literals are written as decimal digits"
  (check-equal? "<p>-48 0 123456789012345678901234567890</p>"
    (html->string (haml (:p -48 " " 0 " " 123456789012345678901234567890)))))

(test "text escapes & < > and leaves quotes"
  (let ([s "a & b < c > d \" e ' f"])
    (check-equal? "<p>a &amp; b &lt; c &gt; d \" e ' f</p>" (html->string (haml (:p s))))
    (check-equal? "<p>a &amp; b &lt; c &gt; d \" e ' f</p>"
      (html->string (haml (:p "a & b < c > d \" e ' f"))))))

(test "attribute values escape & < > \""
  (let ([s "a & b < c > d \" e ' f"])
    (check-equal? "<p title=\"a &amp; b &lt; c &gt; d &quot; e ' f\"></p>"
      (html->string (haml (:p ([title s])))))
    (check-equal? "<p title=\"a &amp; b &lt; c &gt; d &quot; e ' f\"></p>"
      (html->string (haml (:p ([title "a & b < c > d \" e ' f"])))))))

(test "non-ASCII text is written as UTF-8"
  (let ([s "é π 😀 \x80; \x7ff; \x800; \xffff; \x10000;"])
    (check-equal? (string->utf8 (string-append "<p>" s "</p>"))
      (html->bytevector (haml (:p s))))
    (check-equal? (string->utf8 (string-append "<p title=\"" s "\"></p>"))
      (html->bytevector (haml (:p ([title s])))))))

(test "NUL is written as U+FFFD"
  (let ([s "a\x0;b"])
    (check-equal? "<p>a\xFFFD;b</p>" (html->string (haml (:p s))))
    (check-equal? "<p>a\xFFFD;b</p>" (html->string (haml (:p "a\x0;b"))))
    (check-equal? "<p title=\"a\xFFFD;b\"></p>" (html->string (haml (:p ([title s])))))
    (check-equal? "<p title=\"a\xFFFD;bc\"></p>" (html->string (haml (:p ([title s "c"])))))
    (check-equal? "<script>a\xFFFD;b</script>" (html->string (haml (:script s))))
    (check-equal? "<textarea>\na\xFFFD;b</textarea>" (html->string (haml (:textarea s)))))
  (check-equal? "<script>a\xFFFD;b\nc</script>" (html->string (haml (:script "a\x0;b\r\nc")))))

(test "CR and CRLF are written as LF, each piece converted alone"
  (let ([s "a\rb\r\nc\n\rd\r"])
    (check-equal? "<p>a\nb\nc\n\nd\n</p>" (html->string (haml (:p s))))
    (check-equal? "<p>a\nb\nc\n\nd\n</p>" (html->string (haml (:p "a\rb\r\nc\n\rd\r"))))
    (check-equal? "<p title=\"a\nb\nc\n\nd\n\"></p>" (html->string (haml (:p ([title s])))))
    (check-equal? "<script>a\nb\nc\n\nd\n</script>" (html->string (haml (:script s))))
    (check-equal? "<p>a\n\nb</p>" (let ([x "a\r"] [y "\nb"]) (html->string (haml (:p x y)))))
    (check-equal? "<p>a\n\nb</p>" (html->string (haml (:p "a\r" "\nb"))))))

(test "raw-html is written unconverted"
  (check-equal? "<p>a\rb\x0;</p>" (html->string (haml (:p (raw-html "a\rb\x0;"))))))

(test "lists are written element by element, nested lists included"
  (check-equal? "<p>ab1<b>c</b>d</p>"
    (html->string (haml (:p (list "a" (list "b" 1 '()) (haml (:b "c")) '(("d")))))))
  (check-equal? "<p></p>" (html->string (haml (:p (list))))))

(test "html values are written in place, as often as they appear"
  (let ([b (haml (:b "x"))])
    (check-equal? "<p><b>x</b><b>x</b></p>" (html->string (haml (:p b b))))))

(define-syntax raises-value-error
  (syntax-rules ()
    [(_ fragment e) (check-raises (raises-with? 'haml fragment) e)]))

(test "child values other than strings, exact integers, html values and lists raise"
  (for-each
    (lambda (v)
      (raises-value-error "child is not a string, exact integer, html value or list"
        (haml (:p v))))
    (list #f #t 1.5 1/2 'sym (void) #\a (vector) (string->utf8 "x") (lambda () 1)))
  (raises-value-error "child is not a string, exact integer, html value or list"
    (haml (:p (list "a" 'b))))
  (raises-value-error "not a proper list" (haml (:p (cons "a" "b"))))
  (raises-value-error "not a proper list"
    (let ([l (list "a" "b")]) (set-cdr! (cdr l) l) (haml (:p l)))))

(test "a child list that contains itself raises"
  (let ([l (list "a")])
    (set-car! l l)
    (raises-value-error "child list contains itself" (haml (:p l)))
    (raises-value-error "child list contains itself" (haml (:title l)))
    (raises-value-error "child list contains itself" (haml (:table l)))
    (raises-value-error "child list contains itself" (haml (:script l))))
  (let ([l (list "a" "b")])
    (set-car! (cdr l) (list l))
    (raises-value-error "child list contains itself" (haml (:p l))))
  (let ([shared (list "x")])
    (check-equal? "<p>xx</p>" (html->string (haml (:p (list shared shared)))))))

(test "a value error is an &assertion with who html, the value and a &source"
  (let ([c (guard (c [#t c]) (let ([v 'bad]) (haml (:p v))))])
    (check (assertion-violation? c))
    (check-equal? 'haml (condition-who c))
    (check-equal? '(bad) (condition-irritants c))
    (check (source-condition? c))
    (check (contains? (condition-text c) "values-test.sps"))))

(test "values are checked where the form is evaluated, before anything is written"
  (let* ([log '()]
         [note (lambda (n v) (set! log (cons n log)) v)])
    (check-raises (raises-with? 'haml "child is not")
      (haml (:p (note 1 "a") (note 2 'bad) (note 3 "c"))))
    (check-equal? '(2 1) log)))

(test "expressions are evaluated once, in source order"
  (let* ([log '()]
         [note (lambda (x) (set! log (cons x log)) x)])
    (haml (:p ([title (note "1")] [id (note "2")] [class (note "3") (note "4")])
            (note "5")
            (:b ([lang (note "6")]) (note "7"))
            (when (note #t) (note "8"))
            (note "9")))
    (check-equal? '("1" "2" "3" "4" "5" "6" "7" #t "8" "9") (reverse log))))

(define (evaluation-order make)
  (let* ([log '()] [note (lambda (x) (set! log (cons x log)) x)])
    (make note)
    (reverse log)))

(test "class entries, raw text and for-each lists are evaluated in source order"
  (check-equal? '(1 2)
    (evaluation-order (lambda (note) (haml (:p.s ([title (note 1)] [class (note 2)]))))))
  (check-equal? '(1 2 3)
    (evaluation-order
      (lambda (note) (haml (:p ([class (note 1)] [title (note 2)] [class (note 3)]))))))
  (check-equal? '(1 2 3 4)
    (evaluation-order
      (lambda (note)
        (haml (:p ([id (note 1)] [class "a" (note 2)] [title (note 3)] [class (note 4) "b"]))))))
  (check-equal? '(1 2 3)
    (evaluation-order (lambda (note) (haml (:p ([class (note 1) (note 2) (note 3)]))))))
  (check-equal? '(1 2 3)
    (evaluation-order
      (lambda (note) (haml (:p ([class (note 1)] [class (note 2)] [class (note 3)]))))))
  (check-equal? '(1 2 3)
    (evaluation-order (lambda (note) (haml (:p ([title "x" (note 1) (note 2) (note 3)]))))))
  (check-equal? '("a" "c")
    (evaluation-order (lambda (note) (haml (:script (note "a") "b" (note "c"))))))
  (check-equal? '("a" "b" "c")
    (evaluation-order (lambda (note) (haml (:script (note "a") (note "b") (note "c"))))))
  (check-equal? '((1) (2) (3))
    (evaluation-order
      (lambda (note)
        (haml (:dl (for-each (lambda (a b c) (:dt a) (:dd b c))
                     (note (list 1)) (note (list 2)) (note (list 3)))))))))

(test "a markup let evaluates its inits in source order"
  (check-equal? '(1 2 3)
    (evaluation-order
      (lambda (note) (haml (:p (let ([a (note 1)] [b (note 2)] [c (note 3)]) a b c))))))
  (check-equal? "<p>123</p>"
    (html->string (haml (:p (let ([a 1] [b 2] [c 3]) a b c)))))
  (check-equal? "<p>21</p>"
    (let ([a 1] [b 2])
      (html->string (haml (:p (let ([a b] [b a]) a b)))))))

(test "a document cannot be a child"
  (let ([page (haml (:html (:body)))])
    (raises-value-error "a document" (haml (:div page)))
    (raises-value-error "a document" (haml (:div (list page))))))

(test "a document is written with its doctype"
  (check-equal? "<!DOCTYPE html><html lang=\"en\"><body></body></html>"
    (html->string (haml (:html ([lang "en"]) (:body))))))

(test "attribute values: string, exact integer, #t and #f"
  (let ([yes #t] [no #f] [s "x"] [n -3])
    (check-equal? "<input value=\"x\" size=\"-3\" disabled>"
      (html->string (haml (:input ([value s] [size n] [disabled yes] [readonly no])))))))

(test "attribute values of other types raise"
  (for-each
    (lambda (v)
      (raises-value-error "attribute value is not a string, exact integer, #t or #f"
        (haml (:p ([title v])))))
    (list 1.5 'sym (void) '("a") (haml (:b))))
  (for-each
    (lambda (v)
      (raises-value-error "attribute value part is not a string or exact integer"
        (haml (:p ([title "a" v])))))
    (list #t #f 1.5 'sym '("a"))))

(test "a value mutated after evaluation so that it fails its check raises when written"
  (let* ([l (list "a" "b")] [h (haml (:p l))])
    (set-car! (cdr l) 'bad)
    (check-raises (raises-with? 'haml "value changed after the form was evaluated")
      (html->string h)))
  (let* ([l (list "a" "b")] [h (haml (:p l))])
    (set-cdr! (cdr l) 'tail)
    (check-raises (raises-with? 'haml "value changed after the form was evaluated")
      (html->string h))))

(test "strings are not copied: mutation after evaluation changes the output"
  (let* ([s (string-copy "abc")] [h (haml (:p s))])
    (string-set! s 0 #\z)
    (check-equal? "<p>zbc</p>" (html->string h))))

(test-exit)
