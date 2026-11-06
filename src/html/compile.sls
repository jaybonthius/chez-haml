(library (html compile)
  (export compile-html)
  (import (chezscheme) (html markup) (html rules))

  (include "escape.ss")

  (define-record-type sequence
    (nongenerative) (sealed #t)
    (fields (mutable bindings) (mutable items))
    (protocol (lambda (new) (lambda () (new '() '())))))

  (define-record-type binding
    (nongenerative) (sealed #t)
    (fields temporary expression context))

  (define any-context (string->uninterned-symbol "any"))

  (define-record-type place
    (nongenerative) (sealed #t)
    (fields namespace ancestors kind top-first-child context nested?))

  (define-record-type bound-attribute
    (nongenerative) (sealed #t)
    (fields attribute parts))

  (define-record-type contributor
    (nongenerative) (sealed #t)
    (fields kind value))

  (define (text-bytes s)
    (call-with-bytevector-output-port (lambda (p) (write-text p s))))

  (define (attribute-bytes s)
    (call-with-bytevector-output-port (lambda (p) (write-attribute-text p s))))

  (define (raw-text-string s)
    (utf8->string (call-with-bytevector-output-port (lambda (p) (write-raw-text p s)))))

  (define (whitespace-string? s) (for-all html-whitespace? (string->list s)))

  (define (location-text stx)
    (let ([a (and stx (syntax->annotation stx))])
      (and a
           (let ([source (annotation-source a)])
             (call-with-values
               (lambda () (locate-source-object-source source #t #t))
               (case-lambda
                 [(path line char) (format "line ~a, char ~a of ~a" line char path)]
                 [() (format "char ~a of ~a" (source-object-bfp source)
                       (source-file-descriptor-path (source-object-sfd source)))]))))))

  ;; The printer shows a &source form to depth 3 and length 6; quoting more
  ;; of the expression would only enlarge the compiled code.
  (define (abbreviate datum)
    (let walk ([d datum] [depth 3])
      (cond
        [(pair? d)
         (if (fx= depth 0)
             '...
             (let loop ([d d] [n 0])
               (cond
                 [(null? d) '()]
                 [(not (pair? d)) (walk d (fx- depth 1))]
                 [(fx= n 6) '(...)]
                 [else (cons (walk (car d) (fx- depth 1)) (loop (cdr d) (fx+ n 1)))])))]
        [(vector? d) '...]
        [else d])))

  (define (ancestor->datum a)
    (list (symbol->string (ancestor-namespace a)) (ancestor-name a) (ancestor-first-child a)
          (ancestor-location a)))

  (define (datum->ancestor d)
    (apply make-ancestor (string->symbol (car d)) (cdr d)))

  (define (serialize-context namespace ancestors)
    (cons (symbol->string namespace) (map ancestor->datum ancestors)))

  (define (html-ancestor? a name)
    (and a (eq? (ancestor-namespace a) 'html) (string=? (ancestor-name a) name)))

  (define (children-kind namespace name grandparent)
    (if (eq? namespace 'svg)
        'normal
        (case (element-kind 'html name)
          [(void) 'void]
          [(no-children) 'empty]
          [(escapable-text) 'text]
          [(raw-text) 'raw]
          [else (if (or (table-text-element? name)
                        (and (string=? name "noscript") (html-ancestor? grandparent "head")))
                    'table
                    'normal)])))

  (define (literal-elements children)
    (let walk ([nodes children])
      (apply append
        (map (lambda (n)
               (cond
                 [(element? n) (list n)]
                 [(when-form? n) (walk (when-form-children n))]
                 [(unless-form? n) (walk (unless-form-children n))]
                 [(if-form? n) (walk (list (if-form-then n) (if-form-else n)))]
                 [(begin-form? n) (walk (begin-form-children n))]
                 [(cond-form? n)
                  (walk (apply append (map cond-clause-children (cond-form-clauses n))))]
                 [(case-form? n)
                  (walk (apply append (map case-clause-children (case-form-clauses n))))]
                 [(let-form? n) (walk (let-form-children n))]
                 [(for-each-form? n) (walk (for-each-form-children n))]
                 [else '()]))
             nodes))))

  (define (first-mode-setter children)
    (let ([e (find (lambda (e) (template-mode-setter? (element-name e)))
               (literal-elements children))])
      (and e (element-name e))))

  (define (literal-attribute-value e name)
    (let ([a (find (lambda (a) (string=? (attribute-name a) name)) (element-attributes e))])
      (and a
           (let ([parts (attribute-parts a)])
             (and (pair? parts) (null? (cdr parts)) (text-literal? (car parts))
                  (text-literal-string (car parts)))))))

  (define (svg-marked? e)
    (or (svg-only-element-name? (element-name e))
        (equal? (literal-attribute-value e "xmlns") svg-namespace)))

  (define (atom-syntax? x)
    (let ([d (syntax->datum x)])
      (or (identifier? x) (string? d) (number? d) (boolean? d) (char? d))))

  (define (markup-tag? x)
    (and (identifier? x)
         (let* ([s (symbol->string (syntax->datum x))]
                [n (string-length s)])
           (and (fx> n 1)
                (char=? (string-ref s 0) #\:)
                (let loop ([i 1])
                  (cond
                    [(or (fx= i n) (char=? (string-ref s i) #\.))
                     (known-element-name? (substring s 1 i))]
                    [else (loop (fx+ i 1))]))))))

  (define (concatenate bvs)
    (call-with-bytevector-output-port
      (lambda (p) (for-each (lambda (bv) (put-bytevector p bv)) bvs))))

  (define (text-moved? place)
    (case (place-kind place)
      [(table) #t]
      [(top) (and (place-top-first-child place)
                  (not (text-allowed-at-top-after? (place-top-first-child place))))]
      [else (let ([parent (and (pair? (place-ancestors place)) (car (place-ancestors place)))])
              (and (html-ancestor? parent "template")
                   (ancestor-first-child parent)
                   (not (text-allowed-in-template-after? (ancestor-first-child parent)))))]))

  (define (moved-text-message place)
    (let ([parent (and (pair? (place-ancestors place)) (car (place-ancestors place)))])
      (if parent
          (format "text in ~a would be moved by the parser; only whitespace can go here"
            (ancestor-name parent))
          "text at the top of a form of table parts would be moved by the parser")))

  (define (element-namespace place name)
    (if (or (eq? (place-namespace place) 'svg) (string=? name "svg")) 'svg 'html))

  (define (hidden-input? e)
    (and (string=? (element-name e) "input")
         (let ([type (literal-attribute-value e "type")])
           (and type (string-ci=? type "hidden")))))

  (define (node-form n)
    (cond
      [(element? n) (element-form n)]
      [(text-literal? n) (text-literal-form n)]
      [(integer-literal? n) (integer-literal-form n)]
      [(expression? n) (expression-form n)]
      [(when-form? n) (when-form-form n)]
      [(unless-form? n) (unless-form-form n)]
      [(if-form? n) (if-form-form n)]
      [(begin-form? n) (begin-form-form n)]
      [(cond-form? n) (cond-form-form n)]
      [(case-form? n) (case-form-form n)]
      [(let-form? n) (let-form-form n)]
      [else (for-each-form-form n)]))

  (define (first-child-form children) (node-form (car children)))

  (define (raw-text-problem kind s)
    (let ([lower (ascii-downcase s)])
      (find (lambda (p)
              (let ([n (string-length p)] [m (string-length lower)])
                (let loop ([i 0])
                  (and (fx<= (fx+ i n) m)
                       (or (string=? (substring lower i (fx+ i n)) p) (loop (fx+ i 1)))))))
            (raw-text-patterns kind))))

  (define (class-attribute? a) (string=? (attribute-name a) "class"))

  (define (attribute-name-bytes a) (string->utf8 (string-append " " (attribute-name a))))

  (define (literal-part-string p)
    (if (text-literal? p)
        (text-literal-string p)
        (number->string (integer-literal-value p))))

  (define (join strings)
    (fold-left (lambda (acc s) (if (string=? acc "") s (string-append acc " " s))) "" strings))

  (define (constant-contributor? c) (eq? (contributor-kind c) 'constant))

  (define (merge-constant-classes contributors)
    (let loop ([cs contributors])
      (cond
        [(null? cs) '()]
        [(and (constant-contributor? (car cs)) (pair? (cdr cs)) (constant-contributor? (cadr cs)))
         (loop (cons (make-contributor 'constant
                       (string-append (contributor-value (car cs)) " " (contributor-value (cadr cs))))
                     (cddr cs)))]
        [else (cons (car cs) (loop (cdr cs)))])))

  (define (compile-html form children received-context here html-id)
    (define (helper name) (datum->syntax here name))

    (define (bad message subform)
      (syntax-violation 'html message form subform))

    (define (location-constant e)
      (let ([a (syntax->annotation e)])
        (datum->syntax here
          (and a (vector (abbreviate (syntax->datum e)) (annotation-source a))))))

    (define (bind! seq expression context)
      (let ([t (car (generate-temporaries '(t)))])
        (sequence-bindings-set! seq
          (cons (make-binding t expression context) (sequence-bindings seq)))
        t))

    (define (emit! seq item)
      (sequence-items-set! seq (cons item (sequence-items seq))))

    (define (emit-string! seq s) (emit! seq (string->utf8 s)))

    (define (top-context-rules) #`(syntax-rules () [(_ k f) (k #f f)]))

    (define (context-rules context)
      #`(syntax-rules () [(_ k f) (k #,(datum->syntax here context) f)]))

    ;; Tests, case keys, for-each lists and attribute values are not written
    ;; where they appear, so html forms inside them are checked as at the top.
    (define (at-top e place)
      (if (or (not (place-context place)) (atom-syntax? e))
          e
          #`(fluid-let-syntax ([#,(helper 'html-context) #,(top-context-rules)]) #,e)))

    (define (check-no-markup! e)
      (let walk ([x e])
        (syntax-case x ()
          [(h . t)
           (unless (and (identifier? #'h)
                        (exists (lambda (k) (free-identifier=? #'h k))
                          (list #'quote #'quasiquote #'syntax #'quasisyntax html-id)))
             (when (markup-tag? #'h)
               (bad "markup inside a Scheme expression needs its own (html …)" x))
             (let loop ([x x])
               (syntax-case x ()
                 [(a . d) (begin (walk #'a) (loop #'d))]
                 [_ (void)])))]
          [_ (void)])))

    (define (sequence-statements seq)
      (let loop ([items (reverse (sequence-items seq))] [pending '()] [out '()])
        (define (flush)
          (if (null? pending)
              out
              (cons #`(put-bytevector port '#,(datum->syntax here (concatenate (reverse pending))))
                    out)))
        (cond
          [(null? items) (reverse (flush))]
          [(bytevector? (car items)) (loop (cdr items) (cons (car items) pending) out)]
          [else (loop (cdr items) '() (cons (car items) (flush)))])))

    ;; Bindings that need the same context share one fluid-let-syntax; each
    ;; costs about as much expansion time as a few hundred plain bindings.
    (define (wrap-bindings bindings body in-effect)
      (let loop ([bindings bindings] [in-effect in-effect])
        (if (null? bindings)
            body
            (let* ([context (let ([b (find (lambda (b) (not (eq? (binding-context b) any-context)))
                                       bindings)])
                              (if b (binding-context b) in-effect))]
                   [group (let take ([bs bindings])
                            (if (and (pair? bs)
                                     (let ([c (binding-context (car bs))])
                                       (or (eq? c any-context) (equal? c context))))
                                (cons (car bs) (take (cdr bs)))
                                '()))]
                   [rest (list-tail bindings (length group))]
                   [inner #`(let* #,(map (lambda (b) #`[#,(binding-temporary b) #,(binding-expression b)])
                                         group)
                              #,(loop rest context))])
              (if (equal? context in-effect)
                  inner
                  #`(fluid-let-syntax ([#,(helper 'html-context)
                                        #,(if context (context-rules context) (top-context-rules))])
                      #,inner))))))

    (define (sequence->writer seq in-effect)
      (let ([statements (sequence-statements seq)])
        (if (null? statements)
            (helper 'write-nothing)
            (wrap-bindings (reverse (sequence-bindings seq))
              #`(lambda (port) #,@statements)
              in-effect))))

    (define (compile-children! seq children place)
      (for-each (lambda (n) (compile-node! seq n place)) children))

    (define (compile-node! seq n place)
      (cond
        [(text-literal? n) (compile-text! seq n place)]
        [(integer-literal? n) (compile-integer! seq n place)]
        [(expression? n) (compile-expression! seq n place)]
        [(element? n) (compile-element! seq n place)]
        [(begin-form? n) (compile-children! seq (begin-form-children n) place)]
        [else (compile-control! seq n place)]))

    (define (refuse-children-of place subform)
      (define (parent-name) (ancestor-name (car (place-ancestors place))))
      (case (place-kind place)
        [(void) (bad (format "~a is a void element and cannot have children" (parent-name)) subform)]
        [(empty) (bad (format "~a cannot have children" (parent-name)) subform)]
        [(raw) (bad (format "~a holds only text; compute it in Scheme" (parent-name)) subform)]))

    (define (compile-text! seq n place)
      (let ([s (text-literal-string n)])
        (refuse-children-of place (text-literal-form n))
        (when (and (text-moved? place) (not (whitespace-string? s)))
          (bad (moved-text-message place) (text-literal-form n)))
        (emit! seq (text-bytes s))))

    (define (compile-integer! seq n place)
      (refuse-children-of place (integer-literal-form n))
      (when (text-moved? place)
        (bad (moved-text-message place) (integer-literal-form n)))
      (emit-string! seq (number->string (integer-literal-value n))))

    (define (compile-expression! seq n place)
      (let ([e (expression-form n)])
        (refuse-children-of place e)
        (check-no-markup! e)
        (let-values ([(check write)
                      (cond
                        [(eq? (place-kind place) 'text) (values 'check-text-child 'write-text-child)]
                        [(text-moved? place) (values 'check-table-child 'write-table-child)]
                        [else (values 'check-child 'write-child)])])
          (let ([t (bind! seq #`(#,(helper check) #,e '#,(location-constant e))
                     (place-context place))])
            (emit! seq #`(#,(helper write) port #,t))))))

    (define (compile-control! seq n place)
      (define (writer children)
        (let ([sub (make-sequence)])
          (compile-children! sub children place)
          (sequence->writer sub (place-context place))))
      (define (bind-writer! expression)
        (let ([w (bind! seq expression (place-context place))])
          (emit! seq #`(#,w port))))
      (cond
        [(when-form? n)
         (check-no-markup! (when-form-test n))
         (bind-writer!
           #`(if #,(at-top (when-form-test n) place)
                 #,(writer (when-form-children n))
                 #,(helper 'write-nothing)))]
        [(unless-form? n)
         (check-no-markup! (unless-form-test n))
         (bind-writer!
           #`(if #,(at-top (unless-form-test n) place)
                 #,(helper 'write-nothing)
                 #,(writer (unless-form-children n))))]
        [(if-form? n)
         (check-no-markup! (if-form-test n))
         (bind-writer!
           #`(if #,(at-top (if-form-test n) place)
                 #,(writer (list (if-form-then n)))
                 #,(writer (list (if-form-else n)))))]
        [(cond-form? n)
         (let ([clauses (cond-form-clauses n)])
           (for-each (lambda (cl) (when (cond-clause-test cl) (check-no-markup! (cond-clause-test cl))))
             clauses)
           (bind-writer!
             #`(cond
                 #,@(map-in-order
                      (lambda (cl)
                        (if (cond-clause-test cl)
                            #`[#,(at-top (cond-clause-test cl) place)
                               #,(writer (cond-clause-children cl))]
                            #`[else #,(writer (cond-clause-children cl))]))
                      clauses)
                 #,@(if (exists (lambda (cl) (not (cond-clause-test cl))) clauses)
                        '()
                        (list #`[else #,(helper 'write-nothing)])))))]
        [(case-form? n)
         (let ([clauses (case-form-clauses n)])
           (check-no-markup! (case-form-key n))
           (bind-writer!
             #`(#,(case-form-keyword n) #,(at-top (case-form-key n) place)
                 #,@(map-in-order
                      (lambda (cl)
                        #`[#,(case-clause-data cl) #,(writer (case-clause-children cl))])
                      clauses)
                 #,@(if (exists (lambda (cl) (identifier? (case-clause-data cl))) clauses)
                        '()
                        (list #`[else #,(helper 'write-nothing)])))))]
        [(let-form? n)
         (for-each check-no-markup! (let-form-inits n))
         (bind-writer!
           (with-syntax ([(x ...) (let-form-names n)]
                         [(e ...) (let-form-inits n)]
                         [body (writer (let-form-children n))])
             (if (let-form-sequential? n)
                 #'(let* ([x e] ...) body)
                 (with-syntax ([(t ...) (generate-temporaries #'(x ...))])
                   #'(let* ([t e] ...) (let ([x t] ...) body))))))]
        [(for-each-form? n)
         (for-each check-no-markup! (for-each-form-lists n))
         (let* ([lists (for-each-form-lists n)]
                [temporaries (map-in-order (lambda (l) (bind! seq (at-top l place) #f)) lists)]
                [locations (map location-constant lists)]
                [proc (with-syntax ([(x ...) (for-each-form-names n)]
                                    [body (writer (for-each-form-children n))])
                        #'(lambda (x ...) body))]
                [w (bind! seq
                     (if (null? (cdr lists))
                         #`(#,(helper 'for-each-writers) #,proc #,(car temporaries)
                             '#,(car locations))
                         #`(#,(helper 'for-each-writers*) #,proc (list #,@temporaries)
                             '#,locations))
                     (place-context place))])
           (emit! seq #`(#,(helper 'write-writers) port #,w)))]))

    (define (check-element-name! e place)
      (let ([name (element-name e)] [tag (element-tag e)])
        (if (eq? (place-namespace place) 'svg)
            (unless (svg-element-name? name)
              (bad (format "~a is not an SVG 2 element; HTML can go inside an SVG foreignObject, desc or title"
                     name)
                   tag))
            (unless (or (html-element-name? name) (string=? name "svg") (custom-element-name? name))
              (bad (cond
                     [(string=? name "math") "MathML is not supported"]
                     [(and (svg-element-name? name) (not (html-element-name? name)))
                      (format "~a is an SVG element; write it inside (:svg …), or at the top of a form" name)]
                     [else (format "unknown element ~a" name)])
                   tag)))))

    (define (check-nesting! e place)
      (when (eq? (place-namespace place) 'html)
        (let-values ([(message culprit)
                      (nesting-problem (element-name e) (place-ancestors place)
                        (place-top-first-child place) (null? (element-children e))
                        (hidden-input? e))])
          (when message
            (bad (string-append message
                   (let ([where (and culprit (ancestor-location culprit))])
                     (if where (format " (the ~a at ~a)" (ancestor-name culprit) where) ""))
                   (if (place-nested? place)
                       "; if this form's value is not inserted here, build it outside the element, for example in a procedure"
                       ""))
                 (element-tag e))))))

    (define (compile-element! seq e place)
      (refuse-children-of place (element-tag e))
      (when (eq? (place-kind place) 'text)
        (bad (format "~a holds only text" (ancestor-name (car (place-ancestors place))))
             (element-tag e)))
      (check-element-name! e place)
      (check-nesting! e place)
      (emit-element! seq e place))

    (define (compile-document-root! seq e place)
      (check-document-children! (element-children e))
      (emit-string! seq "<!DOCTYPE html>")
      (emit-element! seq e place))

    (define (emit-element! seq e place)
      (let* ([name (element-name e)]
             [namespace (element-namespace place name)]
             [children (element-children e)])
        (emit-string! seq (string-append "<" name))
        (compile-attributes! seq e namespace)
        (emit-string! seq ">")
        (when (and (eq? namespace 'html) (newline-after-start-tag? name))
          (emit-string! seq "\n"))
        (let* ([ancestor (make-ancestor namespace name
                           (and (eq? namespace 'html) (string=? name "template")
                                (first-mode-setter children))
                           (location-text (element-tag e)))]
               [ancestors (cons ancestor (place-ancestors place))]
               [child-namespace (if (and (eq? namespace 'svg) (not (html-integration-point? name)))
                                    'svg
                                    'html)]
               [kind (children-kind namespace name (and (pair? (place-ancestors place))
                                                        (car (place-ancestors place))))])
          (case kind
            [(raw) (compile-raw-text! seq e (serialize-context child-namespace ancestors))]
            [(void)
             (unless (null? children)
               (bad (format "~a is a void element and cannot have children" name)
                    (first-child-form children)))]
            [else
             (compile-children! seq children
               (make-place child-namespace ancestors kind #f
                 (serialize-context child-namespace ancestors) (place-nested? place)))])
          (unless (eq? kind 'void)
            (emit-string! seq (string-append "</" name ">"))))))

    ;; A for-each body is walked twice, since it may repeat.
    (define (check-document-children! children)
      (define order '(start head body))
      (define (later a b) (if (memq a (memq b order)) a b))
      (define (walk nodes state)
        (fold-left
          (lambda (state n)
            (cond
              [(element? n)
               (let ([name (element-name n)])
                 (cond
                   [(string=? name "head")
                    (unless (eq? state 'start)
                      (bad "html holds at most one head, before body" (element-tag n)))
                    'head]
                   [(string=? name "body")
                    (when (eq? state 'body)
                      (bad "html holds at most one body" (element-tag n)))
                    'body]
                   [else state]))]
              [(or (when-form? n) (unless-form? n))
               (later state (walk (if (when-form? n) (when-form-children n) (unless-form-children n))
                              state))]
              [(if-form? n)
               (let* ([then-state (walk (list (if-form-then n)) state)]
                      [else-state (walk (list (if-form-else n)) state)])
                 (later then-state else-state))]
              [(cond-form? n)
               (fold-left later state
                 (map-in-order (lambda (cl) (walk (cond-clause-children cl) state))
                   (cond-form-clauses n)))]
              [(case-form? n)
               (fold-left later state
                 (map-in-order (lambda (cl) (walk (case-clause-children cl) state))
                   (case-form-clauses n)))]
              [(begin-form? n) (walk (begin-form-children n) state)]
              [(let-form? n) (walk (let-form-children n) state)]
              [(for-each-form? n)
               (let ([once (walk (for-each-form-children n) state)])
                 (later once (walk (for-each-form-children n) once)))]
              [else state]))
          state nodes))
      (walk children 'start))

    (define (compile-raw-text! seq e context)
      (let ([kind (element-name e)])
        (define (check-literal! s subform)
          (let ([problem (raw-text-problem kind s)])
            (when problem
              (bad (format "~a text contains ~a, which ends the element or changes how it is parsed"
                     kind problem)
                   subform))))
        (define pieces
          (let loop ([children (element-children e)] [text #f] [text-form #f])
            (define (flush rest)
              (cond
                [text (check-literal! text text-form) (cons text rest)]
                [else rest]))
            (if (null? children)
                (flush '())
                (let ([n (car children)])
                  (cond
                    [(or (text-literal? n) (integer-literal? n))
                     (let ([s (if (text-literal? n)
                                  (raw-text-string (text-literal-string n))
                                  (number->string (integer-literal-value n)))])
                       (loop (cdr children) (if text (string-append text s) s)
                         (or text-form (node-form n))))]
                    [(expression? n)
                     (check-no-markup! (expression-form n))
                     (flush (cons n (loop (cdr children) #f #f)))]
                    [(element? n)
                     (bad (format "~a holds only text, not elements" kind) (element-tag n))]
                    [else
                     (bad (format "~a holds only text; compute it in Scheme rather than with a control form"
                            kind)
                          (node-form n))])))))
        (if (for-all string? pieces)
            (emit! seq (string->utf8 (apply string-append pieces)))
            (let* ([piece-syntaxes
                    (map-in-order
                      (lambda (p)
                        (if (string? p)
                            #`'#,(datum->syntax here (string->utf8 p))
                            (let ([x (expression-form p)])
                              (bind! seq #`(#,(helper 'raw-text-value) #,x '#,(location-constant x))
                                context))))
                      pieces)]
                   [t (bind! seq #`(#,(helper 'raw-text-bytes)
                                     '#,(datum->syntax here (string->symbol kind))
                                     '#,(datum->syntax here (raw-text-patterns kind))
                                     (list #,@piece-syntaxes))
                        any-context)])
              (emit! seq #`(put-bytevector port #,t))))))

    (define (compile-attributes! seq e namespace)
      (let ([classes (element-classes e)])
        (check-attributes! e namespace)
        (for-each
          (lambda (c)
            (when (memv #\nul (string->list c))
              (bad "a class name cannot contain NUL" (element-tag e))))
          classes)
        (let* ([bound (map-in-order
                        (lambda (a) (make-bound-attribute a (bind-attribute-parts! seq a)))
                        (element-attributes e))]
               [class-entries (filter (lambda (b) (class-attribute? (bound-attribute-attribute b)))
                                bound)])
          (when (pair? classes) (compile-class! seq classes class-entries))
          (for-each
            (lambda (b)
              (cond
                [(not (class-attribute? (bound-attribute-attribute b))) (compile-attribute! seq b)]
                [(and (null? classes) (eq? b (car class-entries)))
                 (compile-class! seq classes class-entries)]))
            bound))))

    (define (check-attributes! e namespace)
      (let loop ([attributes (element-attributes e)] [seen '()])
        (unless (null? attributes)
          (let* ([a (car attributes)]
                 [name (attribute-name a)]
                 [subform (attribute-name-syntax a)]
                 [lower (ascii-downcase name)])
            (unless (valid-attribute-name? name)
              (bad "invalid attribute name" subform))
            (if (eq? namespace 'html)
                (when (exists ascii-upper-case? (string->list name))
                  (bad (format "attribute names on HTML elements are lower case; the parser would read this as ~a"
                         lower)
                       subform))
                (check-svg-attribute! a))
            (when (and (identifier? subform) (not (known-attribute-name? name)))
              (bad (format "unknown attribute name ~a; write the name as a string, [~s …], or if this is a call, wrap it in (begin …)"
                     name name)
                   subform))
            (when (and (member lower seen) (not (string=? lower "class")))
              (bad "duplicate attribute" subform))
            (when (and (string=? name "class") (null? (attribute-parts a)))
              (bad "class needs a value" (attribute-form a)))
            (loop (cdr attributes) (cons lower seen))))))

    (define (check-svg-attribute! a)
      (let ([name (attribute-name a)] [subform (attribute-name-syntax a)])
        (when (and (exists ascii-upper-case? (string->list name))
                   (not (adjusted-svg-attribute-name? name)))
          (bad "on SVG elements, the only attribute names with upper case are SVG 2's camel-case ones"
               subform))
        (when (lower-case-of-adjusted-svg-attribute? name)
          (bad "write the SVG attribute in its camel-case spelling, which is the one the parser keeps"
               subform))
        (when (and (reserved-foreign-prefix? name) (not (foreign-attribute-name? name)))
          (bad "not a namespaced attribute the HTML syntax can express" subform))
        (let ([expected (cond
                          [(string=? name "xmlns") svg-namespace]
                          [(string=? name "xmlns:xlink") xlink-namespace]
                          [else #f])])
          (when expected
            (let ([parts (attribute-parts a)])
              (unless (and (pair? parts) (null? (cdr parts)) (text-literal? (car parts))
                           (string=? (text-literal-string (car parts)) expected))
                (bad (format "~a must be the literal ~s" name expected) (attribute-form a))))))))

    (define (bind-attribute-parts! seq a)
      (let* ([parts (attribute-parts a)]
             [check (cond
                      [(and (pair? parts) (pair? (cdr parts))) 'check-attribute-part]
                      [(class-attribute? a) 'check-class-value]
                      [else 'check-attribute-value])])
        (map-in-order
          (lambda (p)
            (cond
              [(expression? p)
               (let ([x (expression-form p)])
                 (check-no-markup! x)
                 (bind! seq #`(#,(helper check) #,x '#,(location-constant x)) #f))]
              [else (literal-part-string p)]))
          parts)))

    (define (compile-attribute! seq b)
      (let ([a (bound-attribute-attribute b)] [parts (bound-attribute-parts b)])
        (cond
          [(null? parts) (emit! seq (attribute-name-bytes a))]
          [(and (null? (cdr parts)) (not (string? (car parts))))
           (emit! seq #`(#,(helper 'write-attribute) port '#,(datum->syntax here (attribute-name-bytes a))
                          #,(car parts)))]
          [else
           (emit! seq (attribute-name-bytes a))
           (emit-string! seq "=\"")
           (for-each
             (lambda (p)
               (if (string? p)
                   (emit! seq (attribute-bytes p))
                   (emit! seq #`(#,(helper 'write-attribute-part) port #,p))))
             parts)
           (emit-string! seq "\"")])))

    (define (compile-class! seq classes entries)
      (let* ([contributors
              (append
                (if (null? classes) '() (list (make-contributor 'constant (join classes))))
                (map-in-order
                  (lambda (b)
                    (let ([parts (bound-attribute-parts b)])
                      (cond
                        [(for-all string? parts) (make-contributor 'constant (apply string-append parts))]
                        [(null? (cdr parts)) (make-contributor 'value (car parts))]
                        [else (make-contributor 'parts (bind! seq #`(list #,@parts) any-context))])))
                  entries))]
             [merged (merge-constant-classes
                       (filter (lambda (c) (not (and (constant-contributor? c)
                                                     (string=? (contributor-value c) ""))))
                         contributors))])
        (cond
          [(null? merged) (void)]
          [(for-all constant-contributor? merged)
           (emit-string! seq " class=\"")
           (emit! seq (attribute-bytes (contributor-value (car merged))))
           (emit-string! seq "\"")]
          [else
           (emit! seq
             #`(#,(helper 'write-class-end) port
                 #,(fold-left
                     (lambda (started c)
                       (case (contributor-kind c)
                         [(constant)
                          #`(#,(helper 'write-class-bytes) port #,started
                              '#,(datum->syntax here (attribute-bytes (contributor-value c))))]
                         [(value) #`(#,(helper 'write-class-value) port #,started #,(contributor-value c))]
                         [else #`(#,(helper 'write-class-parts) port #,started #,(contributor-value c))]))
                     #'#f
                     merged)))])))

    (define (top-place)
      (if received-context
          (let* ([namespace (string->symbol (car received-context))]
                 [ancestors (map datum->ancestor (cdr received-context))]
                 [parent (and (pair? ancestors) (car ancestors))]
                 [ancestors (if (and (html-ancestor? parent "template")
                                     (not (ancestor-first-child parent)))
                                (cons (make-ancestor 'html "template" (first-mode-setter children)
                                        (ancestor-location parent))
                                      (cdr ancestors))
                                ancestors)])
            (make-place namespace ancestors
              (if parent
                  (children-kind (ancestor-namespace parent) (ancestor-name parent)
                    (and (pair? (cdr ancestors)) (cadr ancestors)))
                  'normal)
              #f received-context #t))
          (let* ([elements (literal-elements children)]
                 [svg (filter svg-marked? elements)])
            (when (and (pair? svg) (< (length svg) (length elements)))
              (bad "the top of this form mixes HTML and SVG elements"
                   (element-tag (find (lambda (e) (not (svg-marked? e))) elements))))
            (cond
              [(pair? svg) (make-place 'svg '() 'top #f (list "svg") #f)]
              [(pair? elements)
               (make-place 'html '() 'top (first-mode-setter children) (list "html") #f)]
              [else (make-place 'html '() 'top #f #f #f)]))))

    (define (document? children)
      (and (not received-context)
           (pair? children)
           (null? (cdr children))
           (element? (car children))
           (string=? (element-name (car children)) "html")))

    (let ([seq (make-sequence)] [place (top-place)] [doc? (document? children)])
      (cond
        [doc? (compile-document-root! seq (car children) place)]
        [else (compile-children! seq children place)])
      (wrap-bindings (reverse (sequence-bindings seq))
        #`(#,(helper 'make-html-value)
            (lambda (port) #,@(let ([statements (sequence-statements seq)])
                                (if (null? statements) (list #'(void)) statements)))
            #,doc?)
        received-context)))

  )
