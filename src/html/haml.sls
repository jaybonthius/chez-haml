(library (html haml)
  (export parse-html)
  (import (chezscheme) (html markup))

  (define (parse-html form)
    (syntax-case form ()
      [(_ child ...) (parse-children form #'(child ...) '())]))

  (define (bad form message subform)
    (syntax-violation 'html message form subform))

  (define (colon-name? x)
    (and (identifier? x)
         (let ([s (symbol->string (syntax->datum x))])
           (and (fx> (string-length s) 0) (char=? (string-ref s 0) #\:)))))

  (define (exact-integer? d) (and (integer? d) (exact? d)))

  (define (split-tag-spec form id)
    (let* ([s (symbol->string (syntax->datum id))]
           [n (string-length s)]
           [pieces (let loop ([i 1] [start 1] [acc '()])
                     (cond
                       [(fx= i n) (reverse (cons (substring s start i) acc))]
                       [(char=? (string-ref s i) #\.)
                        (loop (fx+ i 1) (fx+ i 1) (cons (substring s start i) acc))]
                       [else (loop (fx+ i 1) start acc)]))])
      (when (string=? (car pieces) "")
        (bad form "missing tag name after :" id))
      (when (member "" (cdr pieces))
        (bad form "empty class name in tag" id))
      (values (car pieces) (cdr pieces))))

  (define (attribute-list? x)
    (syntax-case x ()
      [() #t]
      [(entry ...)
       (for-all (lambda (e) (syntax-case e () [(a ...) #t] [_ #f])) #'(entry ...))]
      [_ #f]))

  (define (parse-attribute form entry)
    (syntax-case entry ()
      [(name part ...)
       (let ([n (syntax->datum #'name)])
         (unless (or (identifier? #'name) (string? n))
           (bad form "attribute name must be an identifier or a string" #'name))
         (when (colon-name? #'name)
           (bad form "this reads as markup, not an attribute name; give an element its own parentheses, and write a name that starts with : as a string"
                #'name))
         (let ([name-string (if (string? n) n (symbol->string n))])
           (make-attribute entry #'name name-string
             (map-in-order (lambda (p) (parse-attribute-part form name-string p)) #'(part ...)))))]
      [_ (bad form "attribute must be [name part ...]" entry)]))

  (define (parse-attribute-part form name p)
    (syntax-case p (quote)
      [(quote d) (bad form "a quoted datum cannot be an attribute value" p)]
      [(h . t) (make-expression p)]
      [_ (let ([d (syntax->datum p)])
           (cond
             [(identifier? p) (make-expression p)]
             [(string? d) (make-text-literal p d)]
             [(exact-integer? d) (make-integer-literal p d)]
             [(boolean? d)
              (bad form
                (if (string=? name "class")
                    "a literal #t or #f is not an attribute value; write [class \"x\"], or leave it out"
                    "a literal #t or #f is not an attribute value; write [name], or leave the attribute out")
                p)]
             [else (bad form "attribute value literal must be a string or an exact integer" p)]))]))

  (define (parse-element form c binders)
    (syntax-case c ()
      [(tag attributes child ...)
       (attribute-list? #'attributes)
       (syntax-case #'attributes ()
         [(a ...) (make-element* form c #'tag #'(a ...) #'(child ...) binders)])]
      [(tag child ...)
       (make-element* form c #'tag '() #'(child ...) binders)]
      [_ (bad form "expected (:tag child ...) or (:tag (attribute ...) child ...)" c)]))

  (define (make-element* form c tag attributes children binders)
    (let-values ([(name classes) (split-tag-spec form tag)])
      (make-element c tag name classes
        (map-in-order (lambda (a) (parse-attribute form a)) attributes)
        (parse-children form children binders))))

  (define (parse-body form c children binders)
    (when (null? children)
      (bad form "expected at least one child" c))
    (parse-children form children binders))

  (define (parse-children form children binders)
    (map-in-order (lambda (c) (parse-child form c binders)) children))

  ;; A name the markup itself binds shadows the forms recognised here, as it
  ;; would in Scheme.
  (define (markup-bound? x binders)
    (and (identifier? x) (exists (lambda (b) (bound-identifier=? b x)) binders)))

  (define (parse-child form c binders)
    (syntax-case c ()
      [(h . rest)
       (cond
         [(markup-bound? #'h binders) (make-expression c)]
         [(colon-name? #'h) (parse-element form c binders)]
         [(string? (syntax->datum #'h))
          (bad form "a list cannot start with a string; is this an element missing its :tag?" c)]
         [else (parse-form form c binders)])]
      [_ (let ([d (syntax->datum c)])
           (cond
             [(colon-name? c) (bad form "an element is written in parentheses, (:tag ...)" c)]
             [(identifier? c) (make-expression c)]
             [(string? d) (make-text-literal c d)]
             [(exact-integer? d) (make-integer-literal c d)]
             [else (bad form "child literal must be a string or an exact integer" c)]))]))

  (define (parse-form form c binders)
    (define (body children) (parse-body form c children binders))
    (syntax-case c (quote define define-values define-record-type define-syntax
                    when unless if cond case r6rs:case let let* begin for-each)
      [(quote d) (bad form "a quoted datum cannot be a child" c)]
      [(def . rest)
       (and (identifier? #'def)
            (exists (lambda (k) (free-identifier=? #'def k))
              (list #'define #'define-values #'define-record-type #'define-syntax)))
       (bad form "a definition cannot be a child; bind the value with let, or define it outside the form" c)]
      [(when test child ...) (make-when-form c #'test (body #'(child ...)))]
      [(when . _) (bad form "expected (when test child ...)" c)]
      [(unless test child ...) (make-unless-form c #'test (body #'(child ...)))]
      [(unless . _) (bad form "expected (unless test child ...)" c)]
      [(if test consequent alternative)
       (make-if-form c #'test
         (parse-child form #'consequent binders)
         (parse-child form #'alternative binders))]
      [(if test consequent) (bad form "if needs both arms; use when or unless" c)]
      [(if . _) (bad form "expected (if test child child)" c)]
      [(cond clause ...) (make-cond-form c (parse-cond-clauses form c #'(clause ...) binders))]
      [(cond . _) (bad form "expected (cond [test child ...] ...)" c)]
      [(k key clause ...)
       (and (identifier? #'k)
            (or (free-identifier=? #'k #'case) (free-identifier=? #'k #'r6rs:case)))
       (make-case-form c #'k #'key (parse-case-clauses form c #'(clause ...) binders))]
      [(k . _)
       (and (identifier? #'k)
            (or (free-identifier=? #'k #'case) (free-identifier=? #'k #'r6rs:case)))
       (bad form "expected (case key [(datum ...) child ...] ...)" c)]
      [(let name . _) (identifier? #'name) (make-expression c)]
      [(let ([x e] ...) child ...)
       (for-all identifier? #'(x ...))
       (make-let-form c #f #'(x ...) #'(e ...)
         (parse-body form c #'(child ...) (append #'(x ...) binders)))]
      [(let . _) (bad form "expected (let ([name expression] ...) child ...)" c)]
      [(let* ([x e] ...) child ...)
       (for-all identifier? #'(x ...))
       (make-let-form c #t #'(x ...) #'(e ...)
         (parse-body form c #'(child ...) (append #'(x ...) binders)))]
      [(let* . _) (bad form "expected (let* ([name expression] ...) child ...)" c)]
      [(begin child ...) (make-begin-form c (parse-children form #'(child ...) binders))]
      [(begin . _) (bad form "expected (begin child ...)" c)]
      [(for-each (lam (x ...) child ...) l ...)
       (and (recognised? #'lam #'lambda binders)
            (for-all identifier? #'(x ...))
            (pair? #'(x ...))
            (fx= (length #'(x ...)) (length #'(l ...))))
       (make-for-each-form c #'(x ...) #'(l ...)
         (parse-body form c #'(child ...) (append #'(x ...) binders)))]
      [(for-each . _)
       (bad form "expected (for-each (lambda (name ...) child ...) list ...), one list per name" c)]
      [_ (make-expression c)]))

  (define (recognised? x keyword binders)
    (and (identifier? x) (not (markup-bound? x binders)) (free-identifier=? x keyword)))

  (define (parse-cond-clauses form c clauses binders)
    (let loop ([clauses clauses])
      (if (null? clauses)
          '()
          (let ([cl (car clauses)])
            (syntax-case cl ()
              [(test arrow f)
               (recognised? #'arrow #'=> binders)
               (bad form "=> is not allowed in markup" cl)]
              [(test child ...)
               (let ([else-clause? (recognised? #'test #'else binders)])
                 (when (and else-clause? (pair? (cdr clauses)))
                   (bad form "else must be the last clause" cl))
                 (cons (make-cond-clause cl (if else-clause? #f #'test)
                         (parse-body form cl #'(child ...) binders))
                       (loop (cdr clauses))))]
              [_ (bad form "expected [test child ...] in cond" cl)])))))

  (define (parse-case-clauses form c clauses binders)
    (let loop ([clauses clauses])
      (if (null? clauses)
          '()
          (let ([cl (car clauses)])
            (syntax-case cl ()
              [(e child ...)
               (recognised? #'e #'else binders)
               (begin
                 (when (pair? (cdr clauses)) (bad form "else must be the last clause" cl))
                 (cons (make-case-clause cl #'e (parse-body form cl #'(child ...) binders))
                       (loop (cdr clauses))))]
              [((d ...) child ...)
               (cons (make-case-clause cl #'(d ...) (parse-body form cl #'(child ...) binders))
                     (loop (cdr clauses)))]
              [_ (bad form "expected [(datum ...) child ...] in case" cl)]))))))
