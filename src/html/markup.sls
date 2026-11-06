(library (html markup)
  (export
    make-element element? element-form element-tag element-name element-classes
    element-attributes element-children
    make-attribute attribute? attribute-form attribute-name-syntax attribute-name
    attribute-parts
    make-text-literal text-literal? text-literal-form text-literal-string
    make-integer-literal integer-literal? integer-literal-form integer-literal-value
    make-expression expression? expression-form
    make-when-form when-form? when-form-form when-form-test when-form-children
    make-unless-form unless-form? unless-form-form unless-form-test unless-form-children
    make-if-form if-form? if-form-form if-form-test if-form-then if-form-else
    make-begin-form begin-form? begin-form-form begin-form-children
    make-cond-form cond-form? cond-form-form cond-form-clauses
    make-cond-clause cond-clause-test cond-clause-children
    make-case-form case-form? case-form-form case-form-keyword case-form-key case-form-clauses
    make-case-clause case-clause-data case-clause-children
    make-let-form let-form? let-form-form let-form-sequential? let-form-names let-form-inits
    let-form-children
    make-for-each-form for-each-form? for-each-form-form for-each-form-names
    for-each-form-lists for-each-form-children
    map-in-order)
  (import (chezscheme))

  ;; Chez's map applies its procedure in no fixed order, and the author's
  ;; expressions must be bound, and the first malformed piece reported, in
  ;; the order they are written.
  (define (map-in-order f l)
    (let loop ([l l])
      (cond
        [(null? l) '()]
        [else (let ([x (f (car l))]) (cons x (loop (cdr l))))])))

  ;; Every record keeps the syntax it came from, so that the compiler reports
  ;; a problem at the place the author wrote it.

  (define-record-type element
    (nongenerative) (sealed #t)
    (fields form tag name classes attributes children))

  (define-record-type attribute
    (nongenerative) (sealed #t)
    (fields form name-syntax name parts))

  (define-record-type text-literal
    (nongenerative) (sealed #t)
    (fields form string))

  (define-record-type integer-literal
    (nongenerative) (sealed #t)
    (fields form value))

  (define-record-type expression
    (nongenerative) (sealed #t)
    (fields form))

  (define-record-type when-form
    (nongenerative) (sealed #t)
    (fields form test children))

  (define-record-type unless-form
    (nongenerative) (sealed #t)
    (fields form test children))

  (define-record-type if-form
    (nongenerative) (sealed #t)
    (fields form test then else))

  (define-record-type begin-form
    (nongenerative) (sealed #t)
    (fields form children))

  (define-record-type cond-form
    (nongenerative) (sealed #t)
    (fields form clauses))

  (define-record-type cond-clause
    (nongenerative) (sealed #t)
    (fields form test children))

  ;; keyword is the author's own case identifier, since (rnrs) and
  ;; (chezscheme) bind case to forms that compare differently.
  (define-record-type case-form
    (nongenerative) (sealed #t)
    (fields form keyword key clauses))

  (define-record-type case-clause
    (nongenerative) (sealed #t)
    (fields form data children))

  (define-record-type let-form
    (nongenerative) (sealed #t)
    (fields form sequential? names inits children))

  (define-record-type for-each-form
    (nongenerative) (sealed #t)
    (fields form names lists children)))
