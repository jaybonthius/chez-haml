(library (haml)
  (export haml html? write-html html->bytevector html->string raw-html)
  (import (chezscheme) (haml parse) (haml compile))

  ;; The record and writers share this library with the macro so that the
  ;; generated code reaches them without their being exported: an exported
  ;; constructor would let any program forge html values.
  (include "haml/write.ss")

  ;; Rebound around an element's expressions, so that a form nested
  ;; lexically there is checked as that element's child.
  (define-syntax html-context
    (syntax-rules ()
      [(_ k form) (k #f form)]))

  (define-syntax compile-in-context
    (lambda (x)
      (syntax-case x ()
        [(_ context form)
         (compile-html #'form (parse-haml #'form) (syntax->datum #'context) #'here #'haml)])))

  (define-syntax haml
    (lambda (x)
      (syntax-case x ()
        [(_ child ...) #`(html-context compile-in-context #,x)]))))
