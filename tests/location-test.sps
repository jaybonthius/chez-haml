(import (chezscheme) (check) (support) (html))

(define env (environment '(chezscheme) '(html)))

(define (load-fixture file)
  (let ([value #f])
    (load (string-append "tests/fixtures/" file) (lambda (x) (set! value (eval x env))))
    value))

(define (compile-error-text file)
  (guard (c [(syntax-violation? c) (condition-text c)])
    (load-fixture file)
    #f))

(define (has? text fragment)
  (unless (and text (contains? text fragment))
    (printf "  ~s does not contain ~s\n" text fragment))
  (and text (contains? text fragment)))

(test "a compile-time error reports the offending piece's line"
  (let ([text (compile-error-text "nesting-error.ss")])
    (check (has? text "h2 closes an open p"))
    (check (has? text "at line 4, char 8 of tests/fixtures/nesting-error.ss")))
  (let ([text (compile-error-text "parse-error.ss")])
    (check (has? text "child literal must be a string or an exact integer"))
    (check (has? text "at line 5, char 12 of tests/fixtures/parse-error.ss")))
  (let ([text (compile-error-text "attribute-error.ss")])
    (check (has? text "unknown attribute name formatter"))
    (check (has? text "at line 4, char 14 of tests/fixtures/attribute-error.ss"))))

(test "a nesting error names the parent's location"
  (check (has? (compile-error-text "nesting-error.ss")
           "(the p at line 3, char 6 of tests/fixtures/nesting-error.ss)")))

(test "a value error reports the failing expression's line"
  (let ([c (guard (c [#t c]) ((load-fixture "value-error.ss") (cons 1 'oops)))])
    (check (assertion-violation? c))
    (check (has? (condition-text c) "at line 5, char 9 of tests/fixtures/value-error.ss"))))

(test "a value error in a compiled program reports its line"
  (let ([c (guard (c [#t c]) (let ([v 'bad]) (html (:p v))))])
    (check (has? (condition-text c) "at line 41, char 56 of tests/location-test.sps"))))

(test-exit)
