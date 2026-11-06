(library (support)
  (export evaluate render compile-error rejects? contains? condition-text raises-with?)
  (import (chezscheme) (haml))

  (define env (environment '(chezscheme) '(haml)))

  (define (evaluate datum) (eval datum env))

  (define (render datum) (html->string (evaluate datum)))

  (define (compile-error datum)
    (guard (c [(syntax-violation? c) (condition-message c)])
      (expand datum env)
      #f))

  (define (contains? s fragment)
    (let ([n (string-length s)] [m (string-length fragment)])
      (let loop ([i 0])
        (and (fx<= (fx+ i m) n)
             (or (string=? (substring s i (fx+ i m)) fragment)
                 (loop (fx+ i 1)))))))

  ;; Prints what it got when the check fails, since the check itself shows
  ;; only the datum.
  (define (rejects? datum fragment)
    (let* ([message (compile-error datum)]
           [ok? (and message (contains? message fragment))])
      (unless ok? (printf "  ~s gave ~s, expected ~s\n" datum message fragment))
      ok?))

  (define (condition-text c)
    (call-with-string-output-port (lambda (p) (display-condition c p))))

  (define (raises-with? who fragment)
    (lambda (c)
      (and (assertion-violation? c)
           (who-condition? c)
           (eq? (condition-who c) who)
           (contains? (condition-text c) fragment)))))
