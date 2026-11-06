(import (chezscheme) (check) (support) (haml))

(define (page items)
  (haml
    (:html ([lang "en"])
      (:head (:title "SXML benchmark"))
      (:body.page
        (:h1 "Benchmark")
        (map (lambda (i)
               (haml
                 (:article.row ([data-index i])
                   (:h2 "Item " i)
                   (:p "The quick brown fox jumps over the lazy dog.")
                   (:a ([href "/items/" i]) "details"))))
             items)))))

(define (file-bytes file)
  (call-with-port (open-file-input-port file) get-bytevector-all))

(test "the 50-row page renders as expected"
  (check-equal? (file-bytes "tests/data/page.html") (html->bytevector (page (iota 50)))))

(test-exit)
