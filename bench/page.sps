(import (chezscheme) (html))

(define (page items)
  (html
    (:html ([lang "en"])
      (:head (:title "SXML benchmark"))
      (:body.page
        (:h1 "Benchmark")
        (map (lambda (i)
               (html
                 (:article.row ([data-index i])
                   (:h2 "Item " i)
                   (:p "The quick brown fox jumps over the lazy dog.")
                   (:a ([href "/items/" i]) "details"))))
             items)))))

(define (link-view s) (html (:a ([href "/c/" s]) s)))
(define (cell-view s) (html (:td.cell ([title s]) (link-view s))))
(define (row-view r) (html (:tr ([data-id (car r)]) (map cell-view (cdr r)))))
(define (card-view c)
  (html (:div.card (:h3 (car c)) (:table (map row-view (cdr c))))))
(define (section-view s) (html (:section (:h2 (car s)) (map card-view (cdr s)))))
(define (deep-page sections) (html (:html (:body (:main (map section-view sections))))))

(define items (iota 50))

(define sections
  (map (lambda (s)
         (cons (format "Section ~a & co" s)
               (map (lambda (c)
                      (cons (format "Card ~a.~a" s c)
                            (map (lambda (r)
                                   (cons (+ (* 100 s) (* 10 c) r)
                                         (map (lambda (k) (format "c-~a-~a-~a-~a" s c r k)) (iota 3))))
                                 (iota 4))))
                    (iota 3))))
       (iota 4)))

(define (measure render n)
  (render)
  (collect (collect-maximum-generation))
  (let ([bytes-before (sstats-bytes (statistics))]
        [start (current-time 'time-monotonic)])
    (do ([i 0 (fx+ i 1)]) ((fx= i n)) (render))
    (let* ([d (time-difference (current-time 'time-monotonic) start)]
           [seconds (+ (time-second d) (/ (time-nanosecond d) 1e9))])
      (cons (* 1e6 (/ seconds n))
            (/ (- (sstats-bytes (statistics)) bytes-before) n 1024.)))))

(define (median-of-3 xs) (cadr (sort < xs)))

(define (report name render n)
  (let ([runs (map (lambda (round) (measure render n)) '(1 2 3))])
    (printf "~a: ~a bytes, ~,2f us and ~,1f KB per render (median of 3; runs ~{~,2f ~}us)\n"
      name (bytevector-length (render))
      (median-of-3 (map car runs)) (median-of-3 (map cdr runs)) (map car runs))))

(report "page" (lambda () (html->bytevector (page items))) 50000)
(report "deep" (lambda () (html->bytevector (deep-page sections))) 20000)
