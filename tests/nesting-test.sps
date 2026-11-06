(import (chezscheme) (check) (support) (html))

(define (accepted? datum)
  (let ([message (compile-error datum)])
    (when message (printf "  ~s gave ~s\n" datum message))
    (not message)))

(define (tag name) (string->symbol (string-append ":" name)))

(define (read-restructured-pairs file)
  (call-with-input-file file
    (lambda (in)
      (let loop ([pairs '()])
        (let ([line (get-line in)])
          (if (eof-object? line)
              pairs
              (let ([words (split-words line)])
                (loop (append (map (lambda (parent) (cons parent (car words))) (cddr words))
                              pairs)))))))))

(define (split-words s)
  (let loop ([chars (string->list s)] [word '()] [words '()])
    (define (flush) (if (null? word) words (cons (list->string (reverse word)) words)))
    (cond
      [(null? chars) (reverse (flush))]
      [(char=? (car chars) #\space) (loop (cdr chars) '() (flush))]
      [else (loop (cdr chars) (cons (car chars) word) words)])))

(define html-elements
  '("a" "abbr" "address" "area" "article" "aside" "audio" "b" "base" "bdi" "bdo" "blockquote"
    "br" "button" "canvas" "caption" "cite" "code" "col" "colgroup" "data" "datalist" "dd" "del"
    "details" "dfn" "dialog" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption" "figure"
    "footer" "form" "h1" "h2" "h3" "h4" "h5" "h6" "header" "hgroup" "hr" "i" "iframe" "img"
    "input" "ins" "kbd" "label" "legend" "li" "link" "main" "map" "mark" "menu" "meta" "meter"
    "nav" "noscript" "object" "ol" "optgroup" "option" "output" "p" "picture" "pre" "progress"
    "q" "rp" "rt" "ruby" "s" "samp" "script" "search" "section" "select" "selectedcontent"
    "slot" "small" "source" "span" "strong" "style" "sub" "summary" "sup" "table" "tbody" "td"
    "template" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "track" "u" "ul" "var"
    "video" "wbr"))

(define void-elements
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "source" "track" "wbr"))

(define text-elements '("script" "style" "title" "textarea" "iframe" "noscript"))

(test "the checker rejects exactly the pairs the parser restructures"
  (let ([restructured (read-restructured-pairs "tests/data/restructured-pairs.txt")]
        [parents (filter (lambda (p) (not (or (member p void-elements) (member p text-elements))))
                         html-elements)])
    (check-equal? 10010 (* (length parents) (length html-elements)))
    (check-equal? 1468 (length restructured))
    (let ([wrong '()])
      (for-each
        (lambda (parent)
          (for-each
            (lambda (child)
              (let ([expected (or (and (member (cons parent child) restructured) #t)
                                  ;; Children of selectedcontent are replaced when an
                                  ;; option is selected, after parsing.
                                  (string=? parent "selectedcontent"))]
                    [rejected (and (compile-error `(html (,(tag parent) (,(tag child))))) #t)])
                (unless (eq? expected rejected)
                  (set! wrong (cons (list parent child expected rejected) wrong)))))
            html-elements))
        parents)
      (check-equal? '() (reverse wrong)))))

(test "a p is closed by a p closer in button scope"
  (check (rejects? '(html (:p (:div))) "div closes an open p"))
  (check (rejects? '(html (:p (:span (:em (:ul))))) "ul closes an open p"))
  (check (rejects? '(html (:p (:span (:table)))) "table closes an open p"))
  (check (accepted? '(html (:p (:button (:div))))))
  (check (accepted? '(html (:p (:object (:div))))))
  (check (accepted? '(html (:p (:svg (:foreignObject (:div))))))))

(test "a stops only at markers, so it reaches through SVG integration points"
  (check (rejects? '(html (:a (:span (:a)))) "a cannot be inside another a"))
  (check (rejects? '(html (:a (:svg (:foreignObject (:a "in"))) "after")) "a cannot be inside another a"))
  (check (rejects? '(html (:a (:div (:p (:a))))) "a cannot be inside another a"))
  (check (accepted? '(html (:a (:object (:a))))))
  (check (accepted? '(html (:a (:template (:a))))))
  (check (accepted? '(html (:a (:table (:tbody (:tr (:td (:a)))))))))
  (check (accepted? '(html (:a (:svg (:a "svg a")) "after")))))

(test "form cannot be inside another form unless a template is open"
  (check (rejects? '(html (:form (:div (:form)))) "form cannot be inside another form"))
  (check (rejects? '(html (:form (:svg (:foreignObject (:form (:input)))))) "form cannot be inside another form"))
  (check (rejects? '(html (:form (:object (:form)))) "form cannot be inside another form"))
  (check (accepted? '(html (:form (:template (:form))))))
  (check (accepted? '(html (:template (:form (:div (:form))))))))

(test "button, li, dd and dt do not nest"
  (check (rejects? '(html (:button (:span (:button)))) "button cannot be inside another button"))
  (check (accepted? '(html (:button (:select (:button "x"))))))
  (check (rejects? '(html (:li (:li))) "li cannot be inside another li"))
  (check (rejects? '(html (:li (:div (:li)))) "li cannot be inside another li"))
  (check (rejects? '(html (:li (:span (:li)))) "li cannot be inside another li"))
  (check (accepted? '(html (:li (:ul (:li))))))
  (check (accepted? '(html (:li (:select (:option (:li "x")))))))
  (check (rejects? '(html (:dd (:dt))) "dt cannot be inside dd"))
  (check (rejects? '(html (:dt (:div (:dd)))) "dd cannot be inside dt"))
  (check (accepted? '(html (:dd (:dl (:dt)))))))

(test "a heading is not a child of a heading"
  (check (rejects? '(html (:h1 (:h2))) "h2 cannot be a child of h1"))
  (check (accepted? '(html (:h1 (:span (:h2)))))))

(test "option and optgroup are not children of option"
  (check (rejects? '(html (:datalist (:option (:option)))) "option cannot be a child of option"))
  (check (rejects? '(html (:datalist (:option (:optgroup)))) "optgroup cannot be a child of option"))
  (check (accepted? '(html (:optgroup (:optgroup))))))

(test "with a select in scope, option, optgroup and hr do not follow implied end tags"
  (check (rejects? '(html (:select (:p (:option "a")))) "with a select open, option cannot be a child of p"))
  (check (rejects? '(html (:select (:optgroup (:optgroup)))) "with a select open, optgroup cannot be a child of optgroup"))
  (check (rejects? '(html (:select (:option "a" (:hr)))) "with a select open, hr cannot be a child of option"))
  (check (rejects? '(html (:select (:optgroup (:option "a") (:hr)))) "with a select open, hr cannot be a child of optgroup"))
  (check (rejects? '(html (:select (:div (:li (:option "a"))))) "with a select open, option cannot be a child of li"))
  (check (rejects? '(html (:select (:div (:input)))) "input cannot be inside select"))
  (check (rejects? '(html (:select (:div (:select)))) "select cannot be inside select"))
  (check (accepted? '(html (:select (:optgroup (:option "a"))))))
  (check (accepted? '(html (:p (:select (:option (:div "x")))))))
  (check (accepted? '(html (:p (:select (:hr))))))
  (check (accepted? '(html (:select (:option (:span (:option "x")))))))
  (check (accepted? '(html (:select (:button (:selectedcontent)) (:option "a"))))))

(test "with a ruby in scope, rt and rp do not follow implied end tags"
  (check (rejects? '(html (:ruby "a" (:rt "b" (:rp "c")))) "with a ruby open, rp cannot be a child of rt"))
  (check (rejects? '(html (:ruby (:rp "(" (:rt "x")))) "with a ruby open, rt cannot be a child of rp"))
  (check (rejects? '(html (:ruby (:span (:p "x" (:rt "y"))))) "with a ruby open, rt cannot be a child of p"))
  (check (rejects? '(html (:ruby (:rt (:rt)))) "with a ruby open, rt cannot be a child of rt"))
  (check (accepted? '(html (:p (:rt (:rp))))))
  (check (accepted? '(html (:ruby (:rt (:span (:rt "x")))))))
  (check (accepted? '(html (:ruby "a" (:rp "(") (:rt "b") (:rp ")"))))))

(test "table structure"
  (check (rejects? '(html (:table (:tr))) "tr cannot be a child of table"))
  (check (rejects? '(html (:table (:td))) "td cannot be a child of table"))
  (check (rejects? '(html (:tbody (:td))) "td cannot be a child of tbody"))
  (check (rejects? '(html (:tr (:div))) "div cannot be a child of tr"))
  (check (rejects? '(html (:table (:div))) "div cannot be a child of table"))
  (check (rejects? '(html (:div (:td))) "td cannot be a child of div"))
  (check (rejects? '(html (:table (:colgroup (:template) (:span)))) "span cannot be a child of colgroup"))
  (check (accepted? '(html (:table (:caption "c") (:colgroup (:col)) (:thead (:tr (:th "h")))
                                  (:tbody (:tr (:td "d"))) (:tfoot (:tr (:td "f")))))))
  (check (accepted? '(html (:table (:tbody (:tr (:td (:table (:tbody (:tr (:td "x")))))))))))
  (check (accepted? '(html (:table (:script "x") (:style "y") (:template (:tr)) (:form))))))

(test "a template's first literal element sets how its children are checked"
  (check (rejects? '(html (:template (:td "a") (:tr (:td "b")))) "tr cannot be a child of a template whose first literal element child is td"))
  (check (rejects? '(html (:template (:div) (:tr (:td "b")))) "tr cannot be a child of a template whose first literal element child is div"))
  (check (rejects? '(html (:template (:tr) (:td "b"))) "td cannot be a child of a template whose first literal element child is tr"))
  (check (rejects? '(html (:template (:col) (:div "x"))) "div cannot be a child of a template whose first literal element child is col"))
  (check (rejects? '(html (:template (:tbody) (:tr))) "tr cannot be a child of a template whose first literal element child is tbody"))
  (check (rejects? '(html (:template (:col) "x")) "text in template would be moved"))
  (check (accepted? '(html (:template (:tr (:td "a")) (:div "x") "y"))))
  (check (accepted? '(html (:template (:tr (:td "a"))))))
  (check (accepted? '(html (:table (:tbody (:template (:tr (:td "a"))))))))
  (check (accepted? '(html (:template (:meta) (:td "a") (:th "b")))))
  (check (accepted? '(html (:template (:col) (:col) (:template (:div)))))))

(test "the template rule looks into control forms and skips expressions"
  (check (accepted? '(let ([c #t]) (html (:template (if c (:tr (:td "a")) (:div "d")) (:div "x"))))))
  (check (rejects? '(let ([c #t]) (html (:template (when c (:tr)) (:td "x"))))
           "td cannot be a child of a template whose first literal element child is tr"))
  (check (rejects? '(let ([c #t]) (html (:template (cond [c (:col)]) (:div))))
           "div cannot be a child of a template whose first literal element child is col"))
  (check (accepted? '(let ([rows (list (html (:tr (:td "a"))))]) (html (:template rows (:td "x")))))))

(test "a table or form in a template of table parts is dropped"
  (check (rejects? '(html (:template (:tr) (:table))) "table cannot be inside a template whose first child is tr"))
  (check (rejects? '(html (:template (:thead) (:form))) "form cannot be inside a template whose first child is thead"))
  (check (rejects? '(html (:template (:tr) (:div (:form (:p))))) "form cannot be inside a template whose first child is tr"))
  (check (accepted? '(html (:template (:tr (:td (:form)))))))
  (check (accepted? '(html (:template (:tr (:td (:table))))))))

(test "at a form's top, table parts admit only what a table context keeps"
  (check (accepted? '(html (:tr (:td "a")) (:tr (:td "b")))))
  (check (accepted? '(html (:td "a") (:th "b"))))
  (check (rejects? '(html (:tr) (:td)) "td cannot be at the top of a form after tr"))
  (check (rejects? '(html (:tr) (:div)) "div cannot be at the top of a form after tr"))
  (check (rejects? '(html (:tr) "x") "text at the top of a form of table parts"))
  (check (accepted? '(html (:tr) " " (:tr))))
  (check (accepted? '(html (:div) (:p) "x"))))

(test "an empty form in a table is kept, except while a template is open"
  (check (accepted? '(html (:table (:form)))))
  (check (accepted? '(html (:table (:tbody (:tr (:form)))))))
  (check (rejects? '(html (:template (:table (:form)))) "form cannot be a child of table"))
  (check (rejects? '(html (:template (:div (:table (:tbody (:tr (:form)))))))
           "form cannot be a child of tr"))
  (check (rejects? '(html (:template (:table (:tbody (:tr (:td (:table (:form))))))))
           "form cannot be a child of table"))
  (check (rejects? '(html (:table (:tbody (html (:template (:table (:form))))))) "form cannot be a child of table")))

(test "noscript does not nest"
  (check (rejects? '(html (:noscript (:div (:noscript)))) "noscript cannot be inside noscript")))

(test-exit)
