(import (chezscheme) (check) (support) (haml))

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
                    [rejected (and (compile-error `(haml (,(tag parent) (,(tag child))))) #t)])
                (unless (eq? expected rejected)
                  (set! wrong (cons (list parent child expected rejected) wrong)))))
            html-elements))
        parents)
      (check-equal? '() (reverse wrong)))))

(test "a p is closed by a p closer in button scope"
  (check (rejects? '(haml (:p (:div))) "div closes an open p"))
  (check (rejects? '(haml (:p (:span (:em (:ul))))) "ul closes an open p"))
  (check (rejects? '(haml (:p (:span (:table)))) "table closes an open p"))
  (check (accepted? '(haml (:p (:button (:div))))))
  (check (accepted? '(haml (:p (:object (:div))))))
  (check (accepted? '(haml (:p (:svg (:foreignObject (:div))))))))

(test "a stops only at markers, so it reaches through SVG integration points"
  (check (rejects? '(haml (:a (:span (:a)))) "a cannot be inside another a"))
  (check (rejects? '(haml (:a (:svg (:foreignObject (:a "in"))) "after")) "a cannot be inside another a"))
  (check (rejects? '(haml (:a (:div (:p (:a))))) "a cannot be inside another a"))
  (check (accepted? '(haml (:a (:object (:a))))))
  (check (accepted? '(haml (:a (:template (:a))))))
  (check (accepted? '(haml (:a (:table (:tbody (:tr (:td (:a)))))))))
  (check (accepted? '(haml (:a (:svg (:a "svg a")) "after")))))

(test "form cannot be inside another form unless a template is open"
  (check (rejects? '(haml (:form (:div (:form)))) "form cannot be inside another form"))
  (check (rejects? '(haml (:form (:svg (:foreignObject (:form (:input)))))) "form cannot be inside another form"))
  (check (rejects? '(haml (:form (:object (:form)))) "form cannot be inside another form"))
  (check (accepted? '(haml (:form (:template (:form))))))
  (check (accepted? '(haml (:template (:form (:div (:form))))))))

(test "button, li, dd and dt do not nest"
  (check (rejects? '(haml (:button (:span (:button)))) "button cannot be inside another button"))
  (check (accepted? '(haml (:button (:select (:button "x"))))))
  (check (rejects? '(haml (:li (:li))) "li cannot be inside another li"))
  (check (rejects? '(haml (:li (:div (:li)))) "li cannot be inside another li"))
  (check (rejects? '(haml (:li (:span (:li)))) "li cannot be inside another li"))
  (check (accepted? '(haml (:li (:ul (:li))))))
  (check (accepted? '(haml (:li (:select (:option (:li "x")))))))
  (check (rejects? '(haml (:dd (:dt))) "dt cannot be inside dd"))
  (check (rejects? '(haml (:dt (:div (:dd)))) "dd cannot be inside dt"))
  (check (accepted? '(haml (:dd (:dl (:dt)))))))

(test "a heading is not a child of a heading"
  (check (rejects? '(haml (:h1 (:h2))) "h2 cannot be a child of h1"))
  (check (accepted? '(haml (:h1 (:span (:h2)))))))

(test "option and optgroup are not children of option"
  (check (rejects? '(haml (:datalist (:option (:option)))) "option cannot be a child of option"))
  (check (rejects? '(haml (:datalist (:option (:optgroup)))) "optgroup cannot be a child of option"))
  (check (accepted? '(haml (:optgroup (:optgroup))))))

(test "with a select in scope, option, optgroup and hr do not follow implied end tags"
  (check (rejects? '(haml (:select (:p (:option "a")))) "with a select open, option cannot be a child of p"))
  (check (rejects? '(haml (:select (:optgroup (:optgroup)))) "with a select open, optgroup cannot be a child of optgroup"))
  (check (rejects? '(haml (:select (:option "a" (:hr)))) "with a select open, hr cannot be a child of option"))
  (check (rejects? '(haml (:select (:optgroup (:option "a") (:hr)))) "with a select open, hr cannot be a child of optgroup"))
  (check (rejects? '(haml (:select (:div (:li (:option "a"))))) "with a select open, option cannot be a child of li"))
  (check (rejects? '(haml (:select (:div (:input)))) "input cannot be inside select"))
  (check (rejects? '(haml (:select (:div (:select)))) "select cannot be inside select"))
  (check (accepted? '(haml (:select (:optgroup (:option "a"))))))
  (check (accepted? '(haml (:p (:select (:option (:div "x")))))))
  (check (accepted? '(haml (:p (:select (:hr))))))
  (check (accepted? '(haml (:select (:option (:span (:option "x")))))))
  (check (accepted? '(haml (:select (:button (:selectedcontent)) (:option "a"))))))

(test "with a ruby in scope, rt and rp do not follow implied end tags"
  (check (rejects? '(haml (:ruby "a" (:rt "b" (:rp "c")))) "with a ruby open, rp cannot be a child of rt"))
  (check (rejects? '(haml (:ruby (:rp "(" (:rt "x")))) "with a ruby open, rt cannot be a child of rp"))
  (check (rejects? '(haml (:ruby (:span (:p "x" (:rt "y"))))) "with a ruby open, rt cannot be a child of p"))
  (check (rejects? '(haml (:ruby (:rt (:rt)))) "with a ruby open, rt cannot be a child of rt"))
  (check (accepted? '(haml (:p (:rt (:rp))))))
  (check (accepted? '(haml (:ruby (:rt (:span (:rt "x")))))))
  (check (accepted? '(haml (:ruby "a" (:rp "(") (:rt "b") (:rp ")"))))))

(test "table structure"
  (check (rejects? '(haml (:table (:tr))) "tr cannot be a child of table"))
  (check (rejects? '(haml (:table (:td))) "td cannot be a child of table"))
  (check (rejects? '(haml (:tbody (:td))) "td cannot be a child of tbody"))
  (check (rejects? '(haml (:tr (:div))) "div cannot be a child of tr"))
  (check (rejects? '(haml (:table (:div))) "div cannot be a child of table"))
  (check (rejects? '(haml (:div (:td))) "td cannot be a child of div"))
  (check (rejects? '(haml (:table (:colgroup (:template) (:span)))) "span cannot be a child of colgroup"))
  (check (accepted? '(haml (:table (:caption "c") (:colgroup (:col)) (:thead (:tr (:th "h")))
                                  (:tbody (:tr (:td "d"))) (:tfoot (:tr (:td "f")))))))
  (check (accepted? '(haml (:table (:tbody (:tr (:td (:table (:tbody (:tr (:td "x")))))))))))
  (check (accepted? '(haml (:table (:script "x") (:style "y") (:template (:tr)) (:form))))))

(test "a template's first literal element sets how its children are checked"
  (check (rejects? '(haml (:template (:td "a") (:tr (:td "b")))) "tr cannot be a child of a template whose first literal element child is td"))
  (check (rejects? '(haml (:template (:div) (:tr (:td "b")))) "tr cannot be a child of a template whose first literal element child is div"))
  (check (rejects? '(haml (:template (:tr) (:td "b"))) "td cannot be a child of a template whose first literal element child is tr"))
  (check (rejects? '(haml (:template (:col) (:div "x"))) "div cannot be a child of a template whose first literal element child is col"))
  (check (rejects? '(haml (:template (:tbody) (:tr))) "tr cannot be a child of a template whose first literal element child is tbody"))
  (check (rejects? '(haml (:template (:col) "x")) "text in template would be moved"))
  (check (accepted? '(haml (:template (:tr (:td "a")) (:div "x") "y"))))
  (check (accepted? '(haml (:template (:tr (:td "a"))))))
  (check (accepted? '(haml (:table (:tbody (:template (:tr (:td "a"))))))))
  (check (accepted? '(haml (:template (:meta) (:td "a") (:th "b")))))
  (check (accepted? '(haml (:template (:col) (:col) (:template (:div)))))))

(test "the template rule looks into control forms and skips expressions"
  (check (accepted? '(let ([c #t]) (haml (:template (if c (:tr (:td "a")) (:div "d")) (:div "x"))))))
  (check (rejects? '(let ([c #t]) (haml (:template (when c (:tr)) (:td "x"))))
           "td cannot be a child of a template whose first literal element child is tr"))
  (check (rejects? '(let ([c #t]) (haml (:template (cond [c (:col)]) (:div))))
           "div cannot be a child of a template whose first literal element child is col"))
  (check (accepted? '(let ([rows (list (haml (:tr (:td "a"))))]) (haml (:template rows (:td "x")))))))

(test "a table or form in a template of table parts is dropped"
  (check (rejects? '(haml (:template (:tr) (:table))) "table cannot be inside a template whose first child is tr"))
  (check (rejects? '(haml (:template (:thead) (:form))) "form cannot be inside a template whose first child is thead"))
  (check (rejects? '(haml (:template (:tr) (:div (:form (:p))))) "form cannot be inside a template whose first child is tr"))
  (check (accepted? '(haml (:template (:tr (:td (:form)))))))
  (check (accepted? '(haml (:template (:tr (:td (:table))))))))

(test "at a form's top, table parts admit only what a table context keeps"
  (check (accepted? '(haml (:tr (:td "a")) (:tr (:td "b")))))
  (check (accepted? '(haml (:td "a") (:th "b"))))
  (check (rejects? '(haml (:tr) (:td)) "td cannot be at the top of a form after tr"))
  (check (rejects? '(haml (:tr) (:div)) "div cannot be at the top of a form after tr"))
  (check (rejects? '(haml (:tr) "x") "text at the top of a form of table parts"))
  (check (accepted? '(haml (:tr) " " (:tr))))
  (check (accepted? '(haml (:div) (:p) "x"))))

(test "an empty form in a table is kept, except while a template is open"
  (check (accepted? '(haml (:table (:form)))))
  (check (accepted? '(haml (:table (:tbody (:tr (:form)))))))
  (check (rejects? '(haml (:template (:table (:form)))) "form cannot be a child of table"))
  (check (rejects? '(haml (:template (:div (:table (:tbody (:tr (:form)))))))
           "form cannot be a child of tr"))
  (check (rejects? '(haml (:template (:table (:tbody (:tr (:td (:table (:form))))))))
           "form cannot be a child of table"))
  (check (rejects? '(haml (:table (:tbody (haml (:template (:table (:form))))))) "form cannot be a child of table")))

(test "noscript does not nest"
  (check (rejects? '(haml (:noscript (:div (:noscript)))) "noscript cannot be inside noscript")))

(test-exit)
