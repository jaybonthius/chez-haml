(import (chezscheme) (check) (support) (haml))

(define (top . nodes) (cons '*TOP* nodes))

(test "elements, text and attributes read back as SXML"
  (check-equal? (top '(p "one") '(p (\x40; (class "a b") (id "x")) "two" (em "three")) "after")
    (html->sxml (haml (:p "one") (:p.a.b ([id "x"]) "two" (:em "three")) "after"))))

(test "every escape in text and attribute values reads back as the character written"
  (let ([text "a & b < c > d \"e\" 'f'"])
    (check-equal? (top `(p (\x40; (title ,text)) ,text))
      (html->sxml (haml (:p ([title text]) text))))))

(test "a bare attribute reads as the empty string"
  (check-equal? (top '(input (\x40; (type "checkbox") (checked "") (data-bind:done ""))))
    (html->sxml (haml (:input ([type "checkbox"] [checked] [data-bind:done]))))))

(test "attribute names that are not identifiers read back"
  (check-equal? (top '(button (\x40; (\x40;click "open = true") (hx-post "/save")) "Save"))
    (html->sxml (haml (:button (["@click" "open = true"] ["hx-post" "/save"]) "Save")))))

(test "void elements end without an end tag, and what follows is their sibling"
  (check-equal? (top '(div (br) (img (\x40; (src "a.png"))) (input (\x40; (value ""))) "after"))
    (html->sxml (haml (:div (:br) (:img ([src "a.png"])) (:input ([value ""])) "after")))))

(test "script and style text reads back as written"
  (check-equal? (top '(script "if (a < b && c > d) {}") '(style "p > em { color: red }") '(script))
    (html->sxml (haml (:script "if (a < b && c > d) {}") (:style "p > em { color: red }") (:script)))))

(test "the newline written after pre's and textarea's start tags is dropped"
  (check-equal? (top '(pre "\nx") '(textarea "y") '(pre))
    (html->sxml (haml (:pre "\nx") (:textarea "y") (:pre)))))

(test "in SVG elements keep their case, and script text is escaped as any other"
  (check-equal? (top '(svg (\x40; (viewBox "0 0 10 10")) (linearGradient (stop)) (script "a < b")
                        (foreignObject (p "x"))))
    (html->sxml (haml (:svg ([viewBox "0 0 10 10"]) (:linearGradient (:stop)) (:script "a < b")
                        (:foreignObject (:p "x")))))))

(test "a document reads with its doctype"
  (check-equal? (top '(*DECL* DOCTYPE html) '(html (\x40; (lang "en")) (head (title "a < b")) (body)))
    (html->sxml (haml (:html ([lang "en"]) (:head (:title "a < b")) (:body))))))

(test "raw-html that follows the writer's rules reads, and a string or bytevector reads as html does"
  (check-equal? (top '(p (span (\x40; (class "x")) "in")))
    (html->sxml (haml (:p (raw-html "<span class=\"x\">in</span>")))))
  (check-equal? (top '(p "a")) (html->sxml "<p>a</p>"))
  (check-equal? (top '(p "é")) (html->sxml (string->utf8 "<p>é</p>"))))

(test "markup the writer does not write is refused"
  (for-each
    (lambda (markup) (check-raises (raises-with? 'html->sxml "at index") (html->sxml markup)))
    '("<p>a&nbsp;b</p>" "<p>a > b</p>" "<!-- note --><p></p>" "<p>open" "<p class='x'></p>" "</p>"
      "<p></q>" "<br/>" "<p  class=\"x\"></p>" "<script>never closed" "<p>x</p><!DOCTYPE html>"
      "<p title=\"a > b\"></p>" "<p title=\"open></p>")))

(test "html->sxml checks its argument"
  (check-raises (raises-with? 'html->sxml "not html, a string or a bytevector") (html->sxml 'p)))

(test-exit)
