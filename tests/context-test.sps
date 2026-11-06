(import (chezscheme) (check) (support) (html))

(define (accepted? datum)
  (let ([message (compile-error datum)])
    (when message (printf "  ~s gave ~s\n" datum message))
    (not message)))

(test "a form written in a child expression is checked as the element's child"
  (check (rejects? '(html (:p (html (:div)))) "div closes an open p"))
  (check (rejects? '(html (:p (list (html (:div))))) "div closes an open p"))
  (check (rejects? '(html (:table (map (lambda (r) (html (:tr))) '(1)))) "tr cannot be a child of table"))
  (check (rejects? '(html (:select (:optgroup (map (lambda (x) (html (:hr))) '(1)))))
           "with a select open, hr cannot be a child of optgroup"))
  (check (rejects? '(html (:a (:span (html (:div (:a)))))) "a cannot be inside another a"))
  (check-equal? "<table><tbody><tr><td>1</td></tr><tr><td>2</td></tr></tbody></table>"
    (html->string
      (html (:table (:tbody (map (lambda (r) (html (:tr (:td r)))) (list 1 2))))))))

(test "the error says to build the form outside the element"
  (check (rejects? '(html (:p (html (:div)))) "build it outside the element")))

(test "the context reaches through the forms nested in between"
  (check (rejects? '(html (:ul (html (:li (html (:li)))))) "li cannot be inside another li"))
  (check (rejects? '(html (:p (html (:span (html (:div)))))) "div closes an open p")))

(test "let and let* inits and bodies get the element's context"
  (check (rejects? '(html (:p (let ([d (html (:div))]) d))) "div closes an open p"))
  (check (rejects? '(html (:p (let* ([d (html (:div))]) d))) "div closes an open p"))
  (check (rejects? '(html (:p (let ([x 1]) (html (:div))))) "div closes an open p")))

(test "tests, case keys, for-each lists and attribute values get the top"
  (check (accepted? '(html (:p (when (html? (html (:div))) "x")))))
  (check (accepted? '(html (:p (if (html? (html (:div))) "x" "y")))))
  (check (accepted? '(html (:p (cond [(html? (html (:div))) "x"])))))
  (check (accepted? '(html (:p (case (html? (html (:div))) [(#t) "x"])))))
  (check (accepted? '(html (:p (for-each (lambda (x) (:b)) (list (html (:div))))))))
  (check (accepted? '(html (:p ([title (html->string (html (:div)))]) "x"))))
  (check (accepted? '(html (:p ([title "a" (html->string (html (:div)))]) "x"))))
  (check (accepted? '(html (:p ([class (html->string (html (:div)))]) "x")))))

(test "a form inside a for-each body is checked where the body is written"
  (check (rejects? '(html (:p (for-each (lambda (x) (html (:div))) (list 1)))) "div closes an open p")))

(test "SVG context carries into nested forms"
  (check-equal? "<svg><circle></circle></svg>" (html->string (html (:svg (html (:circle))))))
  (check (rejects? '(html (:svg (html (:div)))) "div is not an SVG 2 element"))
  (check-equal? "<svg><script>x = a&lt;b ? 1 : 2</script><circle></circle></svg>"
    (html->string (html (:svg (let ([s (html (:script "x = a<b ? 1 : 2"))]) s) (:circle)))))
  (check (rejects? '(html (:svg (html (:title "a" (:b "c")) (:a (:div "d")))))
           "div is not an SVG 2 element"))
  (check-equal? "<svg><foreignObject><div></div></foreignObject></svg>"
    (html->string (html (:svg (:foreignObject (html (:div))))))))

(test "expressions at a form's top get the context the top was checked in"
  (check (accepted? '(html (:circle) (list (html (:rect))))))
  (check (rejects? '(html (:circle) (list (html (:div)))) "div is not an SVG 2 element"))
  (check (accepted? '(html (:div) (list (html (:p))))))
  (check (rejects? '(html (:div) (list (html (:circle)))) "circle is an SVG element")))

(test "raw text and text-only elements refuse nested forms"
  (check (rejects? '(html (:title (html (:b)))) "holds only text"))
  (check (rejects? '(html (:script (html->string (html (:b))))) "holds only text")))

(test "a template's mode carries into nested forms"
  (check (rejects? '(html (:template (:tr) (html (:td)))) "td cannot be a child of a template whose first literal element child is tr"))
  (check (accepted? '(html (:template (html (:td)) (html (:th))))))
  (check (accepted? '(html (:template (:tr) (html (:tr))))))
  (check (rejects? '(html (:template (html (:tr (:td "x"))) (:div)))
           "tr cannot be a child of a template whose first literal element child is div")))

(test "sibling forms nested in a template are checked independently"
  (check (accepted? '(html (:template (html (:div)) (html (:tr (:td "x"))))))))

(test "documents in nested forms are refused"
  (check (rejects? '(html (:div (html (:html (:body))))) "(:html …) must be the only child"))
  (check (rejects? '(html (:html (:body (html (:head))))) "head can be only at the top of a form or inside html")))

(test "a form passed to a procedure is still checked where it is written"
  (check (rejects? '(html (:table (:tbody (let ([wrap (lambda (td) (html (:tr td)))])
                                            (wrap (html (:td)))))))
           "td cannot be a child of tbody")))

(test "a component procedure defined elsewhere is checked where it is defined"
  (let ([cell (lambda (x) (html (:td x)))])
    (check-equal? "<table><tbody><tr><td>1</td></tr></tbody></table>"
      (html->string (html (:table (:tbody (:tr (cell 1)))))))))

(test-exit)
