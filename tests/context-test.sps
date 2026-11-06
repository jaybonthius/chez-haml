(import (chezscheme) (check) (support) (haml))

(define (accepted? datum)
  (let ([message (compile-error datum)])
    (when message (printf "  ~s gave ~s\n" datum message))
    (not message)))

(test "a form written in a child expression is checked as the element's child"
  (check (rejects? '(haml (:p (haml (:div)))) "div closes an open p"))
  (check (rejects? '(haml (:p (list (haml (:div))))) "div closes an open p"))
  (check (rejects? '(haml (:table (map (lambda (r) (haml (:tr))) '(1)))) "tr cannot be a child of table"))
  (check (rejects? '(haml (:select (:optgroup (map (lambda (x) (haml (:hr))) '(1)))))
           "with a select open, hr cannot be a child of optgroup"))
  (check (rejects? '(haml (:a (:span (haml (:div (:a)))))) "a cannot be inside another a"))
  (check-equal? "<table><tbody><tr><td>1</td></tr><tr><td>2</td></tr></tbody></table>"
    (html->string
      (haml (:table (:tbody (map (lambda (r) (haml (:tr (:td r)))) (list 1 2))))))))

(test "the error says to build the form outside the element"
  (check (rejects? '(haml (:p (haml (:div)))) "build it outside the element")))

(test "the context reaches through the forms nested in between"
  (check (rejects? '(haml (:ul (haml (:li (haml (:li)))))) "li cannot be inside another li"))
  (check (rejects? '(haml (:p (haml (:span (haml (:div)))))) "div closes an open p")))

(test "let and let* inits and bodies get the element's context"
  (check (rejects? '(haml (:p (let ([d (haml (:div))]) d))) "div closes an open p"))
  (check (rejects? '(haml (:p (let* ([d (haml (:div))]) d))) "div closes an open p"))
  (check (rejects? '(haml (:p (let ([x 1]) (haml (:div))))) "div closes an open p")))

(test "tests, case keys, for-each lists and attribute values get the top"
  (check (accepted? '(haml (:p (when (html? (haml (:div))) "x")))))
  (check (accepted? '(haml (:p (if (html? (haml (:div))) "x" "y")))))
  (check (accepted? '(haml (:p (cond [(html? (haml (:div))) "x"])))))
  (check (accepted? '(haml (:p (case (html? (haml (:div))) [(#t) "x"])))))
  (check (accepted? '(haml (:p (for-each (lambda (x) (:b)) (list (haml (:div))))))))
  (check (accepted? '(haml (:p ([title (html->string (haml (:div)))]) "x"))))
  (check (accepted? '(haml (:p ([title "a" (html->string (haml (:div)))]) "x"))))
  (check (accepted? '(haml (:p ([class (html->string (haml (:div)))]) "x")))))

(test "a form inside a for-each body is checked where the body is written"
  (check (rejects? '(haml (:p (for-each (lambda (x) (haml (:div))) (list 1)))) "div closes an open p")))

(test "SVG context carries into nested forms"
  (check-equal? "<svg><circle></circle></svg>" (html->string (haml (:svg (haml (:circle))))))
  (check (rejects? '(haml (:svg (haml (:div)))) "div is not an SVG 2 element"))
  (check-equal? "<svg><script>x = a&lt;b ? 1 : 2</script><circle></circle></svg>"
    (html->string (haml (:svg (let ([s (haml (:script "x = a<b ? 1 : 2"))]) s) (:circle)))))
  (check (rejects? '(haml (:svg (haml (:title "a" (:b "c")) (:a (:div "d")))))
           "div is not an SVG 2 element"))
  (check-equal? "<svg><foreignObject><div></div></foreignObject></svg>"
    (html->string (haml (:svg (:foreignObject (haml (:div))))))))

(test "expressions at a form's top get the context the top was checked in"
  (check (accepted? '(haml (:circle) (list (haml (:rect))))))
  (check (rejects? '(haml (:circle) (list (haml (:div)))) "div is not an SVG 2 element"))
  (check (accepted? '(haml (:div) (list (haml (:p))))))
  (check (rejects? '(haml (:div) (list (haml (:circle)))) "circle is an SVG element")))

(test "raw text and text-only elements refuse nested forms"
  (check (rejects? '(haml (:title (haml (:b)))) "holds only text"))
  (check (rejects? '(haml (:script (html->string (haml (:b))))) "holds only text")))

(test "a template's mode carries into nested forms"
  (check (rejects? '(haml (:template (:tr) (haml (:td)))) "td cannot be a child of a template whose first literal element child is tr"))
  (check (accepted? '(haml (:template (haml (:td)) (haml (:th))))))
  (check (accepted? '(haml (:template (:tr) (haml (:tr))))))
  (check (rejects? '(haml (:template (haml (:tr (:td "x"))) (:div)))
           "tr cannot be a child of a template whose first literal element child is div")))

(test "sibling forms nested in a template are checked independently"
  (check (accepted? '(haml (:template (haml (:div)) (haml (:tr (:td "x"))))))))

(test "documents in nested forms are refused"
  (check (rejects? '(haml (:div (haml (:html (:body))))) "(:html …) must be the only child"))
  (check (rejects? '(haml (:html (:body (haml (:head))))) "head can be only at the top of a form or inside html")))

(test "a form passed to a procedure is still checked where it is written"
  (check (rejects? '(haml (:table (:tbody (let ([wrap (lambda (td) (haml (:tr td)))])
                                            (wrap (haml (:td)))))))
           "td cannot be a child of tbody")))

(test "a component procedure defined elsewhere is checked where it is defined"
  (let ([cell (lambda (x) (haml (:td x)))])
    (check-equal? "<table><tbody><tr><td>1</td></tr></tbody></table>"
      (html->string (haml (:table (:tbody (:tr (cell 1)))))))))

(test-exit)
