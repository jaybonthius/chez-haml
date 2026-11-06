(import (chezscheme) (check) (support) (haml))

(define (accepted? datum)
  (let ([message (compile-error datum)])
    (when message (printf "  ~s gave ~s\n" datum message))
    (not message)))

(test "svg switches to SVG context, where elements keep their case and have end tags"
  (check-equal? "<svg viewBox=\"0 0 10 10\"><circle cx=\"5\" cy=\"5\" r=\"4\"></circle><linearGradient gradientUnits=\"userSpaceOnUse\"><stop offset=\"0\"></stop></linearGradient></svg>"
    (html->string
      (haml (:svg ([viewBox "0 0 10 10"])
              (:circle ([cx 5] [cy 5] [r 4]))
              (:linearGradient ([gradientUnits "userSpaceOnUse"]) (:stop ([offset 0]))))))))

(test "an SVG icon with xmlns, path d and line coordinates"
  (check-equal? "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"24\"><path d=\"M5 13l4 4L19 7\"></path><line x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\"></line><rect rx=\"4\" ry=\"4\"></rect><use xlink:href=\"#a\"></use></svg>"
    (html->string
      (haml (:svg ([xmlns "http://www.w3.org/2000/svg"] [xmlns:xlink "http://www.w3.org/1999/xlink"] [width 24])
              (:path ([d "M5 13l4 4L19 7"]))
              (:line ([x1 0] [y1 0] [x2 1] [y2 1]))
              (:rect ([rx 4] [ry 4]))
              (:use ([xlink:href "#a"])))))))

(test "HTML elements are errors in SVG context"
  (check (rejects? '(haml (:svg (:div))) "div is not an SVG 2 element"))
  (check (rejects? '(haml (:svg (:g (:p "x")))) "p is not an SVG 2 element"))
  (check (rejects? '(haml (:svg (:b))) "b is not an SVG 2 element"))
  (check (rejects? '(haml (:svg (:font))) "font is not an SVG 2 element"))
  (check (rejects? '(haml (:svg (:my-icon))) "my-icon is not an SVG 2 element"))
  (check (rejects? '(haml (:svg (:altGlyph))) "altGlyph is not an SVG 2 element"))
  (check (rejects? '(haml (:svg (:lineargradient))) "lineargradient is not an SVG 2 element")))

(test "foreignObject, desc and title switch back to HTML"
  (check-equal? "<svg><foreignObject><div><p>x</p></div></foreignObject><desc><b>d</b></desc><title>t <i>i</i></title></svg>"
    (html->string
      (haml (:svg (:foreignObject (:div (:p "x"))) (:desc (:b "d")) (:title "t " (:i "i"))))))
  (check (rejects? '(haml (:svg (:foreignObject (:circle)))) "circle is an SVG element"))
  (check (accepted? '(haml (:svg (:foreignObject (:svg (:circle)))))))
  (check-equal? "<svg><foreignObject><template><div></div></template></foreignObject></svg>"
    (html->string (haml (:svg (:foreignObject (:template (:div))))))))

(test "SVG 2 names are written in the spelling the parser's case adjustment restores"
  (check-equal? "<svg><filter><feDropShadow stdDeviation=\"2\"></feDropShadow></filter></svg>"
    (html->string (haml (:svg (:filter (:feDropShadow ([stdDeviation 2]))))))))

(test "script and style in SVG context are escaped like other text"
  (check-equal? "<svg><script>x = a&lt;b &amp;&amp; c</script><style>a&gt;b{}</style></svg>"
    (html->string (haml (:svg (:script "x = a<b && c") (:style "a>b{}")))))
  (let ([s "</script>"])
    (check-equal? "<svg><script>&lt;/script&gt;</script></svg>"
      (html->string (haml (:svg (:script s)))))))

(test "SVG attribute names: SVG 2's camel-case spellings and the namespaced names"
  (check (rejects? '(haml (:svg (["viewbox" "0 0 1 1"]))) "camel-case spelling"))
  (check (rejects? '(haml (:svg (:rect (["pathlength" "1"])))) "camel-case spelling"))
  (check (rejects? '(haml (:svg (["fooBar" "1"]))) "the only attribute names with upper case"))
  (check (rejects? '(haml (:svg (["Fill" "1"]))) "the only attribute names with upper case"))
  (check (rejects? '(haml (:svg (["xlink:foo" "1"]))) "not a namespaced attribute"))
  (check (rejects? '(haml (:svg (["xml:base" "1"]))) "not a namespaced attribute"))
  (check (rejects? '(haml (:svg (["xmlns:foo" "1"]))) "not a namespaced attribute"))
  (check (accepted? '(haml (:svg ([xml:lang "en"] [xml:space "preserve"] [xlink:title "t"]))))))

(test "xmlns and xmlns:xlink take their literal namespace"
  (check (rejects? '(haml (:svg ([xmlns "http://example.com/"]))) "xmlns must be the literal"))
  (check (rejects? '(let ([ns "http://www.w3.org/2000/svg"]) (haml (:svg ([xmlns ns]))))
           "xmlns must be the literal"))
  (check (rejects? '(haml (:svg ([xmlns]))) "xmlns must be the literal"))
  (check (rejects? '(haml (:svg ([xmlns:xlink "http://www.w3.org/2000/svg"])))
           "xmlns:xlink must be the literal")))

(test "an SVG-only name at a form's top starts SVG context"
  (check-equal? "<circle r=\"1\"></circle>" (html->string (haml (:circle ([r 1])))))
  (check-equal? "<g><rect></rect></g><path d=\"M0 0\"></path>"
    (html->string (haml (:g (:rect)) (:path ([d "M0 0"])))))
  (check-equal? "<image href=\"x\"></image>" (html->string (haml (:image ([href "x"])))))
  (check (rejects? '(haml (:g (:div))) "div is not an SVG 2 element")))

(test "a literal svg xmlns on a top element starts SVG context"
  (check-equal? "<a xmlns=\"http://www.w3.org/2000/svg\" href=\"/x\"><circle r=\"2\"></circle></a>"
    (html->string (haml (:a ([xmlns "http://www.w3.org/2000/svg"] [href "/x"]) (:circle ([r 2]))))))
  (check-equal? "<style xmlns=\"http://www.w3.org/2000/svg\">a&gt;b{}</style><circle></circle>"
    (html->string (haml (:style ([xmlns "http://www.w3.org/2000/svg"]) "a>b{}") (:circle)))))

(test "a, script, style and title at a form's top are HTML"
  (check-equal? "<a href=\"/x\"><div>d</div></a>" (html->string (haml (:a ([href "/x"]) (:div "d")))))
  (check-equal? "<script>a<b</script>" (html->string (haml (:script "a<b"))))
  (check (rejects? '(haml (:a (:circle))) "circle is an SVG element")))

(test "a form's top cannot mix HTML and SVG-context elements"
  (check (rejects? '(haml (:circle) (:div)) "the top of this form mixes HTML and SVG elements"))
  (check (rejects? '(haml (:style "circle{fill:red}") (:circle)) "the top of this form mixes HTML and SVG elements"))
  (check (rejects? '(let ([c #t]) (haml (when c (:p)) (:circle))) "the top of this form mixes HTML and SVG elements")))

(test "SVG integration points are boundaries of the scopes"
  (check (accepted? '(haml (:p (:svg (:desc (:div)))))))
  (check (accepted? '(haml (:p (:svg (:foreignObject (:div)))))))
  (check (accepted? '(haml (:li (:svg (:title (:li))))))))

(test-exit)
