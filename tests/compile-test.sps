(import (chezscheme) (check) (support) (html))

(define-syntax raises-value-error
  (syntax-rules ()
    [(_ fragment e) (check-raises (raises-with? 'html fragment) e)]))

(define (count-occurrences s fragment)
  (let ([n (string-length s)] [m (string-length fragment)])
    (let loop ([i 0] [count 0])
      (cond
        [(fx> (fx+ i m) n) count]
        [(string=? (substring s i (fx+ i m)) fragment) (loop (fx+ i 1) (fx+ count 1))]
        [else (loop (fx+ i 1) count)]))))

(define (expansion-text datum)
  (format "~s" (syntax->datum (expand datum (environment '(chezscheme) '(html))))))

(test "constant markup is merged into one bytevector"
  (check-equal? 1
    (count-occurrences
      (expansion-text '(html (:div.a (:p ([title "t"] [hidden]) "x & y" 3) (:br) "z")))
      "put-bytevector"))
  (check-equal? "<div class=\"a\"><p title=\"t\" hidden>x &amp; y3</p><br>z</div>"
    (html->string (html (:div.a (:p ([title "t"] [hidden]) "x & y" 3) (:br) "z")))))

(test "a constant ends before an attribute whose value is an expression"
  (let ([v "1"])
    (check-equal? 2
      (count-occurrences
        (expansion-text '(let ([v "1"]) (html (:p ([id "a"] [title v] [lang "en"]) "x"))))
        "put-bytevector"))
    (check-equal? "<p id=\"a\" title=\"1\" lang=\"en\">x</p>"
      (html->string (html (:p ([id "a"] [title v] [lang "en"]) "x"))))))

(test "attributes of several parts are concatenated"
  (let ([n 5] [s "a&b"])
    (check-equal? "<a href=\"/items/5?q=a&amp;b\">x</a>"
      (html->string (html (:a ([href "/items/" n "?q=" s]) "x"))))
    (check-equal? "<a href=\"12\"></a>" (html->string (html (:a ([href 1 2])))))))

(test "bare attributes and string attribute names"
  (check-equal? "<input autofocus data-bind:filter>"
    (html->string (html (:input ([autofocus] [data-bind:filter])))))
  (check-equal? "<button hx-post=\"/x\" @click=\"open = true\"></button>"
    (html->string (html (:button (["hx-post" "/x"] ["@click" "open = true"]))))))

(test "#t writes the bare name and #f leaves the attribute out"
  (let ([t #t] [f #f])
    (check-equal? "<input disabled>" (html->string (html (:input ([disabled t] [readonly f])))))))

(test "class merges shorthand classes and every class entry, in order"
  (let ([a "x"] [b #f] [c ""] [n 7])
    (check-equal? "<p class=\"s t x u\"></p>" (html->string (html (:p.s ([class "t"] [class a] [class "u"])))))
    (check-equal? "<p id=\"i\" class=\"x\" title=\"t\"></p>"
      (html->string (html (:p ([id "i"] [class a] [title "t"] [class b])))))
    (check-equal? "<p class=\"s\" id=\"i\"></p>" (html->string (html (:p.s ([id "i"] [class b] [class c])))))
    (check-equal? "<p id=\"i\"></p>" (html->string (html (:p ([id "i"] [class b] [class c])))))
    (check-equal? "<p class=\"7 a7b\"></p>" (html->string (html (:p ([class n] [class "a" n "b"])))))
    (check-equal? "<p class=\"s\"></p>" (html->string (html (:p.s ([class c "" c])))))
    (check-equal? "<p class=\"x y\"></p>" (html->string (html (:p (["class" a] [class "y"])))))
    (check-equal? "<p class=\"a\"></p>" (html->string (html (:p ([class ""] [class "a"])))))
    (check-equal? "<p></p>" (html->string (html (:p ([class ""])))))))

(test "class rejects #t and a bare class"
  (let ([t #t])
    (raises-value-error "class cannot be #t" (html (:p ([class t])))))
  (raises-value-error "attribute value part is not a string or exact integer"
    (let ([f #f]) (html (:p ([class "a" f])))))
  (check (rejects? '(html (:p ([class]))) "class needs a value"))
  (check (rejects? '(html (:p ([class #t]))) "write [class \"x\"], or leave it out")))

(test "shorthand classes are escaped attribute text"
  (check-equal? "<div class=\"a&amp;b c&lt;d\"></div>"
    (html->string (html (:div.a&b.c<d))))
  (check (rejects? (list 'html (list (string->symbol ":div.a\x0;b"))) "a class name cannot contain NUL")))

(test "duplicate attributes, compared ASCII case-insensitively, are errors except class"
  (check (rejects? '(html (:a ([href "a"] [href "b"]))) "duplicate attribute"))
  (check (rejects? '(html (:a ([href "a"] ["href" "b"]))) "duplicate attribute"))
  (check (rejects? '(html (:svg ([viewBox "0 0 1 1"] ["viewBox" "0 0 2 2"]))) "duplicate attribute"))
  (check (rejects? '(html (:svg ([viewBox "0 0 1 1"] ["viewbox" "0 0 2 2"]))) "camel-case spelling"))
  (check-equal? "<p data-É=\"1\" data-é=\"2\"></p>"
    (html->string (html (:p (["data-É" "1"] ["data-é" "2"]))))))

(test "upper case in an HTML attribute name is an error"
  (check (rejects? '(html (:div (["Title" "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(html (:div (["CLASS" "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(html (:div ([viewBox "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(html (:div ([LANG "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(html (:svg ([fooBar "x"]))) "the only attribute names with upper case")))

(test "identifier attribute names must be known"
  (check (rejects? '(html (:div ([hx-post "/x"]))) "unknown attribute name hx-post"))
  (check (rejects? '(html (:div ([x-data "{}"]))) "write the name as a string"))
  (for-each
    (lambda (name)
      (check (not (compile-error `(html (:svg (:g ([,name "1"]))))))))
    '(d rx ry cx cy r x y width height x1 x2 y1 y2 in2 k1 k2 k3 k4 role part exportparts
      property xlink:href xml:lang is slot itemprop popover nonce
      viewBox preserveAspectRatio stroke-width fill))
  (for-each
    (lambda (name)
      (check (not (compile-error `(html (:div ([,name "1"])))))))
    '(data-signals data-on:click__debounce.500ms aria-label onclick onwhatever
      x-on:click id class title)))

(test "unknown, obsolete and MathML elements are errors; custom elements are allowed"
  (check (rejects? '(html (:dvi)) "unknown element dvi"))
  (check (rejects? '(html (:DIV)) "unknown element DIV"))
  (check (rejects? '(html (:center "x")) "unknown element center"))
  (check (rejects? '(html (:marquee "x")) "unknown element marquee"))
  (check (rejects? '(html (:font)) "unknown element font"))
  (check (rejects? '(html (:math)) "MathML is not supported"))
  (check (rejects? '(html (:Foo-bar)) "unknown element Foo-bar"))
  (check (rejects? '(html (:font-face)) "unknown element font-face"))
  (check (rejects? '(html (:p (:circle))) "circle is an SVG element"))
  (check-equal? "<my-card x-y=\"1\"><p>x</p></my-card>"
    (html->string (html (:my-card (["x-y" "1"]) (:p "x")))))
  (check-equal? "<math-é></math-é>" (html->string (html (:math-é)))))

(test "void elements have no end tag and no children"
  (check-equal? "<br><img src=\"x\"><input>" (html->string (html (:br) (:img ([src "x"])) (:input))))
  (check (rejects? '(html (:br "x")) "br is a void element and cannot have children"))
  (check (rejects? '(html (:img (:b))) "img is a void element and cannot have children"))
  (check (rejects? '(html (:input x)) "input is a void element and cannot have children"))
  (check (rejects? '(html (:hr (when x "y"))) "hr is a void element and cannot have children")))

(test "iframe and selectedcontent take no children"
  (check-equal? "<iframe src=\"x\"></iframe>" (html->string (html (:iframe ([src "x"])))))
  (check (rejects? '(html (:iframe "x")) "iframe cannot have children"))
  (check (rejects? '(html (:select (:button (:selectedcontent "x")))) "selectedcontent cannot have children")))

(test "a newline is written after the start tag of pre and textarea"
  (let ([s "\nx"])
    (check-equal? "<pre>\n\nx</pre>" (html->string (html (:pre s))))
    (check-equal? "<pre>\n</pre>" (html->string (html (:pre))))
    (check-equal? "<textarea name=\"t\">\na</textarea>" (html->string (html (:textarea ([name "t"]) "a"))))))

(test "textarea and title hold only text"
  (let ([s "<b>"] [n 3])
    (check-equal? "<title>&lt;b&gt;3</title>" (html->string (html (:title s n))))
    (check-equal? "<textarea>\n&lt;b&gt;</textarea>" (html->string (html (:textarea (list s))))))
  (check (rejects? '(html (:title (:b "x"))) "title holds only text"))
  (check (rejects? '(html (:textarea (:b "x"))) "textarea holds only text"))
  (raises-value-error "holds only text, not an html value"
    (let ([b (html (:b))]) (html (:title b))))
  (raises-value-error "holds only text, not an html value"
    (html (:textarea (list (raw-html "<b>")))))
  (raises-value-error "child is not a string, exact integer or list"
    (let ([f #f]) (html (:title f)))))

(test "script and style text is written raw"
  (let ([s "a < b && c > \"d\""])
    (check-equal? "<script>if (a < b && c > \"d\") f()</script>"
      (html->string (html (:script "if (a < b && c > \"d\") f()"))))
    (check-equal? "<script>x = 'a < b && c > \"d\"';</script>"
      (html->string (html (:script "x = '" s "';"))))
    (check-equal? "<style>a > b { content: \"&\" }</style>"
      (html->string (html (:style "a > b { content: \"&\" }"))))
    (check-equal? "<script>1-2[x]</script>"
      (html->string (html (:script 1 (list -2 (list "[" "x" "]"))))))))

(test "literal raw text containing its end tag is a compile-time error"
  (check (rejects? '(html (:script "a</script>")) "script text contains </script"))
  (check (rejects? '(html (:script "a</SCRIPT>")) "script text contains </script"))
  (check (rejects? '(html (:script "a<!--b")) "script text contains <!--"))
  (check (rejects? '(html (:script "</noscript>")) "script text contains </noscript"))
  (check (rejects? '(html (:style "</Style")) "style text contains </style"))
  (check (rejects? '(html (:style "</noscript")) "style text contains </noscript"))
  (check (rejects? '(html (:script "</scr" "ipt>")) "script text contains </script"))
  (check (rejects? '(html (:script "<!" "--")) "script text contains <!--"))
  (check (not (compile-error '(html (:style "<!-- a -->")))))
  (check (not (compile-error '(html (:script "</scrip t>"))))))

(test "raw text split across literals, values and lists is checked as one text"
  (let ([v "ipt><img src=x onerror=alert(1)>"])
    (raises-value-error "script text would contain </script"
      (html (:script "var s='</scr" v "'"))))
  (let ([v "</sty"])
    (raises-value-error "style text would contain </style"
      (html (:style "a{}" v "le><b>x</b>"))))
  (let ([v "--<script>'"])
    (raises-value-error "script text would contain <!--"
      (html (:script "x='<!" v))))
  (let ([v "</nos"])
    (raises-value-error "style text would contain </noscript"
      (html (:noscript (:style v "cript><img src=x onerror=alert(1)>")))))
  (let ([a "</"] [b "script>"])
    (raises-value-error "script text would contain </script" (html (:script a b))))
  (raises-value-error "script text would contain </script"
    (html (:script (list "</" (list "scr" "ipt")))))
  (raises-value-error "script text would contain </script"
    (html (:script (list "<" "/" "s" "c" "r" "i" "p" "t"))))
  (raises-value-error "script text would contain <!--"
    (html (:script (list "<!-" "-"))))
  (let ([v "SCRIPT"])
    (raises-value-error "script text would contain </script" (html (:script "</" v)))))

(test "raw text values are encoded once, when the form is evaluated"
  (let* ([s (string-copy "safe text")] [h (html (:script "var s = '" s "';"))])
    (string-copy! "</script><img src=x onerror=f(" 0 s 0 9)
    (check-equal? "<script>var s = 'safe text';</script>" (html->string h))))

(test "html values, raw-html and control forms are refused in script and style"
  (raises-value-error "script and style hold only text, not an html value"
    (let ([b (html (:b))]) (html (:script b))))
  (raises-value-error "script and style hold only text, not an html value"
    (html (:style (list (raw-html "x")))))
  (raises-value-error "child is not a string, exact integer or list"
    (let ([f #f]) (html (:script f))))
  (check (rejects? '(html (:script (when x "y"))) "compute it in Scheme"))
  (check (rejects? '(html (:style (begin "a"))) "compute it in Scheme"))
  (check (rejects? '(html (:script (:b))) "script holds only text, not elements")))

(test "noscript holds ordinary markup, and script inside it rejects </noscript"
  (let ([pixel (html (:img ([src "/pixel"])))])
    (check-equal? "<noscript><p>no js</p><img src=\"/pixel\"></noscript>"
      (html->string (html (:noscript (:p "no js") pixel)))))
  (check (rejects? '(html (:noscript (:p (:noscript)))) "noscript cannot be inside noscript")))

(test "control forms choose children at evaluation"
  (let ([yes #t] [no #f])
    (check-equal? "<p>ab</p>" (html->string (html (:p (when yes "a" "b") (when no "c")))))
    (check-equal? "<p>c</p>" (html->string (html (:p (unless yes "a" "b") (unless no "c")))))
    (check-equal? "<p><b>y</b><i>n</i></p>"
      (html->string (html (:p (if yes (:b "y") (:b "n")) (if no (:i "y") (:i "n"))))))
    (check-equal? "<p>two</p>"
      (html->string (html (:p (cond [no "one"] [(= 2 2) "two"] [else "three"])))))
    (check-equal? "<p>three</p>"
      (html->string (html (:p (cond [no "one"] [else "three"])))))
    (check-equal? "<p></p>" (html->string (html (:p (cond [no "one"])))))
    (check-equal? "<p>b</p>"
      (html->string (html (:p (case (+ 1 1) [(1) "a"] [(2 3) "b"] [else "c"])))))
    (check-equal? "<p></p>" (html->string (html (:p (case 9 [(1) "a"])))))
    (check-equal? "<p>xy</p>" (html->string (html (:p (begin "x" "y")))))
    (check-equal? "<p>3 4</p>"
      (html->string (html (:p (let ([a 1] [b 2]) (+ a b) " " (* b 2))))))
    (check-equal? "<p>3</p>" (html->string (html (:p (let* ([a 1] [b (+ a 2)]) b)))))))

(test "only the chosen branch's expressions are evaluated"
  (let* ([log '()] [note (lambda (x) (set! log (cons x log)) x)])
    (html->string
      (html (:p (if (note #f) (note "then") (note "else"))
                (cond [(note #f) (note "c1")] [(note #t) (note "c2")] [else (note "c3")])
                (case (note 2) [(1) (note "k1")] [else (note "k2")])
                (when (note #f) (note "w"))
                (unless (note #t) (note "u")))))
    (check-equal? '(#f "else" #f #t "c2" 2 "k2" #f #t) (reverse log))))

(test "inline for-each writes its body for each element, in order"
  (check-equal? "<ul><li>a</li><li>b</li></ul>"
    (html->string (html (:ul (for-each (lambda (x) (:li x)) (list "a" "b"))))))
  (check-equal? "<ul></ul>" (html->string (html (:ul (for-each (lambda (x) (:li x)) (list))))))
  (check-equal? "<dl><dt>a</dt><dd>1</dd><dt>b</dt><dd>2</dd></dl>"
    (html->string
      (html (:dl (for-each (lambda (k v) (:dt k) (:dd v)) (list "a" "b") (list 1 2))))))
  (let* ([log '()] [note (lambda (x) (set! log (cons x log)) x)])
    (html (:ul (for-each (lambda (x) (:li (note x))) (list 1 2 3))))
    (check-equal? '(1 2 3) (reverse log))))

(test "inline for-each checks its lists"
  (raises-value-error "for-each list is not a proper list"
    (html (:ul (for-each (lambda (x) (:li x)) (cons 1 2)))))
  (raises-value-error "for-each list is not a proper list"
    (html (:ul (for-each (lambda (x) (:li x)) 'x))))
  (raises-value-error "for-each list is not a proper list"
    (let ([l (list 1 2)]) (set-cdr! (cdr l) l) (html (:ul (for-each (lambda (x) (:li x)) l)))))
  (raises-value-error "for-each lists have different lengths"
    (html (:ul (for-each (lambda (x y) (:li x y)) (list 1 2) (list 1)))))
  (raises-value-error "for-each list is not a proper list"
    (html (:ul (for-each (lambda (x y) (:li x y)) (list 1 2) (cons 1 2))))))

(test "inline for-each collects without mutating what it returned"
  (let ([k #f] [values-made '()])
    (let ([h (html (:ul (for-each (lambda (x)
                                    (:li (if (string=? x "b")
                                             (call/cc (lambda (c) (set! k c) "b1"))
                                             x)))
                                  (list "a" "b" "c"))))])
      (set! values-made (cons h values-made))
      (when (= (length values-made) 1) (k "b2"))
      (check-equal? '("<ul><li>a</li><li>b1</li><li>c</li></ul>"
                      "<ul><li>a</li><li>b2</li><li>c</li></ul>")
        (map html->string (reverse values-made))))))

(test "table parts hold no text"
  (check (rejects? '(html (:table "x")) "text in table would be moved"))
  (check (rejects? '(html (:table (:tbody (:tr 5)))) "text in tr would be moved"))
  (check (rejects? '(html (:table (:colgroup "c"))) "text in colgroup would be moved"))
  (check (rejects? '(html (:table (:tbody "x"))) "text in tbody would be moved"))
  (check-equal? "<table>\n  <tbody><tr> <td>x</td>\n</tr></tbody></table>"
    (html->string (html (:table "\n  " (:tbody (:tr " " (:td "x") "\n"))))))
  (raises-value-error "text here would be moved by the parser"
    (let ([n 5]) (html (:table (:tbody (:tr n))))))
  (raises-value-error "text here would be moved by the parser"
    (let ([s " "]) (html (:table s))))
  (raises-value-error "text here would be moved by the parser"
    (let ([rows (list (html (:tr)) "x")]) (html (:table (:tbody rows)))))
  (let ([e ""] [rows (list (html (:tr (:td "a"))) "" (list))])
    (check-equal? "<table><tbody><tr><td>a</td></tr></tbody></table>"
      (html->string (html (:table e (:tbody rows))))))
  (raises-value-error "child is not an html value"
    (let ([f #f]) (html (:table f)))))

(test "text values are refused where literal text is"
  (let ([s "x"] [n 5] [cell (html (:td "b"))])
    (raises-value-error "text here would be moved by the parser"
      (html (:template (:col) s)))
    (raises-value-error "text here would be moved by the parser"
      (html (:template (:col) n)))
    (raises-value-error "text here would be moved by the parser"
      (html (:tr (:td "a")) s))
    (raises-value-error "text here would be moved by the parser"
      (html (:td "a") (list s)))
    (check-equal? "<td>a</td><td>b</td>" (html->string (html (:td "a") cell)))
    (check-equal? "<template><tr><td>a</td></tr>x</template>"
      (html->string (html (:template (:tr (:td "a")) s))))
    (check-equal? "<div></div>x" (html->string (html (:div) s)))))

(test "an input in a table part needs the literal type hidden"
  (check-equal? "<table><input type=\"hidden\" name=\"x\"></table>"
    (html->string (html (:table (:input ([type "hidden"] [name "x"]))))))
  (check-equal? "<table><tbody><tr><input type=\"HIDDEN\"></tr></tbody></table>"
    (html->string (html (:table (:tbody (:tr (:input ([type "HIDDEN"]))))))))
  (check (rejects? '(html (:table (:input))) "input cannot be a child of table"))
  (check (rejects? '(html (:table (:input ([type "text"])))) "input cannot be a child of table"))
  (check (rejects? '(html (:table (:input ([type t])))) "input cannot be a child of table"))
  (check (rejects? '(html (:table (:colgroup (:input ([type "hidden"]))))) "input cannot be a child of colgroup")))

(test "a document is the only child of its form"
  (check-equal? "<!DOCTYPE html><html><head><title>t</title></head><body><p>y</p></body></html>"
    (html->string (html (:html (:head (:title "t")) (:body (:p "y"))))))
  (check-equal? "<!DOCTYPE html><html><body></body></html>" (html->string (html (:html (:body)))))
  (check-equal? "<!DOCTYPE html><html>\n<head></head>\n</html>"
    (html->string (html (:html "\n" (:head) "\n"))))
  (check (rejects? '(html (:html (:head) (:body)) (:p "after")) "(:html …) must be the only child"))
  (check (rejects? '(html (:p "x") (:html (:head) (:body))) "(:html …) must be the only child"))
  (check (rejects? '(html (when x (:html))) "(:html …) must be the only child"))
  (check (rejects? '(html (:div (:html))) "(:html …) must be the only child")))

(test "html holds at most one head, then at most one body"
  (check (rejects? '(html (:html (:body) (:head))) "html holds at most one head, before body"))
  (check (rejects? '(html (:html (:head) (:head))) "html holds at most one head, before body"))
  (check (rejects? '(html (:html (:body) (:body))) "html holds at most one body"))
  (check (rejects? '(html (:html (:head) (:script "x") (:body))) "html holds only head and body"))
  (check (rejects? '(html (:html "text" (:body))) "text in html would be moved"))
  (check (rejects? '(html (:html (if x (:body) (:head)) (:body))) "html holds at most one body"))
  (check (rejects? '(html (:html (for-each (lambda (x) (:body)) xs))) "html holds at most one body"))
  (check (not (compile-error '(let ([x #t]) (html (:html (:head) (if x (:body.a) (:body.b)))))))))

(test "head holds only metadata elements, and its noscript only link, meta and style"
  (check-equal? "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>t</title><link rel=\"x\"><noscript><style>a{}</style></noscript></head></html>"
    (html->string (html (:html (:head (:meta ([charset "utf-8"])) (:title "t") (:link ([rel "x"]))
                                      (:noscript (:style "a{}")))))))
  (check (rejects? '(html (:html (:head (:title "t") (:div "x")))) "head cannot hold div"))
  (check (rejects? '(html (:html (:head (:my-meta)))) "head cannot hold my-meta"))
  (check (rejects? '(html (:html (:head "text"))) "text in head would be moved"))
  (check (rejects? '(html (:head (:noscript (:p "no js")))) "a noscript in head holds only link, meta and style"))
  (check (rejects? '(html (:head (:noscript "x"))) "text in noscript would be moved")))

(test "expressions in html and head must give html values"
  (let ([extra (html (:link ([rel "stylesheet"] [href "/a.css"])))] [body (html (:body "b"))])
    (check-equal? "<!DOCTYPE html><html><head><title>t</title><link rel=\"stylesheet\" href=\"/a.css\"></head><body>b</body></html>"
      (html->string (html (:html (:head (:title "t") extra) body)))))
  (raises-value-error "text here would be moved by the parser"
    (let ([s "x"]) (html (:html (:head s)))))
  (raises-value-error "text here would be moved by the parser"
    (let ([n 1]) (html (:html n)))))

(test "head and body are allowed at a form's top and in html only"
  (check-equal? "<head><title>t</title></head>" (html->string (html (:head (:title "t")))))
  (check-equal? "<body class=\"x\"></body>" (html->string (html (:body.x))))
  (check (rejects? '(html (:div (:body))) "body can be only at the top of a form or inside html"))
  (check (rejects? '(html (:div (:head))) "head can be only at the top of a form or inside html"))
  (check (rejects? '(html (:body (:head))) "head can be only at the top of a form or inside html")))

(test-exit)
