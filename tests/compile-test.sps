(import (chezscheme) (check) (support) (haml))

(define-syntax raises-value-error
  (syntax-rules ()
    [(_ fragment e) (check-raises (raises-with? 'haml fragment) e)]))

(define (count-occurrences s fragment)
  (let ([n (string-length s)] [m (string-length fragment)])
    (let loop ([i 0] [count 0])
      (cond
        [(fx> (fx+ i m) n) count]
        [(string=? (substring s i (fx+ i m)) fragment) (loop (fx+ i 1) (fx+ count 1))]
        [else (loop (fx+ i 1) count)]))))

(define (expansion-text datum)
  (format "~s" (syntax->datum (expand datum (environment '(chezscheme) '(haml))))))

(test "constant markup is merged into one bytevector"
  (check-equal? 1
    (count-occurrences
      (expansion-text '(haml (:div.a (:p ([title "t"] [hidden]) "x & y" 3) (:br) "z")))
      "put-bytevector"))
  (check-equal? "<div class=\"a\"><p title=\"t\" hidden>x &amp; y3</p><br>z</div>"
    (html->string (haml (:div.a (:p ([title "t"] [hidden]) "x & y" 3) (:br) "z")))))

(test "a constant ends before an attribute whose value is an expression"
  (let ([v "1"])
    (check-equal? 2
      (count-occurrences
        (expansion-text '(let ([v "1"]) (haml (:p ([id "a"] [title v] [lang "en"]) "x"))))
        "put-bytevector"))
    (check-equal? "<p id=\"a\" title=\"1\" lang=\"en\">x</p>"
      (html->string (haml (:p ([id "a"] [title v] [lang "en"]) "x"))))))

(test "attributes of several parts are concatenated"
  (let ([n 5] [s "a&b"])
    (check-equal? "<a href=\"/items/5?q=a&amp;b\">x</a>"
      (html->string (haml (:a ([href "/items/" n "?q=" s]) "x"))))
    (check-equal? "<a href=\"12\"></a>" (html->string (haml (:a ([href 1 2])))))))

(test "bare attributes and string attribute names"
  (check-equal? "<input autofocus data-bind:filter>"
    (html->string (haml (:input ([autofocus] [data-bind:filter])))))
  (check-equal? "<button hx-post=\"/x\" @click=\"open = true\"></button>"
    (html->string (haml (:button (["hx-post" "/x"] ["@click" "open = true"]))))))

(test "#t writes the bare name and #f leaves the attribute out"
  (let ([t #t] [f #f])
    (check-equal? "<input disabled>" (html->string (haml (:input ([disabled t] [readonly f])))))))

(test "class merges shorthand classes and every class entry, in order"
  (let ([a "x"] [b #f] [c ""] [n 7])
    (check-equal? "<p class=\"s t x u\"></p>" (html->string (haml (:p.s ([class "t"] [class a] [class "u"])))))
    (check-equal? "<p id=\"i\" class=\"x\" title=\"t\"></p>"
      (html->string (haml (:p ([id "i"] [class a] [title "t"] [class b])))))
    (check-equal? "<p class=\"s\" id=\"i\"></p>" (html->string (haml (:p.s ([id "i"] [class b] [class c])))))
    (check-equal? "<p id=\"i\"></p>" (html->string (haml (:p ([id "i"] [class b] [class c])))))
    (check-equal? "<p class=\"7 a7b\"></p>" (html->string (haml (:p ([class n] [class "a" n "b"])))))
    (check-equal? "<p class=\"s\"></p>" (html->string (haml (:p.s ([class c "" c])))))
    (check-equal? "<p class=\"x y\"></p>" (html->string (haml (:p (["class" a] [class "y"])))))
    (check-equal? "<p class=\"a\"></p>" (html->string (haml (:p ([class ""] [class "a"])))))
    (check-equal? "<p></p>" (html->string (haml (:p ([class ""])))))))

(test "class rejects #t and a bare class"
  (let ([t #t])
    (raises-value-error "class cannot be #t" (haml (:p ([class t])))))
  (raises-value-error "attribute value part is not a string or exact integer"
    (let ([f #f]) (haml (:p ([class "a" f])))))
  (check (rejects? '(haml (:p ([class]))) "class needs a value"))
  (check (rejects? '(haml (:p ([class #t]))) "write [class \"x\"], or leave it out")))

(test "shorthand classes are escaped attribute text"
  (check-equal? "<div class=\"a&amp;b c&lt;d\"></div>"
    (html->string (haml (:div.a&b.c<d))))
  (check (rejects? (list 'haml (list (string->symbol ":div.a\x0;b"))) "a class name cannot contain NUL")))

(test "duplicate attributes, compared ASCII case-insensitively, are errors except class"
  (check (rejects? '(haml (:a ([href "a"] [href "b"]))) "duplicate attribute"))
  (check (rejects? '(haml (:a ([href "a"] ["href" "b"]))) "duplicate attribute"))
  (check (rejects? '(haml (:svg ([viewBox "0 0 1 1"] ["viewBox" "0 0 2 2"]))) "duplicate attribute"))
  (check (rejects? '(haml (:svg ([viewBox "0 0 1 1"] ["viewbox" "0 0 2 2"]))) "camel-case spelling"))
  (check-equal? "<p data-É=\"1\" data-é=\"2\"></p>"
    (html->string (haml (:p (["data-É" "1"] ["data-é" "2"]))))))

(test "upper case in an HTML attribute name is an error"
  (check (rejects? '(haml (:div (["Title" "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(haml (:div (["CLASS" "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(haml (:div ([viewBox "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(haml (:div ([LANG "x"]))) "attribute names on HTML elements are lower case"))
  (check (rejects? '(haml (:svg ([fooBar "x"]))) "the only attribute names with upper case")))

(test "identifier attribute names must be known"
  (check (rejects? '(haml (:div ([hx-post "/x"]))) "unknown attribute name hx-post"))
  (check (rejects? '(haml (:div ([x-data "{}"]))) "write the name as a string"))
  (for-each
    (lambda (name)
      (check (not (compile-error `(haml (:svg (:g ([,name "1"]))))))))
    '(d rx ry cx cy r x y width height x1 x2 y1 y2 in2 k1 k2 k3 k4 role part exportparts
      property xlink:href xml:lang is slot itemprop popover nonce
      viewBox preserveAspectRatio stroke-width fill))
  (for-each
    (lambda (name)
      (check (not (compile-error `(haml (:div ([,name "1"])))))))
    '(data-signals data-on:click__debounce.500ms aria-label onclick onwhatever
      x-on:click id class title)))

(test "unknown, obsolete and MathML elements are errors; custom elements are allowed"
  (check (rejects? '(haml (:dvi)) "unknown element dvi"))
  (check (rejects? '(haml (:DIV)) "unknown element DIV"))
  (check (rejects? '(haml (:center "x")) "unknown element center"))
  (check (rejects? '(haml (:marquee "x")) "unknown element marquee"))
  (check (rejects? '(haml (:font)) "unknown element font"))
  (check (rejects? '(haml (:math)) "MathML is not supported"))
  (check (rejects? '(haml (:Foo-bar)) "unknown element Foo-bar"))
  (check (rejects? '(haml (:font-face)) "unknown element font-face"))
  (check (rejects? '(haml (:p (:circle))) "circle is an SVG element"))
  (check-equal? "<my-card x-y=\"1\"><p>x</p></my-card>"
    (html->string (haml (:my-card (["x-y" "1"]) (:p "x")))))
  (check-equal? "<math-é></math-é>" (html->string (haml (:math-é)))))

(test "void elements have no end tag and no children"
  (check-equal? "<br><img src=\"x\"><input>" (html->string (haml (:br) (:img ([src "x"])) (:input))))
  (check (rejects? '(haml (:br "x")) "br is a void element and cannot have children"))
  (check (rejects? '(haml (:img (:b))) "img is a void element and cannot have children"))
  (check (rejects? '(haml (:input x)) "input is a void element and cannot have children"))
  (check (rejects? '(haml (:hr (when x "y"))) "hr is a void element and cannot have children")))

(test "iframe and selectedcontent take no children"
  (check-equal? "<iframe src=\"x\"></iframe>" (html->string (haml (:iframe ([src "x"])))))
  (check (rejects? '(haml (:iframe "x")) "iframe cannot have children"))
  (check (rejects? '(haml (:select (:button (:selectedcontent "x")))) "selectedcontent cannot have children")))

(test "a newline is written after the start tag of pre and textarea"
  (let ([s "\nx"])
    (check-equal? "<pre>\n\nx</pre>" (html->string (haml (:pre s))))
    (check-equal? "<pre>\n</pre>" (html->string (haml (:pre))))
    (check-equal? "<textarea name=\"t\">\na</textarea>" (html->string (haml (:textarea ([name "t"]) "a"))))))

(test "textarea and title hold only text"
  (let ([s "<b>"] [n 3])
    (check-equal? "<title>&lt;b&gt;3</title>" (html->string (haml (:title s n))))
    (check-equal? "<textarea>\n&lt;b&gt;</textarea>" (html->string (haml (:textarea (list s))))))
  (check (rejects? '(haml (:title (:b "x"))) "title holds only text"))
  (check (rejects? '(haml (:textarea (:b "x"))) "textarea holds only text"))
  (raises-value-error "holds only text, not an html value"
    (let ([b (haml (:b))]) (haml (:title b))))
  (raises-value-error "holds only text, not an html value"
    (haml (:textarea (list (raw-html "<b>")))))
  (raises-value-error "child is not a string, exact integer or list"
    (let ([f #f]) (haml (:title f)))))

(test "script and style text is written raw"
  (let ([s "a < b && c > \"d\""])
    (check-equal? "<script>if (a < b && c > \"d\") f()</script>"
      (html->string (haml (:script "if (a < b && c > \"d\") f()"))))
    (check-equal? "<script>x = 'a < b && c > \"d\"';</script>"
      (html->string (haml (:script "x = '" s "';"))))
    (check-equal? "<style>a > b { content: \"&\" }</style>"
      (html->string (haml (:style "a > b { content: \"&\" }"))))
    (check-equal? "<script>1-2[x]</script>"
      (html->string (haml (:script 1 (list -2 (list "[" "x" "]"))))))))

(test "literal raw text containing its end tag is a compile-time error"
  (check (rejects? '(haml (:script "a</script>")) "script text contains </script"))
  (check (rejects? '(haml (:script "a</SCRIPT>")) "script text contains </script"))
  (check (rejects? '(haml (:script "a<!--b")) "script text contains <!--"))
  (check (rejects? '(haml (:script "</noscript>")) "script text contains </noscript"))
  (check (rejects? '(haml (:style "</Style")) "style text contains </style"))
  (check (rejects? '(haml (:style "</noscript")) "style text contains </noscript"))
  (check (rejects? '(haml (:script "</scr" "ipt>")) "script text contains </script"))
  (check (rejects? '(haml (:script "<!" "--")) "script text contains <!--"))
  (check (not (compile-error '(haml (:style "<!-- a -->")))))
  (check (not (compile-error '(haml (:script "</scrip t>"))))))

(test "raw text split across literals, values and lists is checked as one text"
  (let ([v "ipt><img src=x onerror=alert(1)>"])
    (raises-value-error "script text would contain </script"
      (haml (:script "var s='</scr" v "'"))))
  (let ([v "</sty"])
    (raises-value-error "style text would contain </style"
      (haml (:style "a{}" v "le><b>x</b>"))))
  (let ([v "--<script>'"])
    (raises-value-error "script text would contain <!--"
      (haml (:script "x='<!" v))))
  (let ([v "</nos"])
    (raises-value-error "style text would contain </noscript"
      (haml (:noscript (:style v "cript><img src=x onerror=alert(1)>")))))
  (let ([a "</"] [b "script>"])
    (raises-value-error "script text would contain </script" (haml (:script a b))))
  (raises-value-error "script text would contain </script"
    (haml (:script (list "</" (list "scr" "ipt")))))
  (raises-value-error "script text would contain </script"
    (haml (:script (list "<" "/" "s" "c" "r" "i" "p" "t"))))
  (raises-value-error "script text would contain <!--"
    (haml (:script (list "<!-" "-"))))
  (let ([v "SCRIPT"])
    (raises-value-error "script text would contain </script" (haml (:script "</" v)))))

(test "raw text values are encoded once, when the form is evaluated"
  (let* ([s (string-copy "safe text")] [h (haml (:script "var s = '" s "';"))])
    (string-copy! "</script><img src=x onerror=f(" 0 s 0 9)
    (check-equal? "<script>var s = 'safe text';</script>" (html->string h))))

(test "html values, raw-html and control forms are refused in script and style"
  (raises-value-error "script and style hold only text, not an html value"
    (let ([b (haml (:b))]) (haml (:script b))))
  (raises-value-error "script and style hold only text, not an html value"
    (haml (:style (list (raw-html "x")))))
  (raises-value-error "child is not a string, exact integer or list"
    (let ([f #f]) (haml (:script f))))
  (check (rejects? '(haml (:script (when x "y"))) "compute it in Scheme"))
  (check (rejects? '(haml (:style (begin "a"))) "compute it in Scheme"))
  (check (rejects? '(haml (:script (:b))) "script holds only text, not elements")))

(test "noscript holds ordinary markup, and script inside it rejects </noscript"
  (let ([pixel (haml (:img ([src "/pixel"])))])
    (check-equal? "<noscript><p>no js</p><img src=\"/pixel\"></noscript>"
      (html->string (haml (:noscript (:p "no js") pixel)))))
  (check (rejects? '(haml (:noscript (:p (:noscript)))) "noscript cannot be inside noscript")))

(test "control forms choose children at evaluation"
  (let ([yes #t] [no #f])
    (check-equal? "<p>ab</p>" (html->string (haml (:p (when yes "a" "b") (when no "c")))))
    (check-equal? "<p>c</p>" (html->string (haml (:p (unless yes "a" "b") (unless no "c")))))
    (check-equal? "<p><b>y</b><i>n</i></p>"
      (html->string (haml (:p (if yes (:b "y") (:b "n")) (if no (:i "y") (:i "n"))))))
    (check-equal? "<p>two</p>"
      (html->string (haml (:p (cond [no "one"] [(= 2 2) "two"] [else "three"])))))
    (check-equal? "<p>three</p>"
      (html->string (haml (:p (cond [no "one"] [else "three"])))))
    (check-equal? "<p></p>" (html->string (haml (:p (cond [no "one"])))))
    (check-equal? "<p>b</p>"
      (html->string (haml (:p (case (+ 1 1) [(1) "a"] [(2 3) "b"] [else "c"])))))
    (check-equal? "<p></p>" (html->string (haml (:p (case 9 [(1) "a"])))))
    (check-equal? "<p>xy</p>" (html->string (haml (:p (begin "x" "y")))))
    (check-equal? "<p>3 4</p>"
      (html->string (haml (:p (let ([a 1] [b 2]) (+ a b) " " (* b 2))))))
    (check-equal? "<p>3</p>" (html->string (haml (:p (let* ([a 1] [b (+ a 2)]) b)))))))

(test "only the chosen branch's expressions are evaluated"
  (let* ([log '()] [note (lambda (x) (set! log (cons x log)) x)])
    (html->string
      (haml (:p (if (note #f) (note "then") (note "else"))
                (cond [(note #f) (note "c1")] [(note #t) (note "c2")] [else (note "c3")])
                (case (note 2) [(1) (note "k1")] [else (note "k2")])
                (when (note #f) (note "w"))
                (unless (note #t) (note "u")))))
    (check-equal? '(#f "else" #f #t "c2" 2 "k2" #f #t) (reverse log))))

(test "inline for-each writes its body for each element, in order"
  (check-equal? "<ul><li>a</li><li>b</li></ul>"
    (html->string (haml (:ul (for-each (lambda (x) (:li x)) (list "a" "b"))))))
  (check-equal? "<ul></ul>" (html->string (haml (:ul (for-each (lambda (x) (:li x)) (list))))))
  (check-equal? "<dl><dt>a</dt><dd>1</dd><dt>b</dt><dd>2</dd></dl>"
    (html->string
      (haml (:dl (for-each (lambda (k v) (:dt k) (:dd v)) (list "a" "b") (list 1 2))))))
  (let* ([log '()] [note (lambda (x) (set! log (cons x log)) x)])
    (haml (:ul (for-each (lambda (x) (:li (note x))) (list 1 2 3))))
    (check-equal? '(1 2 3) (reverse log))))

(test "inline for-each checks its lists"
  (raises-value-error "for-each list is not a proper list"
    (haml (:ul (for-each (lambda (x) (:li x)) (cons 1 2)))))
  (raises-value-error "for-each list is not a proper list"
    (haml (:ul (for-each (lambda (x) (:li x)) 'x))))
  (raises-value-error "for-each list is not a proper list"
    (let ([l (list 1 2)]) (set-cdr! (cdr l) l) (haml (:ul (for-each (lambda (x) (:li x)) l)))))
  (raises-value-error "for-each lists have different lengths"
    (haml (:ul (for-each (lambda (x y) (:li x y)) (list 1 2) (list 1)))))
  (raises-value-error "for-each list is not a proper list"
    (haml (:ul (for-each (lambda (x y) (:li x y)) (list 1 2) (cons 1 2))))))

(test "inline for-each collects without mutating what it returned"
  (let ([k #f] [values-made '()])
    (let ([h (haml (:ul (for-each (lambda (x)
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
  (check (rejects? '(haml (:table "x")) "text in table would be moved"))
  (check (rejects? '(haml (:table (:tbody (:tr 5)))) "text in tr would be moved"))
  (check (rejects? '(haml (:table (:colgroup "c"))) "text in colgroup would be moved"))
  (check (rejects? '(haml (:table (:tbody "x"))) "text in tbody would be moved"))
  (check-equal? "<table>\n  <tbody><tr> <td>x</td>\n</tr></tbody></table>"
    (html->string (haml (:table "\n  " (:tbody (:tr " " (:td "x") "\n"))))))
  (raises-value-error "text here would be moved by the parser"
    (let ([n 5]) (haml (:table (:tbody (:tr n))))))
  (raises-value-error "text here would be moved by the parser"
    (let ([s " "]) (haml (:table s))))
  (raises-value-error "text here would be moved by the parser"
    (let ([rows (list (haml (:tr)) "x")]) (haml (:table (:tbody rows)))))
  (let ([e ""] [rows (list (haml (:tr (:td "a"))) "" (list))])
    (check-equal? "<table><tbody><tr><td>a</td></tr></tbody></table>"
      (html->string (haml (:table e (:tbody rows))))))
  (raises-value-error "child is not an html value"
    (let ([f #f]) (haml (:table f)))))

(test "text values are refused where literal text is"
  (let ([s "x"] [n 5] [cell (haml (:td "b"))])
    (raises-value-error "text here would be moved by the parser"
      (haml (:template (:col) s)))
    (raises-value-error "text here would be moved by the parser"
      (haml (:template (:col) n)))
    (raises-value-error "text here would be moved by the parser"
      (haml (:tr (:td "a")) s))
    (raises-value-error "text here would be moved by the parser"
      (haml (:td "a") (list s)))
    (check-equal? "<td>a</td><td>b</td>" (html->string (haml (:td "a") cell)))
    (check-equal? "<template><tr><td>a</td></tr>x</template>"
      (html->string (haml (:template (:tr (:td "a")) s))))
    (check-equal? "<div></div>x" (html->string (haml (:div) s)))))

(test "an input in a table part needs the literal type hidden"
  (check-equal? "<table><input type=\"hidden\" name=\"x\"></table>"
    (html->string (haml (:table (:input ([type "hidden"] [name "x"]))))))
  (check-equal? "<table><tbody><tr><input type=\"HIDDEN\"></tr></tbody></table>"
    (html->string (haml (:table (:tbody (:tr (:input ([type "HIDDEN"]))))))))
  (check (rejects? '(haml (:table (:input))) "input cannot be a child of table"))
  (check (rejects? '(haml (:table (:input ([type "text"])))) "input cannot be a child of table"))
  (check (rejects? '(haml (:table (:input ([type t])))) "input cannot be a child of table"))
  (check (rejects? '(haml (:table (:colgroup (:input ([type "hidden"]))))) "input cannot be a child of colgroup")))

(test "a document is the only child of its form"
  (check-equal? "<!DOCTYPE html><html><head><title>t</title></head><body><p>y</p></body></html>"
    (html->string (haml (:html (:head (:title "t")) (:body (:p "y"))))))
  (check-equal? "<!DOCTYPE html><html><body></body></html>" (html->string (haml (:html (:body)))))
  (check-equal? "<!DOCTYPE html><html>\n<head></head>\n</html>"
    (html->string (haml (:html "\n" (:head) "\n"))))
  (check (rejects? '(haml (:html (:head) (:body)) (:p "after")) "(:html …) must be the only child"))
  (check (rejects? '(haml (:p "x") (:html (:head) (:body))) "(:html …) must be the only child"))
  (check (rejects? '(haml (when x (:html))) "(:html …) must be the only child"))
  (check (rejects? '(haml (:div (:html))) "(:html …) must be the only child")))

(test "html holds at most one head, then at most one body"
  (check (rejects? '(haml (:html (:body) (:head))) "html holds at most one head, before body"))
  (check (rejects? '(haml (:html (:head) (:head))) "html holds at most one head, before body"))
  (check (rejects? '(haml (:html (:body) (:body))) "html holds at most one body"))
  (check (rejects? '(haml (:html (:head) (:script "x") (:body))) "html holds only head and body"))
  (check (rejects? '(haml (:html "text" (:body))) "text in html would be moved"))
  (check (rejects? '(haml (:html (if x (:body) (:head)) (:body))) "html holds at most one body"))
  (check (rejects? '(haml (:html (for-each (lambda (x) (:body)) xs))) "html holds at most one body"))
  (check (not (compile-error '(let ([x #t]) (haml (:html (:head) (if x (:body.a) (:body.b)))))))))

(test "head holds only metadata elements, and its noscript only link, meta and style"
  (check-equal? "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>t</title><link rel=\"x\"><noscript><style>a{}</style></noscript></head></html>"
    (html->string (haml (:html (:head (:meta ([charset "utf-8"])) (:title "t") (:link ([rel "x"]))
                                      (:noscript (:style "a{}")))))))
  (check (rejects? '(haml (:html (:head (:title "t") (:div "x")))) "head cannot hold div"))
  (check (rejects? '(haml (:html (:head (:my-meta)))) "head cannot hold my-meta"))
  (check (rejects? '(haml (:html (:head "text"))) "text in head would be moved"))
  (check (rejects? '(haml (:head (:noscript (:p "no js")))) "a noscript in head holds only link, meta and style"))
  (check (rejects? '(haml (:head (:noscript "x"))) "text in noscript would be moved")))

(test "expressions in html and head must give html values"
  (let ([extra (haml (:link ([rel "stylesheet"] [href "/a.css"])))] [body (haml (:body "b"))])
    (check-equal? "<!DOCTYPE html><html><head><title>t</title><link rel=\"stylesheet\" href=\"/a.css\"></head><body>b</body></html>"
      (html->string (haml (:html (:head (:title "t") extra) body)))))
  (raises-value-error "text here would be moved by the parser"
    (let ([s "x"]) (haml (:html (:head s)))))
  (raises-value-error "text here would be moved by the parser"
    (let ([n 1]) (haml (:html n)))))

(test "head and body are allowed at a form's top and in html only"
  (check-equal? "<head><title>t</title></head>" (html->string (haml (:head (:title "t")))))
  (check-equal? "<body class=\"x\"></body>" (html->string (haml (:body.x))))
  (check (rejects? '(haml (:div (:body))) "body can be only at the top of a form or inside html"))
  (check (rejects? '(haml (:div (:head))) "head can be only at the top of a form or inside html"))
  (check (rejects? '(haml (:body (:head))) "head can be only at the top of a form or inside html")))

(test-exit)
