(import (chezscheme) (check) (support) (html))

(test "tag specs"
  (check-equal? "<div class=\"a b\"></div>" (html->string (html (:div.a.b))))
  (check-equal? "<div class=\"-mt-2 2xl:p-4 @container\"></div>"
    (html->string (html (:div.-mt-2.2xl:p-4.@container))))
  (check (rejects? '(html (:)) "missing tag name after :"))
  (check (rejects? '(html (:.card "x")) "missing tag name after :"))
  (check (rejects? '(html (:div..card)) "empty class name in tag"))
  (check (rejects? '(html (:div.)) "empty class name in tag")))

(test "an element is a list headed by its tag"
  (check (rejects? '(html (:p :br)) "an element is written in parentheses"))
  (check (rejects? '(html ("div" "x")) "a list cannot start with a string")))

(test "child literals are strings and exact integers"
  (check (rejects? '(html (:p #\a)) "child literal must be a string or an exact integer"))
  (check (rejects? '(html (:p 1.5)) "child literal must be a string or an exact integer"))
  (check (rejects? '(html (:p #t)) "child literal must be a string or an exact integer"))
  (check (rejects? '(html (:p 1/2)) "child literal must be a string or an exact integer"))
  (check (rejects? '(html (:p 'sym)) "a quoted datum cannot be a child")))

(test "attribute lists"
  (check-equal? "<p></p>" (html->string (html (:p ()))))
  (check-equal? "<p>x</p>" (html->string (html (:p [] "x"))))
  (check (rejects? '(html (:a ([href 'x]))) "a quoted datum cannot be an attribute value"))
  (check (rejects? '(html (:a ([href 1.5]))) "attribute value literal must be a string or an exact integer"))
  (check (rejects? '(html (:a ([href "/x/" #f]))) "write [name], or leave the attribute out"))
  (check (rejects? '(html (:input ([autofocus #t]))) "write [name], or leave the attribute out"))
  (check (rejects? '(html (:input ([autofocus #f]))) "write [name], or leave the attribute out"))
  (check (rejects? '(html (:a ([1 "x"]))) "attribute name must be an identifier or a string"))
  (check (rejects? '(html (:a ([(f x) "y"]))) "attribute name must be an identifier or a string"))
  (check (rejects? '(html (:a (["on click" "x"]))) "invalid attribute name"))
  (check (rejects? '(html (:a ([a=b "x"]))) "invalid attribute name"))
  (check (rejects? '(html (:a (["<x" "x"]))) "invalid attribute name"))
  (check (rejects? '(html (:a (["" "x"]))) "invalid attribute name"))
  (check (rejects? '(html (:a ([href "x"] []))) "attribute must be [name part ...]")))

(test "a list of elements after the tag reads as an attribute list, and is refused"
  (check (rejects? '(html (:div ((:h1 "Title") (:p "Body"))))
           "this reads as markup, not an attribute name"))
  (check (rejects? '(html (:ul ((:li "a") (:li "b"))))
           "give an element its own parentheses"))
  (check-equal? "<a :href=\"x\"></a>" (html->string (html (:a ([":href" "x"]))))))

(test "a call whose parts are all calls reads as an attribute list, and is refused"
  (check (rejects? '(html (:td ((formatter col) (cell-value row col))))
           "wrap it in (begin …)"))
  (check-equal? "<td>x</td>"
    (let ([formatter (lambda (c) (lambda (v) v))] [cell-value (lambda (r c) "x")])
      (html->string (html (:td (begin ((formatter 1) (cell-value 2 1)))))))))

(test "malformed control forms"
  (check (rejects? '(html (if #t "x")) "if needs both arms"))
  (check (rejects? '(html (if)) "expected (if test child child)"))
  (check (rejects? '(html (if a b c d)) "expected (if test child child)"))
  (check (rejects? '(html (when)) "expected (when test child ...)"))
  (check (rejects? '(html (unless)) "expected (unless test child ...)"))
  (check (rejects? '(html (when #t)) "expected at least one child"))
  (check (rejects? '(html (let ([x 1]))) "expected at least one child"))
  (check (rejects? '(html (let ([1 2]) "x")) "expected (let ([name expression] ...) child ...)"))
  (check (rejects? '(html (let* ([x]) "x")) "expected (let* ([name expression] ...) child ...)"))
  (check (rejects? '(html (cond [x => f])) "=> is not allowed in markup"))
  (check (rejects? '(html (cond x)) "expected [test child ...] in cond"))
  (check (rejects? '(html (cond [x])) "expected at least one child"))
  (check (rejects? '(html (cond [else "a"] [x "b"])) "else must be the last clause"))
  (check (rejects? '(html (case k [x "y"])) "expected [(datum ...) child ...] in case"))
  (check (rejects? '(html (case k [(1)])) "expected at least one child"))
  (check (rejects? '(html (case)) "expected (case key [(datum ...) child ...] ...)"))
  (check (rejects? '(html (for-each f xs)) "one list per name"))
  (check (rejects? '(html (for-each (lambda (x y) (:li x)) xs)) "one list per name"))
  (check (rejects? '(html (for-each (lambda () "x"))) "one list per name"))
  (check (rejects? '(html (for-each (lambda (x)) xs)) "expected at least one child"))
  (check (rejects? '(html (:p . "x")) "expected (:tag child ...) or (:tag (attribute ...) child ...)"))
  (check (rejects? '(html (:p "a" . "x")) "expected (:tag child ...)"))
  (check (rejects? '(html (:p (begin . x))) "expected (begin child ...)")))

(define (compile-error-subform datum)
  (guard (c [(syntax-violation? c) (syntax->datum (syntax-violation-subform c))])
    (expand datum (environment '(chezscheme) '(html)))
    #f))

(test "the first malformed piece is reported"
  (check-equal? 1.5 (compile-error-subform '(html (:p 1.5 #\a 2.5))))
  (check-equal? 1.5 (compile-error-subform '(html (:p ([href 1.5] [src #\a] [alt 2.5])))))
  (check-equal? ':dvi (compile-error-subform '(html (:dvi) (:spn) (:dv)))))

(test "definitions are not children"
  (check (rejects? '(html (let ([x 1]) (define y 2) (:p y))) "a definition cannot be a child"))
  (check (rejects? '(html (:p (define-values (a) (values 1)))) "a definition cannot be a child")))

(test "a named let is an expression"
  (check-equal? "<p>3</p>"
    (html->string (html (:p (let loop ([i 0]) (if (= i 3) i (loop (+ i 1)))))))))

(test "markup inside a Scheme expression needs its own form"
  (check (rejects? '(html (:ul (map (lambda (x) (:li x)) xs))) "needs its own (html …)"))
  (check (rejects? '(html (:p ([title (:b "x")]))) "needs its own (html …)"))
  (check (rejects? '(html (when (:b) "x")) "needs its own (html …)"))
  (check (not (compile-error '(html (:p (length '((:li "x"))))))))
  (check (not (compile-error '(html (:p (length `((:li "x"))))))))
  (check (not (compile-error '(html (:p (length (list #'(:li "x"))))))))
  (check (not (compile-error '(html (:p (length (list #`(:li "x"))))))))
  (check (not (compile-error '(html (:ul (map (lambda (x) (html (:li x))) '(1)))))))
  (check (not (compile-error '(let ([f list] [:not-an-element 1]) (html (:p (f :not-an-element))))))))

(test "forms are recognised by binding, not by name"
  (check-equal? "<p>no</p>"
    (let ([when (lambda (c x) (if c x "no"))])
      (html->string (html (:p (when #f "yes"))))))
  (check-equal? "<ul>mine</ul>"
    (let ([for-each (lambda (f l) "mine")])
      (html->string (html (:ul (for-each (lambda (x) "y") '(1)))))))
  (check-equal? "<p>ab</p>"
    (evaluate
      '(let ()
         (import (rename (only (chezscheme) when) (when quand)))
         (html->string (html (:p (quand #t "a" "b"))))))))

(test "case under (chezscheme) and under (rnrs) compares as imported"
  (check-equal? "<p>yes</p>"
    (html->string (html (:p (case "a" [("a") "yes"] [else "no"])))))
  (check-equal? "<p>no</p>"
    (evaluate
      '(let ()
         (import (only (rnrs) case else))
         (html->string (html (:p (case (string #\a) [("a") "yes"] [else "no"])))))))
  (check-equal? "<p><b></b>b</p>"
    (evaluate
      '(let ()
         (import (only (rnrs) case))
         (html->string (html (:p (case 2 [(1) "a"] [(2 3) (:b) "b"]))))))))

(test "a name bound by markup shadows the recognised forms"
  (check-equal? "<p>chosen</p>"
    (html->string
      (html (:p (let ([when (lambda (c x) "chosen")]) (when #f "yes"))))))
  (check-equal? "<p>xy</p>"
    (html->string
      (html (:p (for-each (lambda (case) (case "x")) (list (lambda (s) (string-append s "y"))))))))
  (check-equal? "<p>1</p>"
    (html->string (html (:p (let* ([if (lambda (a b c) b)]) (if #f 1 2))))))
  (check-equal? "<p>t</p>"
    (html->string (html (:p (let ([else #f]) (cond [else "a"] [#t "t"]))))))
  (check-equal? "<p>1a</p>"
    (html->string (html (:p (let ([=> 1]) (cond [#t => "a"]))))))
  (check (rejects? '(html (:p (let ([lambda list]) (for-each (lambda (x) "a") (list 1)))))
           "expected (for-each (lambda (name ...) child ...) list ...)")))

(test-exit)
