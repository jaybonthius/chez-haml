# chez-haml

HTML written as Scheme code, for Chez Scheme 10.4.1. The `haml` form is checked against the WHATWG HTML standard when the code compiles, and expands into code that writes UTF-8 bytes.

```scheme
(import (chezscheme) (haml))

(define (todo-item t)
  (haml
    (:li.todo ([class (and (todo-done? t) "done")])
      (todo-title t))))

(define (todo-page todos)
  (haml
    (:html ([lang "en"])
      (:head (:title "Todos"))
      (:body
        (if (null? todos)
            (:p.empty "Nothing to do.")
            (:ul (map todo-item todos)))))))

(write-html (todo-page todos) port)
```

## Building

```sh
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
```

Programs import `(haml)` with `--libdirs src::build/obj/src`.

## Syntax

An element is `(:tag child ...)`. Strings are escaped; exact integers are written as digits.

```scheme
(haml (:h1 "Hello, " "<world>"))     ; <h1>Hello, &lt;world&gt;</h1>
```

Classes follow the tag, separated by dots. There is no id shorthand and no implicit `div`.

```scheme
(haml (:h1.title.is-large "Hello"))  ; <h1 class="title is-large">Hello</h1>
```

Attributes go in a list after the tag. A value with several parts is concatenated. `[name]` writes a bare attribute; a value of `#t` writes it bare and `#f` leaves it out. `class` entries are added to the dotted classes.

```scheme
(haml (:a ([href "/items/" 42] [title "Item"]) "details"))
;; <a href="/items/42" title="Item">details</a>

(haml (:input ([type "checkbox"] [checked done?] [disabled locked?] [data-bind:done])))
;; <input type="checkbox" checked data-bind:done>

(haml (:li.todo ([class (and done? "done")]) "Buy milk"))
;; <li class="todo done">Buy milk</li>
```

An attribute name written as an identifier must be a known HTML or SVG attribute, `data-*`, `aria-*`, `on*`, or contain `:`. Write any other name as a string, and classes that are not symbols (`w-1.5`, `w-[10px]`) in a `class` string:

```scheme
(haml (:button (["@click" "open = true"] ["hx-post" "/save"]) "Save"))
```

Any other form is a Scheme expression. Its value is written by type: a string, an exact integer, an `html` value, or a list of these. Anything else raises an error with the expression's line.

```scheme
(define (todo-item title) (haml (:li title)))
(haml (:ul (map todo-item titles)))  ; <ul><li>a</li><li>b</li></ul>
```

`when`, `unless`, `if`, `cond`, `case`, `let`, `let*`, `begin`, and `for-each` with a `lambda` hold markup directly. Markup inside any other expression needs its own `(haml …)`, as in `todo-item` above.

```scheme
(haml (:ul (for-each (lambda (t) (:li t)) titles)))
(haml (:div (when (null? todos) (:p.empty "Nothing to do."))))
```

`:html` must be a form's only child; `<!DOCTYPE html>` is written before it. An SVG element at the top of a form starts SVG, so SVG fragments can be components:

```scheme
(haml (:circle ([cx 3] [cy 4] [r 2])))  ; <circle cx="3" cy="4" r="2"></circle>
```

## Procedures

| Procedure | Returns |
|---|---|
| `(html? x)` | whether `x` is an `html` value |
| `(write-html h port)` | writes `h` to a binary output port, without flushing it |
| `(html->bytevector h)` | the UTF-8 bytes |
| `(html->string h)` | the text |
| `(raw-html s)` | a value that writes the string or UTF-8 bytevector `s` unchecked |

## Checked when the code compiles

- Unknown and obsolete elements, and unknown attribute names.
- Void elements with children, duplicate attributes, upper case in HTML attribute names.
- Nesting that the HTML parser would restructure: a `div` inside a `p`, a `tr` directly inside a `table`, a link inside a link, text in a table.
- The structure of `html`, `head` and `body`.
- SVG element names, contexts and attribute spellings.
- `script` and `style` text that contains its own end tag.

A `haml` form written inside an element is checked as that element's child; a component defined elsewhere is not checked where it is inserted.

## Behaviour when it runs

A form's expressions are evaluated, in the order written, when the form is evaluated. The bytes are written later, by `write-html` or `html->bytevector`; a nested value writes into the same port, so no bytes are copied. Text escapes `& < >`, and attribute values escape `& < > "`. NUL is written as U+FFFD, CR and CRLF as LF.

HTML escaping does not make data safe inside JavaScript: a Datastar or Alpine expression, a `javascript:` URL, or JSON in a `script`. Strings and lists are not copied, so changing them after the form is evaluated changes the output.
