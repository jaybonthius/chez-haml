(library (haml rules)
  (export
    ;; elements
    html-element-name? svg-element-name? svg-only-element-name? custom-element-name?
    known-element-name? element-kind newline-after-start-tag? html-integration-point?
    table-text-element?
    ;; attributes
    valid-attribute-name? known-attribute-name? adjusted-svg-attribute-name?
    lower-case-of-adjusted-svg-attribute? foreign-attribute-name? reserved-foreign-prefix?
    svg-namespace xlink-namespace
    ;; nesting
    make-ancestor ancestor-namespace ancestor-name ancestor-first-child ancestor-location
    template-mode-setter? text-allowed-in-template-after? text-allowed-at-top-after?
    nesting-problem
    ;; text
    raw-text-patterns html-whitespace? ascii-upper-case? ascii-downcase)
  (import (chezscheme))

  (define (string-set names)
    (let ([table (make-hashtable string-hash string=?)])
      (for-each (lambda (n) (hashtable-set! table n #t)) names)
      table))

  (define (in? table name) (hashtable-ref table name #f))

  ;; HTML Standard, index, "List of elements" (indices.html#elements-3),
  ;; without math and svg, which start foreign content.
  (define html-elements
    (string-set
      '("a" "abbr" "address" "area" "article" "aside" "audio" "b" "base" "bdi" "bdo" "blockquote"
        "body" "br" "button" "canvas" "caption" "cite" "code" "col" "colgroup" "data" "datalist"
        "dd" "del" "details" "dfn" "dialog" "div" "dl" "dt" "em" "embed" "fieldset" "figcaption"
        "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html"
        "i" "iframe" "img" "input" "ins" "kbd" "label" "legend" "li" "link" "main" "map" "mark"
        "menu" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup" "option" "output" "p"
        "picture" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp" "script" "search" "section"
        "select" "selectedcontent" "slot" "small" "source" "span" "strong" "style" "sub" "summary"
        "sup" "table" "tbody" "td" "template" "textarea" "tfoot" "th" "thead" "time" "title" "tr"
        "track" "u" "ul" "var" "video" "wbr")))

  ;; SVG 2, element index (eltindex.html), in the spelling the parser's case
  ;; adjustment restores (HTML Standard §13.2.6.5).
  (define svg-elements
    (string-set
      '("a" "animate" "animateMotion" "animateTransform" "circle" "clipPath" "defs" "desc" "ellipse"
        "feBlend" "feColorMatrix" "feComponentTransfer" "feComposite" "feConvolveMatrix"
        "feDiffuseLighting" "feDisplacementMap" "feDistantLight" "feDropShadow" "feFlood" "feFuncA"
        "feFuncB" "feFuncG" "feFuncR" "feGaussianBlur" "feImage" "feMerge" "feMergeNode"
        "feMorphology" "feOffset" "fePointLight" "feSpecularLighting" "feSpotLight" "feTile"
        "feTurbulence" "filter" "foreignObject" "g" "image" "line" "linearGradient" "marker" "mask"
        "metadata" "mpath" "path" "pattern" "polygon" "polyline" "radialGradient" "rect" "script"
        "set" "stop" "style" "svg" "switch" "symbol" "text" "textPath" "title" "tspan" "use"
        "view")))

  (define (html-element-name? name) (in? html-elements name))
  (define (svg-element-name? name) (in? svg-elements name))

  ;; a, script, style and title are HTML names too, and svg is entered from
  ;; HTML, so none of them marks a form's top as SVG.
  (define (svg-only-element-name? name)
    (and (in? svg-elements name)
         (not (member name '("a" "script" "style" "title" "svg")))))

  (define (known-element-name? name)
    (or (in? html-elements name) (in? svg-elements name) (string=? name "svg")))

  (define (ascii-upper-case? c) (char<=? #\A c #\Z))
  (define (ascii-lower-case? c) (char<=? #\a c #\z))

  ;; The parser lowers only ASCII letters in names; string-downcase would
  ;; also fold letters it keeps apart.
  (define (ascii-downcase s)
    (list->string
      (map (lambda (c) (if (ascii-upper-case? c) (char-downcase c) c)) (string->list s))))

  ;; HTML Standard §13.2.2, ASCII whitespace.
  (define (html-whitespace? c)
    (memv c '(#\tab #\newline #\page #\return #\space)))

  ;; HTML Standard §4.13.3, valid custom element name, with DOM's valid
  ;; element local name for a name that starts with an ASCII letter.
  (define reserved-custom-element-names
    '("annotation-xml" "color-profile" "font-face" "font-face-src" "font-face-uri"
      "font-face-format" "font-face-name" "missing-glyph"))

  (define (custom-element-name? name)
    (let ([n (string-length name)])
      (and (fx> n 0)
           (ascii-lower-case? (string-ref name 0))
           (let loop ([i 0] [hyphen? #f])
             (if (fx= i n)
                 hyphen?
                 (let ([c (string-ref name i)])
                   (and (not (ascii-upper-case? c))
                        (not (html-whitespace? c))
                        (not (memv c '(#\nul #\/ #\>)))
                        (loop (fx+ i 1) (or hyphen? (char=? c #\-)))))))
           (not (member name reserved-custom-element-names)))))

  ;; HTML Standard §13.1.2, kinds of elements; iframe and selectedcontent
  ;; have content model nothing, and the parser reads an iframe's content as
  ;; raw text and replaces a selectedcontent's children.
  (define void-elements
    (string-set '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "source"
                  "track" "wbr")))

  (define (element-kind namespace name)
    (cond
      [(eq? namespace 'svg) 'normal]
      [(in? void-elements name) 'void]
      [(member name '("script" "style")) 'raw-text]
      [(member name '("textarea" "title")) 'escapable-text]
      [(member name '("iframe" "selectedcontent")) 'no-children]
      [else 'normal]))

  ;; HTML Standard §13.2.5.3 to §13.2.5.31, matched ASCII case-insensitively:
  ;; the element's end tag ends its text, and <!-- in script changes how the
  ;; rest is tokenized. </noscript ends an enclosing noscript, which the
  ;; parser reads as raw text when scripting is enabled.
  (define raw-text-pattern-table
    '(("script" "</script" "<!--" "</noscript")
      ("style" "</style" "</noscript")))

  (define (raw-text-patterns name) (cdr (assoc name raw-text-pattern-table)))

  ;; HTML Standard §13.2.6.4.7: the parser drops one newline after these
  ;; start tags.
  (define (newline-after-start-tag? name) (member name '("pre" "textarea")))

  ;; HTML Standard §13.2.6.2, HTML integration points.
  (define (html-integration-point? name) (member name '("foreignObject" "desc" "title")))

  ;; HTML Standard §13.2.6.4.9 and §13.2.6.4.4: text in these is moved.
  (define (table-text-element? name)
    (member name '("table" "tbody" "thead" "tfoot" "tr" "colgroup" "html" "head")))

  ;; HTML Standard §13.1.2.3, attribute name syntax, also refusing "<",
  ;; which the tokenizer reports as an error.
  (define (valid-attribute-name? name)
    (and (fx> (string-length name) 0)
         (for-all
           (lambda (c)
             (let ([n (char->integer c)])
               (not (or (fx< n #x20)
                        (fx<= #x7F n #x9F)
                        (memv c '(#\space #\" #\' #\> #\/ #\= #\<))
                        (fx<= #xFDD0 n #xFDEF)
                        (fx= (fxand n #xFFFE) #xFFFE)))))
           (string->list name))))

  ;; HTML Standard, index, "List of attributes" (indices.html#attributes-3).
  (define html-attributes
    '("abbr" "accept" "accept-charset" "accesskey" "action" "allow" "allowfullscreen" "alpha"
      "alt" "as" "async" "autocapitalize" "autocomplete" "autocorrect" "autofocus" "autoplay"
      "blocking" "charset" "checked" "cite" "class" "closedby" "color" "colorspace" "cols"
      "colspan" "command" "commandfor" "content" "contenteditable" "controls" "coords"
      "crossorigin" "data" "datetime" "decoding" "default" "defer" "dir" "dirname" "disabled"
      "download" "draggable" "enctype" "enterkeyhint" "fetchpriority" "for" "form" "formaction"
      "formenctype" "formmethod" "formnovalidate" "formtarget" "headers" "headingoffset"
      "headingreset" "height" "hidden" "high" "href" "hreflang" "http-equiv" "id" "imagesizes"
      "imagesrcset" "inert" "inputmode" "integrity" "is" "ismap" "itemid" "itemprop" "itemref"
      "itemscope" "itemtype" "kind" "label" "lang" "list" "loading" "loop" "low" "max" "maxlength"
      "media" "method" "min" "minlength" "multiple" "muted" "name" "nomodule" "nonce" "novalidate"
      "open" "optimum" "pattern" "ping" "placeholder" "playsinline" "popover" "popovertarget"
      "popovertargetaction" "poster" "preload" "readonly" "referrerpolicy" "rel" "required"
      "reversed" "rows" "rowspan" "sandbox" "scope" "selected" "shadowrootclonable"
      "shadowrootcustomelementregistry" "shadowrootdelegatesfocus" "shadowrootmode"
      "shadowrootserializable" "shadowrootslotassignment" "shape" "size" "sizes" "slot" "span"
      "spellcheck" "src" "srcdoc" "srclang" "srcset" "start" "step" "style" "tabindex" "target"
      "title" "translate" "type" "usemap" "value" "width" "wrap" "writingsuggestions"))

  ;; SVG 2, Appendix G, attribute index (attindex.html), regular and
  ;; presentation attributes.
  (define svg-attributes
    '("accumulate" "additive" "alignment-baseline" "amplitude" "aria-activedescendant"
      "aria-atomic" "aria-autocomplete" "aria-busy" "aria-checked" "aria-colcount" "aria-colindex"
      "aria-colspan" "aria-controls" "aria-current" "aria-describedby" "aria-details"
      "aria-disabled" "aria-dropeffect" "aria-errormessage" "aria-expanded" "aria-flowto"
      "aria-grabbed" "aria-haspopup" "aria-hidden" "aria-invalid" "aria-keyshortcuts" "aria-label"
      "aria-labelledby" "aria-level" "aria-live" "aria-modal" "aria-multiline"
      "aria-multiselectable" "aria-orientation" "aria-owns" "aria-placeholder" "aria-posinset"
      "aria-pressed" "aria-readonly" "aria-relevant" "aria-required" "aria-roledescription"
      "aria-rowcount" "aria-rowindex" "aria-rowspan" "aria-selected" "aria-setsize" "aria-sort"
      "aria-valuemax" "aria-valuemin" "aria-valuenow" "aria-valuetext" "attributeName" "autofocus"
      "azimuth" "baseFrequency" "baseline-shift" "begin" "bias" "by" "calcMode" "class" "clip"
      "clip-path" "clip-rule" "clipPathUnits" "color" "color-interpolation"
      "color-interpolation-filters" "crossorigin" "cursor" "cx" "cy" "diffuseConstant" "direction"
      "display" "divisor" "dominant-baseline" "download" "dur" "dx" "dy" "edgeMode" "elevation"
      "end" "exponent" "fill" "fill-opacity" "fill-rule" "filter" "filterUnits" "flood-color"
      "flood-opacity" "font-family" "font-size" "font-size-adjust" "font-stretch" "font-style"
      "font-variant" "font-weight" "fr" "from" "fx" "fy" "glyph-orientation-horizontal"
      "glyph-orientation-vertical" "gradientTransform" "gradientUnits" "height" "href" "hreflang"
      "id" "image-rendering" "in" "in2" "intercept" "k1" "k2" "k3" "k4" "kernelMatrix"
      "kernelUnitLength" "keyPoints" "keySplines" "keyTimes" "lang" "lengthAdjust"
      "letter-spacing" "lighting-color" "limitingConeAngle" "marker-end" "marker-mid"
      "marker-start" "markerHeight" "markerUnits" "markerWidth" "mask" "mask-type"
      "maskContentUnits" "maskUnits" "max" "media" "method" "min" "mode" "numOctaves" "offset"
      "onabort" "onafterprint" "onbeforeprint" "onbegin" "oncancel" "oncanplay" "oncanplaythrough"
      "onchange" "onclick" "onclose" "oncopy" "oncuechange" "oncut" "ondblclick" "ondrag"
      "ondragend" "ondragenter" "ondragexit" "ondragleave" "ondragover" "ondragstart" "ondrop"
      "ondurationchange" "onemptied" "onend" "onended" "onerror" "onfocus" "onhashchange"
      "oninput" "oninvalid" "onkeydown" "onkeypress" "onkeyup" "onload" "onloadeddata"
      "onloadedmetadata" "onloadstart" "onmessage" "onmousedown" "onmouseenter" "onmouseleave"
      "onmousemove" "onmouseout" "onmouseover" "onmouseup" "onoffline" "ononline" "onpagehide"
      "onpageshow" "onpaste" "onpause" "onplay" "onplaying" "onpopstate" "onprogress"
      "onratechange" "onrepeat" "onreset" "onresize" "onscroll" "onseeked" "onseeking" "onselect"
      "onshow" "onstalled" "onstorage" "onsubmit" "onsuspend" "ontimeupdate" "ontoggle" "onunload"
      "onvolumechange" "onwaiting" "onwheel" "opacity" "operator" "order" "orient" "origin"
      "overflow" "paint-order" "path" "path-length" "pathLength" "patternContentUnits"
      "patternTransform" "patternUnits" "ping" "playbackorder" "pointer-events" "points"
      "pointsAtX" "pointsAtY" "pointsAtZ" "preserveAlpha" "preserveAspectRatio" "primitiveUnits"
      "r" "radius" "refX" "refY" "referrerpolicy" "rel" "repeatCount" "repeatDur"
      "requiredExtensions" "restart" "result" "role" "rotate" "scale" "seed" "shape-rendering"
      "side" "slope" "spacing" "specularConstant" "specularExponent" "spreadMethod" "startOffset"
      "stdDeviation" "stitchTiles" "stop-color" "stop-opacity" "stroke" "stroke-dasharray"
      "stroke-dashoffset" "stroke-linecap" "stroke-linejoin" "stroke-miterlimit" "stroke-opacity"
      "stroke-width" "style" "surfaceScale" "systemLanguage" "tabindex" "tableValues" "target"
      "targetX" "targetY" "text-anchor" "text-decoration" "text-rendering" "textLength"
      "timelinebegin" "title" "to" "transform" "transform-origin" "type" "unicode-bidi" "values"
      "vector-effect" "viewBox" "visibility" "width" "word-spacing" "writing-mode" "x" "x1" "x2"
      "xChannelSelector" "xlink:href" "xlink:title" "xml:space" "y" "y1" "y2" "yChannelSelector"
      "z"))

  ;; SVG 2 §6.6, geometry properties, which are presentation attributes
  ;; missing from the attribute index.
  (define svg-geometry-attributes '("cx" "cy" "r" "rx" "ry" "x" "y" "width" "height" "d"))

  ;; HTML Standard §13.2.6.1, adjust foreign attributes: the only namespaced
  ;; attributes the HTML syntax can express.
  (define foreign-attributes
    (string-set '("xlink:actuate" "xlink:arcrole" "xlink:href" "xlink:role" "xlink:show"
                  "xlink:title" "xlink:type" "xml:lang" "xml:space" "xmlns" "xmlns:xlink")))

  ;; Shadow parts for custom elements, and RDFa's property for Open Graph
  ;; meta tags.
  (define other-attributes '("part" "exportparts" "property"))

  (define known-attributes
    (string-set (append html-attributes svg-attributes svg-geometry-attributes
                        (vector->list (hashtable-keys foreign-attributes)) other-attributes)))

  (define (prefixed? name prefix)
    (let ([n (string-length prefix)])
      (and (fx> (string-length name) n) (string=? (substring name 0 n) prefix))))

  (define (known-attribute-name? name)
    (or (in? known-attributes name)
        (prefixed? name "data-")
        (prefixed? name "aria-")
        (prefixed? name "on")
        (and (memv #\: (string->list name)) #t)))

  (define adjusted-svg-attributes
    (string-set
      (filter (lambda (n) (exists ascii-upper-case? (string->list n))) svg-attributes)))

  (define lower-case-adjusted-svg-attributes
    (string-set (map ascii-downcase (vector->list (hashtable-keys adjusted-svg-attributes)))))

  (define (adjusted-svg-attribute-name? name) (in? adjusted-svg-attributes name))

  (define (lower-case-of-adjusted-svg-attribute? name)
    (in? lower-case-adjusted-svg-attributes name))

  (define (foreign-attribute-name? name) (in? foreign-attributes name))

  (define (reserved-foreign-prefix? name)
    (or (string=? name "xmlns")
        (prefixed? name "xmlns:")
        (prefixed? name "xlink:")
        (prefixed? name "xml:")))

  (define svg-namespace "http://www.w3.org/2000/svg")
  (define xlink-namespace "http://www.w3.org/1999/xlink")

  (define-record-type ancestor
    (nongenerative)
    (sealed #t)
    (fields namespace name first-child location))

  (define (html-named? a name)
    (and (eq? (ancestor-namespace a) 'html) (string=? (ancestor-name a) name)))

  (define (html-named-in? a names)
    (and (eq? (ancestor-namespace a) 'html) (member (ancestor-name a) names) #t))

  ;; HTML Standard §13.2.4.2, the stack of open elements.
  (define default-scope
    '((html "applet") (html "caption") (html "html") (html "table") (html "td") (html "th")
      (html "marquee") (html "object") (html "select") (html "template")
      (mathml "mi") (mathml "mo") (mathml "mn") (mathml "ms") (mathml "mtext")
      (mathml "annotation-xml")
      (svg "foreignObject") (svg "desc") (svg "title")))

  (define button-scope (cons '(html "button") default-scope))

  (define special-elements
    (append
      (map (lambda (n) (list 'html n))
           '("address" "applet" "area" "article" "aside" "base" "basefont" "bgsound" "blockquote"
             "body" "br" "button" "caption" "center" "col" "colgroup" "dd" "details" "dir" "div"
             "dl" "dt" "embed" "fieldset" "figcaption" "figure" "footer" "form" "frame" "frameset"
             "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html" "iframe" "img"
             "input" "keygen" "li" "link" "listing" "main" "marquee" "menu" "meta" "nav" "noembed"
             "noframes" "noscript" "object" "ol" "p" "param" "plaintext" "pre" "script" "search"
             "section" "select" "source" "style" "summary" "table" "tbody" "td" "template"
             "textarea" "tfoot" "th" "thead" "title" "tr" "track" "ul" "wbr" "xmp"))
      '((mathml "mi") (mathml "mo") (mathml "mn") (mathml "ms") (mathml "mtext")
        (mathml "annotation-xml")
        (svg "foreignObject") (svg "desc") (svg "title"))))

  (define (listed? a elements)
    (and (member (list (ancestor-namespace a) (ancestor-name a)) elements) #t))

  ;; HTML Standard §13.2.4.3: entering these inserts a marker in the list of
  ;; active formatting elements.
  (define markers '("applet" "object" "marquee" "template" "td" "th" "caption"))

  ;; HTML Standard §13.2.6.4.7: start tags that close a p in button scope.
  (define p-closers
    '("address" "article" "aside" "blockquote" "center" "details" "dialog" "dir" "div" "dl"
      "fieldset" "figcaption" "figure" "footer" "header" "hgroup" "main" "menu" "nav" "ol" "p"
      "search" "section" "summary" "ul" "h1" "h2" "h3" "h4" "h5" "h6" "pre" "listing" "form"
      "li" "dd" "dt" "plaintext" "table" "hr" "xmp"))

  (define headings '("h1" "h2" "h3" "h4" "h5" "h6"))

  ;; HTML Standard §13.2.6.3, elements that have implied end tags, without
  ;; the obsolete rb and rtc.
  (define implied-end-tags '("dd" "dt" "li" "optgroup" "option" "p" "rp" "rt"))

  ;; The scope algorithm of §13.2.4.2: the target is tested before the list.
  (define (find-in-scope ancestors target? boundary?)
    (let loop ([as ancestors])
      (cond
        [(null? as) #f]
        [(target? (car as)) (car as)]
        [(boundary? (car as)) #f]
        [else (loop (cdr as))])))

  (define (in-scope ancestors name scope)
    (find-in-scope ancestors (lambda (a) (html-named? a name)) (lambda (a) (listed? a scope))))

  ;; What the parser keeps directly inside each table element (§13.2.6.4.9
  ;; to §13.2.6.4.14); an empty form and a hidden input are kept too, except
  ;; in colgroup.
  (define table-children
    '(("table" "caption" "colgroup" "tbody" "thead" "tfoot" "script" "style" "template")
      ("tbody" "tr" "script" "style" "template")
      ("thead" "tr" "script" "style" "template")
      ("tfoot" "tr" "script" "style" "template")
      ("tr" "td" "th" "script" "style" "template")
      ("colgroup" "col" "template")))

  (define table-parts '("caption" "colgroup" "tbody" "thead" "tfoot" "col" "tr" "td" "th"))

  (define (table-part? name) (and (member name table-parts) #t))

  (define (kept-in-table? parent name form-kept? hidden-input?)
    (or (and (member name (cdr (assoc parent table-children))) #t)
        (and (not (string=? parent "colgroup"))
             (or (and (string=? name "form") form-kept?)
                 (and (string=? name "input") hidden-input?)))))

  ;; HTML Standard §13.2.6.4.16: the first start tag in a template, other
  ;; than these, sets the template's insertion mode.
  (define (template-mode-setter? name)
    (not (member name '("base" "link" "meta" "script" "style" "template" "title"))))

  (define (template-mode first-child)
    (cond
      [(not first-child) #f]
      [(member first-child '("caption" "colgroup" "tbody" "thead" "tfoot")) "table"]
      [(string=? first-child "col") "colgroup"]
      [(string=? first-child "tr") "tbody"]
      [(member first-child '("td" "th")) "tr"]
      [else "body"]))

  (define (template-part-levels mode)
    (cond
      [(string=? mode "table") '("caption" "colgroup" "tbody" "thead" "tfoot")]
      [(string=? mode "tbody") '("tr")]
      [(string=? mode "tr") '("td" "th")]
      [else '()]))

  ;; In column-group mode the parser ignores everything but col and
  ;; template; in the other table modes it keeps elements and text, but not
  ;; table parts of another level. At a form's top a table context is
  ;; possible, which would also foster-parent those elements and text.
  (define (allowed-in-template-after? mode name)
    (cond
      [(string=? mode "colgroup") (and (member name '("col" "template")) #t)]
      [(string=? mode "body") (not (table-part? name))]
      [else (or (not (table-part? name))
                (and (member name (template-part-levels mode)) #t))]))

  (define (allowed-at-top-after? mode name form-kept? hidden-input?)
    (if (member mode '("colgroup" "body"))
        (allowed-in-template-after? mode name)
        (kept-in-table? mode name form-kept? hidden-input?)))

  (define (text-allowed-in-template-after? first-child)
    (not (equal? (template-mode first-child) "colgroup")))

  (define (text-allowed-at-top-after? first-child)
    (let ([mode (template-mode first-child)])
      (or (not mode) (string=? mode "body"))))

  ;; HTML Standard §13.2.6.4.4 and §13.2.6.4.5.
  (define (head-child? name)
    (and (member name '("base" "link" "meta" "noscript" "script" "style" "template" "title")) #t))

  (define (head-noscript-child? name) (and (member name '("link" "meta" "style")) #t))

  (define (nesting-problem name ancestors top-first-child empty? hidden-input?)
    (define parent (and (pair? ancestors) (car ancestors)))
    (define (problem message culprit) (values message culprit))
    (define (parent-is? names) (and parent (html-named-in? parent names)))
    ;; HTML Standard §13.2.6.4.9: in a table the parser inserts an empty form
    ;; and pops it at once, but ignores the form while a template is open.
    (define form-kept?
      (and empty? (not (find (lambda (a) (html-named? a "template")) ancestors))))
    (define (cannot-be-child)
      (problem (format "~a cannot be a child of ~a; the parser would move it" name
                 (ancestor-name parent))
               parent))
    (cond
      [(string=? name "html")
       (problem "(:html …) must be the only child of its html form, at the top" #f)]
      [(and (member name '("head" "body")) parent (not (html-named? parent "html")))
       (problem (format "~a can be only at the top of a form or inside html" name) parent)]
      [(and parent (html-named? parent "html") (not (member name '("head" "body"))))
       (problem (format "html holds only head and body, not ~a" name) parent)]
      [(and parent (html-named? parent "head") (not (head-child? name)))
       (problem (format "head cannot hold ~a; the parser would move it to body" name) parent)]
      [(and parent (html-named? parent "noscript") (pair? (cdr ancestors))
            (html-named? (cadr ancestors) "head") (not (head-noscript-child? name)))
       (problem (format "a noscript in head holds only link, meta and style, not ~a" name)
                parent)]
      [(and (parent-is? (map car table-children))
            (not (kept-in-table? (ancestor-name parent) name form-kept? hidden-input?)))
       (cannot-be-child)]
      [(and parent (html-named? parent "template") (ancestor-first-child parent)
            (not (allowed-in-template-after? (template-mode (ancestor-first-child parent)) name)))
       (problem (format "~a cannot be a child of a template whose first literal element child is ~a; the parser would drop or move it"
                  name (ancestor-first-child parent))
                parent)]
      [(and (not parent) top-first-child
            (not (allowed-at-top-after? (template-mode top-first-child) name form-kept? hidden-input?)))
       (problem (format "~a cannot be at the top of a form after ~a; the parser would drop or move one of them"
                  name top-first-child)
                #f)]
      [(and (table-part? name) parent (not (html-named? parent "template"))
            (not (and (html-named-in? parent (map car table-children))
                      (kept-in-table? (ancestor-name parent) name form-kept? hidden-input?))))
       (cannot-be-child)]
      [(and (member name p-closers) (in-scope ancestors "p" button-scope))
       => (lambda (p) (problem (format "~a closes an open p, so it cannot be inside one" name) p))]
      [(and (string=? name "a")
            (find-in-scope ancestors (lambda (a) (html-named? a "a")) (lambda (a) (html-named-in? a markers))))
       => (lambda (a) (problem "a cannot be inside another a" a))]
      ;; HTML Standard §13.2.6.4.7: a form start tag is ignored only when the
      ;; form element pointer is set and no template is open.
      [(and (string=? name "form")
            (not (find (lambda (a) (html-named? a "template")) ancestors))
            (find (lambda (a) (html-named? a "form")) ancestors))
       => (lambda (f) (problem "form cannot be inside another form" f))]
      [(and (string=? name "button") (in-scope ancestors "button" default-scope))
       => (lambda (b) (problem "button cannot be inside another button" b))]
      [(and (string=? name "li")
            (find-in-scope ancestors (lambda (a) (html-named? a "li")) special-other-than-address-div-p?))
       => (lambda (li) (problem "li cannot be inside another li" li))]
      [(and (member name '("dd" "dt"))
            (find-in-scope ancestors (lambda (a) (html-named-in? a '("dd" "dt")))
              special-other-than-address-div-p?))
       => (lambda (d) (problem (format "~a cannot be inside ~a" name (ancestor-name d)) d))]
      [(and (member name headings) (parent-is? headings))
       (problem (format "~a cannot be a child of ~a" name (ancestor-name parent)) parent)]
      [(and (member name '("option" "optgroup")) (parent-is? '("option")))
       (cannot-be-child)]
      [(and (member name '("option" "optgroup" "hr" "input" "select"))
            (in-scope ancestors "select" default-scope))
       => (lambda (select)
            (cond
              [(member name '("input" "select"))
               (problem (format "~a cannot be inside select" name) select)]
              [(parent-is? (if (string=? name "option")
                               (remove "optgroup" implied-end-tags)
                               implied-end-tags))
               (problem (format "with a select open, ~a cannot be a child of ~a" name
                          (ancestor-name parent))
                        parent)]
              [else (problem #f #f)]))]
      [(and (member name '("rt" "rp")) (in-scope ancestors "ruby" default-scope)
            (parent-is? implied-end-tags))
       (problem (format "with a ruby open, ~a cannot be a child of ~a" name (ancestor-name parent))
                parent)]
      [(and (string=? name "noscript") (find (lambda (a) (html-named? a "noscript")) ancestors))
       => (lambda (n) (problem "noscript cannot be inside noscript" n))]
      [(and (member name '("table" "form"))
            (find-in-scope ancestors
              (lambda (a)
                (and (html-named? a "template")
                     (member (template-mode (ancestor-first-child a)) '("table" "tbody" "tr"))
                     #t))
              (lambda (a) (html-named-in? a '("td" "th" "caption" "template")))))
       => (lambda (t)
            (problem (format "~a cannot be inside a template whose first child is ~a; the parser would drop it"
                       name (ancestor-first-child t))
                     t))]
      [else (problem #f #f)]))

  (define (special-other-than-address-div-p? a)
    (and (listed? a special-elements) (not (html-named-in? a '("address" "div" "p"))))))
