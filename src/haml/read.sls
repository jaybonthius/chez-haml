(library (haml read)
  (export read-markup)
  (import (chezscheme) (haml rules))

  ;; The inverses of escape.ss's text and attribute writers. Only what this
  ;; library writes is read, so markup from anywhere else fails instead of
  ;; being read as something it is not.
  (define text-escapes '(("&amp;" . #\&) ("&lt;" . #\<) ("&gt;" . #\>)))
  (define attribute-escapes (cons '("&quot;" . #\") text-escapes))

  (define doctype "<!DOCTYPE html>")

  (define (element-namespace parent name)
    (if (or (eq? parent 'svg) (string=? name "svg")) 'svg 'html))

  (define (child-namespace namespace name)
    (if (and (eq? namespace 'svg) (not (html-integration-point? name))) 'svg 'html))

  (define (read-markup who s)
    (let ([n (string-length s)])
      (define (fail i message)
        (assertion-violationf who "~a at index ~a: ~s" message i (substring s i (fxmin n (fx+ i 40)))))
      (define (at? i prefix)
        (let ([m (string-length prefix)])
          (and (fx<= (fx+ i m) n) (string=? prefix (substring s i (fx+ i m))))))
      (define (name-end i)
        (if (and (fx< i n) (not (memv (string-ref s i) '(#\space #\tab #\newline #\return #\page #\> #\/ #\= #\" #\' #\<))))
            (name-end (fx+ i 1))
            i))
      (define (escaped i stop escapes)
        (let ([out (open-output-string)])
          (let loop ([i i])
            (cond
              [(or (fx= i n) (char=? (string-ref s i) stop)) (values (get-output-string out) i)]
              [(char=? (string-ref s i) #\&)
               (let ([e (find (lambda (e) (at? i (car e))) escapes)])
                 (unless e (fail i "an escape this library does not write"))
                 (write-char (cdr e) out)
                 (loop (fx+ i (string-length (car e)))))]
              [(char=? (string-ref s i) #\>) (fail i "a > this library escapes")]
              [else (write-char (string-ref s i) out) (loop (fx+ i 1))]))))
      (define (attributes i)
        (let loop ([i i] [found '()])
          (cond
            [(fx= i n) (fail i "a start tag with no end")]
            [(char=? (string-ref s i) #\>) (values (reverse found) (fx+ i 1))]
            [(char=? (string-ref s i) #\space)
             (let* ([start (fx+ i 1)] [end (name-end start)] [name (substring s start end)])
               (unless (valid-attribute-name? name) (fail start "not an attribute"))
               (if (at? end "=\"")
                   (let-values ([(value close) (escaped (fx+ end 2) #\" attribute-escapes)])
                     (when (fx= close n) (fail start "an attribute value with no end"))
                     (loop (fx+ close 1) (cons (list (string->symbol name) value) found)))
                   (loop end (cons (list (string->symbol name) "") found))))]
            [else (fail i "markup this library does not write")])))
      (define (raw-text i name)
        (let ([close (string-append "</" name ">")])
          (let loop ([j i])
            (cond
              [(fx= j n) (fail i (format "no end tag for ~a" name))]
              [(at? j close) (values (if (fx= j i) '() (list (substring s i j))) (fx+ j (string-length close)))]
              [else (loop (fx+ j 1))]))))
      (define (element i parent)
        (let* ([start (fx+ i 1)] [end (name-end start)] [name (substring s start end)])
          (unless (and (fx< start end) (char-alphabetic? (string-ref s start)))
            (fail i "markup this library does not write"))
          (let ([namespace (element-namespace parent name)])
            (let-values ([(attrs i) (attributes end)])
              (let ([make (lambda (children)
                            (cons (string->symbol name)
                                  (if (null? attrs) children (cons (cons '\x40; attrs) children))))]
                    [i (if (and (eq? namespace 'html) (newline-after-start-tag? name) (fx< i n)
                                (char=? (string-ref s i) #\newline))
                           (fx+ i 1)
                           i)])
                (case (element-kind namespace name)
                  [(void) (values (make '()) i)]
                  [(raw-text)
                   (let-values ([(children i) (raw-text i name)])
                     (values (make children) i))]
                  [else
                   (let-values ([(children i) (content i name (child-namespace namespace name))])
                     (values (make children) i))]))))))
      (define (content i name namespace)
        (let loop ([i i] [nodes '()])
          (cond
            [(fx= i n)
             (when name (fail i (format "no end tag for ~a" name)))
             (values (reverse nodes) i)]
            [(at? i "</")
             (let ([end (name-end (fx+ i 2))])
               (unless (and name (fx< end n) (char=? (string-ref s end) #\>)
                            (string=? name (substring s (fx+ i 2) end)))
                 (fail i "an end tag that closes no open element"))
               (values (reverse nodes) (fx+ end 1)))]
            [(char=? (string-ref s i) #\<)
             (let-values ([(node i) (element i namespace)])
               (loop i (cons node nodes)))]
            [else
             (let-values ([(text i) (escaped i #\< text-escapes)])
               (loop i (cons text nodes)))])))
      (let-values ([(declaration start) (if (at? 0 doctype)
                                            (values '((*DECL* DOCTYPE html)) (string-length doctype))
                                            (values '() 0))])
        (let-values ([(nodes i) (content start #f 'html)])
          (cons '*TOP* (append declaration nodes)))))))
