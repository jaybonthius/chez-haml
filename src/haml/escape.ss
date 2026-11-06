(define replacement-character '#vu8(#xEF #xBF #xBD))

(define (write-utf8-char port c)
  (cond
    [(fx< c #x800)
     (put-u8 port (fxior #xC0 (fxsrl c 6)))
     (put-u8 port (fxior #x80 (fxand c #x3F)))]
    [(fx< c #x10000)
     (put-u8 port (fxior #xE0 (fxsrl c 12)))
     (put-u8 port (fxior #x80 (fxand (fxsrl c 6) #x3F)))
     (put-u8 port (fxior #x80 (fxand c #x3F)))]
    [else
     (put-u8 port (fxior #xF0 (fxsrl c 18)))
     (put-u8 port (fxior #x80 (fxand (fxsrl c 12) #x3F)))
     (put-u8 port (fxior #x80 (fxand (fxsrl c 6) #x3F)))
     (put-u8 port (fxior #x80 (fxand c #x3F)))]))

;; Every escaped character is below 63, so the common letters take the
;; first branch. NUL becomes U+FFFD and CR or CRLF becomes LF, as the parser
;; would read them.
(define-syntax define-text-writer
  (syntax-rules ()
    [(_ name [code replacement] ...)
     (define (name port s)
       (let ([n (string-length s)])
         (let loop ([i 0])
           (when (fx< i n)
             (let ([c (char->integer (string-ref s i))])
               (cond
                 [(fx> c 62)
                  (if (fx< c #x80) (put-u8 port c) (write-utf8-char port c))
                  (loop (fx+ i 1))]
                 [(fx= c code) (put-bytevector port replacement) (loop (fx+ i 1))]
                 ...
                 [(fx= c 13)
                  (put-u8 port 10)
                  (if (and (fx< (fx+ i 1) n) (char=? (string-ref s (fx+ i 1)) #\newline))
                      (loop (fx+ i 2))
                      (loop (fx+ i 1)))]
                 [(fx= c 0) (put-bytevector port replacement-character) (loop (fx+ i 1))]
                 [else (put-u8 port c) (loop (fx+ i 1))]))))))]))

(define-text-writer write-text
  [38 '#vu8(38 97 109 112 59)]
  [60 '#vu8(38 108 116 59)]
  [62 '#vu8(38 103 116 59)])

(define-text-writer write-attribute-text
  [38 '#vu8(38 97 109 112 59)]
  [60 '#vu8(38 108 116 59)]
  [62 '#vu8(38 103 116 59)]
  [34 '#vu8(38 113 117 111 116 59)])

(define-text-writer write-raw-text)
