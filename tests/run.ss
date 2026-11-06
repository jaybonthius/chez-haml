;; Loading a library object checks the files the library included, and
;; include finds them only through source-directories.
(source-directories '("src" "."))
(load-program (car (command-line-arguments)))
