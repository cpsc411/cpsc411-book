#lang info
;(define collection "cpsc411-book")
(define deps '("at-exp-lib"
               ;"cpsc411-lib"
               "https://github.com/cpsc411/cpsc411-pub.git?path=cpsc411-lib#2022w2"
               "gregor-lib"
               "memoize-lib"
               "racket-graphviz"
               ("scribble-bettergrammar-lib" #:version "1.6.4")
               "scribble-lib"
               "web-server-lib"
               "with-cache"
               "base"
               "functional-lib"
               "draw-lib"
               "pict-lib"))
(define build-deps
  '("graph-doc"
    "rackunit-doc"
    "rackunit-lib"
    "scribble-doc"
    ("base" #:version "7.4")
    "racket-doc"
    ;"cpsc411-doc"
    "https://github.com/cpsc411/cpsc411-pub.git?path=cpsc411-doc#2022w2"
    ;"cpsc411-lib"
    "https://github.com/cpsc411/cpsc411-pub.git?path=cpsc411-lib#2022w2"
    "cpsc411-reference-lib"
    ; must be installed manually
    ;"https://github.com/cpsc411/cpsc411-priv.git?path=cpsc411-reference-lib#2022w2"
    "racket-graphviz"
    ("scribble-bettergrammar-lib" #:version "1.6.4")))
(define scribblings '(("index.scrbl" (multi-page no-search))))
(define test-omit-paths (list "serve.rkt"))
(define pkg-desc "The CPSC411 Book, which includes lecture notes and assignments.")
(define pkg-authors '(wilbowma))
