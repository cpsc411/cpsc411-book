#lang info
;(define collection "cpsc411-book")
(define deps '("at-exp-lib"
               "cpsc411-lib"
               "gregor-lib"
               "racket-graphviz"
               ("scribble-bettergrammar-lib" #:version "1.4")
               "scribble-lib"
               "web-server-lib"
               "with-cache"
               "base"
               "functional-lib"))
(define build-deps
  '("graph-doc"
    "rackunit-doc"
    "rackunit-lib"
    "scribble-doc"
    ("base" #:version "7.4")
    "racket-doc"
    #;"cpsc411-doc"
    "cpsc411-lib"
    "cpsc411-reference-lib"
    "racket-graphviz"
    ("scribble-bettergrammar-lib" #:version "1.4")))
(define scribblings '(("index.scrbl" (multi-page no-search))))
(define pkg-desc "The CPSC411 Book, which includes lecture notes and assignments.")
(define pkg-authors '(wilbowma))
