#lang info
;(define collection "cpsc411-book")
(define deps '("at-exp-lib"
               "cpsc411-lib"
               "gregor-lib"
               ;; NOTE: memoize is required by a dependency of gregor lib, so force install my forked version. Otherwise, we get package conflicts.
               "https://github.com/wilbowma/memoize?path=memoize-lib"
               "https://github.com/wilbowma/memoize?path=memoize-doc"
               "https://github.com/wilbowma/memoize?path=memoize-test"
               "https://github.com/wilbowma/memoize?path=memoize"
               "racket-graphviz"
               ("scribble-bettergrammar-lib" #:version "1.4.2")
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
    "cpsc411-doc"
    "cpsc411-lib"
    "cpsc411-reference-lib"
    "racket-graphviz"
    ("scribble-bettergrammar-lib" #:version "1.4.2")))
(define scribblings '(("index.scrbl" (multi-page no-search))))
(define test-omit-paths (list "serve.rkt"))
(define pkg-desc "The CPSC411 Book, which includes lecture notes and assignments.")
(define pkg-authors '(wilbowma))
