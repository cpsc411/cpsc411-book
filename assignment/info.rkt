#lang info
(define compile-omit-paths '("attempt-at-cached-eval.rkt" "a11.scrbl"))
(define test-omit-paths '("attempt-at-cached-eval.rkt" "a11.scrbl"))
(define scribblings '())
;; TODO: Scribble isn't really meant to build modules, but documents.
;; I don't think I can do this and then later "include" the pre-built (partial)
;; documents.
#;(define scribblings
  '(("a0.scrbl")
    ("a10.scrbl")
    ("a11.scrbl")
    ("a1.scrbl")
    ("a2.scrbl")
    ("a3.scrbl")
    ("a3-wrong.scrbl")
    ("a4.scrbl")
    ("a4-wrong.scrbl")
    ("a5.scrbl")
    ("a6.scrbl")
    ("a7.scrbl")
    ("a8.scrbl")
    ("a9.scrbl")))
