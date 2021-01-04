#lang racket
(require
 racket/set
 "a4-graph-lib.rkt"
 "a4-compiler-lib.rkt")

(provide
 current-assignable-registers

 values-lang-fact
 values-lang-fib
 type-check-values-lang

 check-values-lang
 interp-values-lang
 uniquify
 select-instructions
 uncover-locals
 assign-homes
 assign-homes-opt
   undead-analysis
   conflict-analysis
   assign-registers
 discard-call-live
 replace-locations
 expose-basic-blocks
 flatten-program
 patch-instructions
 interp-paren-x64
   link-paren-x64
 generate-x64)

(module+ test
  (require rackunit))

;; ------------------------------------------------------------------------
;; New starter code.
(TODO "Merge your solution from a3.rkt into this section.
       You should move this starter code to just before the definition of
       interp-paren-x64 in your current implementation.")

;; Exercise 2
(define (link-paren-x64 p)
  (TODO "Design and implement link-paren-x64 for Exercise 2."))

;; Exercise 3
(define (interp-paren-x64 p)

  (define (eval-program env pc p)
    (TODO "Finish designing eval-program for Exercise 3")
    (if (= pc (length p))
        (dict-ref env 'rax)
        (eval-statement env pc p (list-ref p pc))))

  (TODO "Redesign and implement interp-paren-x64 for Exercise 3."))

(module+ test
  (define-check (check-timeout? th seconds)
    (when (sync/timeout seconds (thread th))
      (fail-check)))

  (check-timeout?
   (thunk
    (interp-paren-x64
     '(begin
        (define L.f.10 (jump L.f.10)))))
   2))

;; ------------------------------------------------------------------------

