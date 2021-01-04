#lang racket
(require
 racket/set
 "a3-graph-lib.rkt"
 "a3-compiler-lib.rkt")

(provide
 current-assignable-registers
 compiler-a2
 compiler-a3

 check-values-lang
 interp-values-lang
 uniquify
 select-instructions
 check-loc-lang
 uncover-locals
 assign-homes
 assign-registers
 conflict-analysis
 undead-analysis
 assign-homes-opt
 replace-locations
 patch-instructions
 check-paren-x64
 interp-paren-x64
 generate-x64)

(module+ test
  (require rackunit))

;; ------------------------------------------------------------------------
;; New starter code.

(TODO "Merge your solution from a2.rkt into this section.
       You should move this starter code to just after the definition of
       assign-homes in your current implementation.")

;; Exercise 1
(define current-assignable-registers
  (TODO "Design and implement the parameter current-assignable-registers for Exercise 1"))

;; Exercise 2

(define (assign-registers p)
  (TODO "Design and implement assign-registers for Exercise 2."))

;; Exercise 3

(define (conflict-analysis p)
  (TODO "Design and implement conflict-analysis for Exercise 3."))

;; Exercise 4

(define (undead-analysis p)
  (TODO "Design and implement undead-analysis for Exercise 4."))

(module+ test
  (define (get-undead-info p)
    (match p
      [`(begin ,info . ,any)
       (car (dict-ref info 'undead))]))

  (define-check (check-undead-sets-equal? actuals expecteds)
    (unless (equal? (length actuals)
                    (length expecteds))
      (fail-check (format "Expected ~a elements in the undead set, but found only ~a elements\n  expecteds~a:\n  actuals:~a"
                          (length expecteds)
                          (length actuals)
                          expecteds
                          actuals)))
    (for ([s1 actuals]
          [s2 expecteds]
          [i (in-naturals)])
      (unless (set=? s1 s2)
        (fail-check (format "Expected undead set ~a for instruction ~a, but got ~a" s2 i s1)))))

  (check-undead-sets-equal?
   (get-undead-info
    (undead-analysis
     '(begin
        ((locals (v.1 w.2 x.3 y.4 z.5 t.6 t.7 t.8 t.9)))
        (set! v.1 1)
        (set! w.2 46)
        (set! x.3 v.1)
        (set! t.7 7)
        (set! x.3 (+ x.3 t.7))
        (set! y.4 x.3)
        (set! t.8 4)
        (set! y.4 (+ y.4 t.8))
        (set! z.5 x.3)
        (set! z.5 (+ z.5 w.2))
        (set! t.6 y.4)
        (set! t.9 -1)
        (set! t.6 (* t.6 t.9))
        (set! z.5 (+ z.5 t.6))
        (halt z.5))))
   '((v.1) (v.1 w.2) (w.2 x.3) (t.7 w.2 x.3) (w.2 x.3) (y.4 w.2 x.3)
           (t.8 y.4 w.2 x.3) (y.4 w.2 x.3) (z.5 y.4 w.2) (z.5 y.4) (t.6 z.5)
           (t.6 z.5 t.9) (t.6 z.5) (z.5) ())))

;; Exercise 5

(define (assign-homes-opt p)
  (TODO "Design and implement assign-homes-opt for Exercise 5."))

;; ------------------------------------------------------------------------

(TODO "Add this section to your solution to A2 after the definition of generate-x64")

;; Exercise 6

(define (compiler-a2 p)
  (TODO "Implement compiler-a2 for Exercise 6"))

(define (compiler-a3 p)
  (TODO "Implement compiler-a3 for Exercise 6"))

#|

Fill me in ...

|#
(TODO "Write a brief paragraph in the above comment block comparing the
       compiler-a2 and compiler-a3 for Exercise 6.")

;; ------------------------------------------------------------------------
