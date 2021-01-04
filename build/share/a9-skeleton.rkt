#lang racket

(provide
 uniquify
 define->letrec
 dox-lambdas
 implement-safe-primops
 uncover-free
 convert-closures
 optimize-known-calls
 hoist-lambdas
 implement-closures
 sequentialize-let
 implement-safe-apply
 specify-representation
 a-normalize
 select-instructions
 expose-allocation-pointer
 uncover-locals
 undead-analysis
 conflict-analysis
 pre-assign-frame-variables
 assign-frames
 assign-registers
 assign-frame-variables
 discard-call-live
 replace-locations
 implement-fvars
 expose-basic-blocks
 flatten-program
 patch-instructions
 implement-mops
 generate-x64)

; A useful helper.
; You should not feel compelled to use this unless you felt like you needed such
; a function.
;
; A multi-return-value map.
; maps the function f over the lists ls lss ....
; f is expected to return n values, and map-n will return n lists.
(define (map-n n f ls . lss)
  (if (empty? ls)
      (apply values (build-list n (lambda _ '())))
      (call-with-values
       (thunk (apply f (car ls) (map car lss)))
       (lambda vs
         (call-with-values
          (thunk (apply map-n n f (cdr ls) (map cdr lss)))
          (lambda lss
            (apply
             values
             (map cons vs lss))))))))

(define map2 (curry map-n 2))
;; e.g.
;; (map2 (lambda (x y) (values y x)) '(1 2 3) '(a b c))
;; returns: '(a b c) '(1 2 3)
