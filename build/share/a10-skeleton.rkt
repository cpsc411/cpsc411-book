#lang racket

(require
 "a10-graph-lib.rkt"
 "a10-compiler-lib.rkt")

(provide
 uniquify
 expand-macros
 define->letrec
 purify-letrec
 convert-assigned
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
