#lang racket

(require
 racket/set
 racket/format
 "share/a8-graph-lib.rkt"
 "share/a8-compiler-lib.rkt")

(provide
 uniquify
 implement-safe-primops
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
