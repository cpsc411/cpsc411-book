#lang reader "assignment-lang.rkt"

@(require
  #;(except-in "a2.scrbl" doc)
  (except-in "../chapter/tail-calls.scrbl" doc)
  (for-label cpsc411/compiler-lib)
  (for-label cpsc411/reference/a5-solution))

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@(define (notes . _) (void))
@title[#:tag "top" #:tag-prefix "a5:"]{Milestone 5: Adding Call}

@section{Milestone Summary}
The goals of this milestone are to (1) introduce the procedure call abstraction
(2) introduce calling conventions, an abstraction for compiling procedure calls
In the process, we'll learn a different approach to compiler design.
Rather than building up layers of abstractions bottom-up, we consider how to
compile a designed feature top-down.
We'll introduce an abstraction and consider how to design translations to lower
the new an abstraction to existing features.

This milestone is due @(due 'a5).

@todo{Design component?}

@subsection{Learning Objectives}
@todo{Redo above summary to split out summary from learning objectives.}

@subsection{Checklist}
@todo{Should be able to generate this}

@emph{Completely new passes}

@typeset-passlist[
check-values-lang
impose-calling-conventions
]

@emph{Major modifications}

@typeset-passlist[
]

@emph{Minor modifications}

@typeset-passlist[
uniquify
sequentialize-let
canonicalize-bind
select-instructions
uncover-locals
undead-analysis
conflict-analysis
assign-registers
replace-locations
expose-basic-blocks
]

@emph{No modifications}
@typeset-passlist[
resolve-predicates
patch-instructions
implement-fvars
generate-x64
link-paren-x64
interp-paren-x64
]

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:" "chp5:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

@section{Exercises}

@exercise{Design and implement @racket[check-values-lang] to validate the safety
of source programs.
Remember that you will have to reject some safe programs.}

@challenge{Design an extension to @ch5-tech{Values-lang v5} that would enable
safely passing procedures as arguments.
Your design should give a grammar for the language, describe the intended
meaning of any additions to the language, and describe how the features are used
to ensure safety.}

@exercise{Extend @racket[uniquify] to support procedure calls in the source language.

To compile procedure names, you should use @racket[fresh-label].
}

@exercise{Extend @racket[sequentialize-let] with procedure calls.}

@exercise{Design and implement @racket[impose-calling-conventions]

You should use @racket[current-parameter-registers] rather than hard-coding the
calling convention registers.
}

@exercise{Extend @racket[canonicalize-bind] with support for blocks and the jump
instruction.}

@exercise{Extend @racket[select-instructions] with support for blocks and the
jump instruction.}

@exercise{Extend @racket[uncover-locals] with support for blocks and the jump
instruction.}

@exercise{Extend @racket[undead-analysis] with support for blocks and the jump
instruction.}

@exercise{Extend @racket[conflict-analysis] with support for blocks and the jump
instruction.}

@exercise{Extend @racket[assign-registers] with support for blocks and the jump
instruction.}

@exercise{Extend @racket[expose-basic-blocks] with support for blocks and the jump
instruction in the source.}
