#lang reader "assignment-lang.rkt"

@(require
  (except-in "../chapter/return.scrbl" doc)
  (for-label cpsc411/compiler-lib)
  (for-label cpsc411/reference/a5-solution)
  cpsc411/langs/v6)

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@; NB: Do not line wrap this as it might break the link
@(define x64-abi
   @hyperlink["https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI"]{x64
   System V ABI})

@title[#:tag "top" #:tag-prefix "a6:"]{Milestone 6: Adding Return}

@section{Assignment Summary}

The goal of this assignment is to introduce the procedure return abstraction and
non-tail calls, so that we can call functions that return values to non-tail
contexts.

This assignment is due @(due 'a6).


@todo{Design component?}

@subsubsub*section{Learning Objectives}
@todo{Update these}
@;@itemlist[
@;@item{Students should be able to identify the advantages of calling
@;conventions.}
@;@item{Students should be able to identify the disadvantages of calling
@;conventions.}
@;@item{Students should be able to implement a compiler for a language that supports general function calls by imposing a calling convention for functions.}
@;@item{Students should be able to differentiate between a frame and the stack.}
@;@item{Students should be able to describe the purpose of a frame.}
@;@item{Students should be able to describe the purpose of a stack of frames.}
@;@item{Students should be able to construct a representation of a frame for a
@;non-tail call relative to the caller.}
@;@item{Students should be able to construct a representation of a frame for a
@;tail call relative to the caller.}
@;@item{Students should be able to explain how proper tail calls are implemented
@;and why they do not use additional stack space compared to non-tail calls.}
@;]

@subsubsub*section{Checklist}

@emph{Completely new passes}

@typeset-passlist[
assign-call-undead-variables
allocate-frames
assign-frame-variables
]

@emph{Major modifications to passes}

@typeset-passlist[
impose-calling-conventions
undead-analysis
expose-basic-blocks
implement-fvars
]

@emph{Minor modifications to passes}

@typeset-passlist[
check-values-lang
uniquify
sequentialize-let
canonicalize-bind
select-instructions
uncover-locals
conflict-analysis
assign-registers
replace-locations
]

@emph{(Possibly) no modifications to passes}

@typeset-passlist[
resolve-predicates
patch-instructions
generate-x64
]

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:"
"chp-return:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

@section{Exercises}

@exercise{Extend @racket[check-values-lang] to validate the safety
of source programs.
Remember that you will have to reject some safe programs.
}

@exercise{Extend @racket[uniquify] to support procedure calls in the source
language.
}

@exercise{Extend @racket[sequentialize-let] with non-tail calls.}

@exercise{Extend @racket[impose-calling-conventions] with support for non-tail
calls and return.

You should use @racket[current-return-address-register] and
@racket[current-return-value-register] rather than hard-coding the calling
convention registers.
}

@exercise{Extend @racket[canonicalize-bind] with support for return points.}

@exercise{Extend @racket[select-instructions] with support for return points.}

@exercise{Extend @racket[uncover-locals] with support for return points.}

@exercise{Extend @racket[undead-analysis] with support for return points.

@margin-note{
Hint:

The simplest way to get the @asm-pred-lang-v6/undead[call-undead] locations is
to use a single mutable variable that is local to the helper function that
processes definition.
See @racket[box]
}

}

@exercise{Extend @racket[conflict-analysis] with support for return points.}

@exercise{Design and implement @racket[assign-call-undead-variables].}

@exercise{Design and implement @racket[allocate-frames].}

@exercise{Extend @racket[assign-registers] to avoid directly implementing
spilling.

You may be able to reuse a lot of code from
@racket[assign-call-undead-variables], if abstracted properly.

It might help to re-imagine the "pick a low-degree variable" progress in the
allocation algorithm as a sorting problem.
If the variables are sorted in degree order, then the core assignment algorithm
doesn't need to know about degrees---it always picks the first variable in the
sorted list.
See @racket[sort].

Be careful when computing which registers cannot be used; your conflict graph
contains a mix of abstract locations and registers.

You may still find it useful to use a single mutable variable to collect
spilled variables.

You should use the parameter @racket[current-assignable-registers].
}

@exercise{Design and implement @racket[assign-frame-variables].

Be careful when computing which frame variables cannot be used; your conflict
graph contains a mix of abstract locations and frame locations.}

@exercise{Extend @racket[replace-locations] to support return points and
non-tail calls.}

@;@challenge{Design and implement @racket[optimize-moves] to optimize
@;@tech{Para-asm-lang v6}.
@;
@;@itemlist[
@;@item{Replace all instructions of the form @object-code{(set! rloc_1 rloc_1)}.
@;with @object-code{(nop)}, which must be added to all subsequent languages.
@;This instruction should generate an empty string in x64.}
@;
@;@item{Rewrite instructions of the form @object-code{(set! rloc_1 (binop rloc_2
@;rloc_1))} to @object-code{(set! rloc_1 (binop rloc_1 rloc_2))}, where
@;@object-code{binop} is commutative.
@;This will help @racket[patch-instructions] produce better code.}
@;]
@;}

@exercise{Extend @racket[expose-basic-blocks] to implement the return point
abstraction.}

@exercise{Extend @racket[implement-fvars] to correctly implement frame variables
relative to the current value of the
@racket[current-frame-base-pointer-register].

You should use an accumulator representing the current offset from the current
block's frame.
You may want to use @racket[fvar->addr].

You shouldn't assume that the @racket[current-frame-base-pointer-register] is
@paren-x64-v6[rbp], and should use the parameter instead.
}
