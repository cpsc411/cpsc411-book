#lang reader "assignment-lang.rkt"
@(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  (for-label
   (except-in cpsc411/reference/a7-solution
              patch-instructions
              flatten-program
              expose-basic-blocks
              implement-fvars
              optimize-predicates
              replace-locations
              assign-frame-variables
              assign-registers
              allocate-frames
              assign-call-undead-variables
              conflict-analysis
              undead-analysis
              uncover-locals
              select-instructions
              normalize-bind
              impose-calling-conventions
              sequentialize-let)))

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@title[#:tag "top" #:tag-prefix "a7:"]{Milestone 7: Immediate Data Types}

@section{Assignment Summary}

The goal of this assignment is to introduce data type representation.
We will finally add booleans, as well as the empty list, the void object,
characters, and an error value.
We will introduce a tagged object representation to distinguished different data
types dynamically.
This will let us implement dynamic tag checking, reducing the amount of
undefined behaviour in our surface language.

This assignment is due @(due 'a7).

You can use the interrogator to get limited access to the reference solution:
@url{https://www.students.cs.ubc.ca/~cs-411/@|semester|/interrogator.cgi?an=a7}.

@todo{Explicit learning objectives}

@subsubsub*section{Checklist}

@emph{Completely new passes}
@itemlist[
@item{@racket[remove-complex-opera*]}
@item{@racket[specify-representation]}
@item{@racket[implement-safe-primops]}
]

@emph{Minor modifications to passes}
@itemlist[
@item{@racket[uniquify]}
]

@emph{You will need to redesign the remaining passes and consider what
modifications may be needed.}

@;@emph{No modifications to other passes} (if your compiler has been abstracted
@;with respect to binops.).

@section{Reading}
The readings for this week are @Secref[#:tag-prefixes '("book:" "chp-ae:")]{top}
and @Secref[#:tag-prefixes '("book:" "chp-immediates:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

@section{Exercises}

@exercise{Replace @racketmodname[cpsc411/2c-run-time] with @racketmodname[cpsc411/ptr-run-time]}

@exercise[#:optional #t]{Design and implement @racket[check-exprs-lang].}

@exercise{
Redesign and extend the implementation of @racket[uniquify].
}



@exercise{
Design and implement the function @racket[implement-safe-primops].
}

@exercise{Design and implement the function @racket[specify-representation].

Remember not to use magic constants, and instead use parameters for tags, shift
lengths, and masks.
See @racketmodname[cpsc411/compiler-lib], which defines some of these.}

@exercise{Design and implement the function @racket[remove-complex-opera*].

If you follow the template for the @emph{target} grammar, rather than the
source, similar to the design of validation passes, you can avoiding generating
unnecessary auxiliary variables.
This approach is simplified if you use a procedural accumulator (a continuation) for
helpers that @emph{might} need to generate auxiliary bindings.
However, (as always) you're free to write a naive but correct implementation.

When transforming calls, @values-bits-lang-v7[(call e e ...)], be sure to
enforce left-to-right evaluation order.
}

@exercise{
Redesign and extend the implementations of all the following passes:
@itemlist[
@item{@racket[sequentialize-let]}
@item{@racket[impose-calling-conventions]}
@item{@racket[normalize-bind]}
@item{@racket[select-instructions]}
@item{@racket[uncover-locals]}
@item{@racket[undead-analysis]}
@item{@racket[conflict-analysis]}
@item{@racket[assign-call-undead-variables]}
@item{@racket[allocate-frames]}
@item{@racket[assign-registers]}
@item{@racket[assign-frame-variables]}
@item{@racket[replace-locations]}
@item{@racket[optimize-predicates]}
@item{@racket[implement-fvars]}
@item{@racket[expose-basic-blocks]}
@item{@racket[flatten-program]}
]

This should involve a change to a single function if your compiler has been
abstracted over @paren-x64-v7[binop]s.
}

@exercise{
Redesign and extend the implementation of @racket[patch-instructions].
This may not require any changes.

You need not support a more general form of
@paren-x64-v7[arithmetic-shift-right]: it cannot be implemented by simply
patching this instruction with a few moves to auxiliary locations as
before; the second operand must be an integer literal in the range 0 to 63
(inclusive).
@ch1-tech{x64} does not provide a way to make that second operand a location.}

@exercise{
Redesign and extend the implementation of the function @racket[generate-x64].

This should be a small change if you've abstracted over @paren-x64-v7[binops].
}
