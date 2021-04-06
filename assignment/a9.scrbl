#lang reader "assignment-lang.rkt"
@(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  (for-label cpsc411/reference/a9-solution))

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@title[#:tag "top" #:tag-prefix "a9:"]{Milestone 9: First-class Procedures}

@section{Assignment Summary}

The goal of this assignment is to add first-class procedures to our language.
This adds a procedure data types that includes the values of free variables at
the procedure's definition.
We add @exprs-lang-v9[lambda] as a first-class value in the source language, which
can create a procedure at any point in the program.

This assignment is due @(due 'a9).

You can use the interrogator to get limited access to the reference solution:
@url{https://williamjbowman.com:8081/servlets/standalone.rkt?an=a9} (faster) or
@url{https://www.students.cs.ubc.ca/~cs-411/2020w2/interrogator.cgi?an=a9}.

@subsubsub*section{Checklist}

@emph{Completely new passes}
@itemlist[
@item{@racket[implement-safe-call]}
@item{@racket[define->letrec]}
@item{@racket[optimize-direct-calls]}
@item{@racket[dox-lambdas]}
@item{@racket[uncover-free]}
@item{@racket[convert-closures]}
@item{@racket[optimize-known-calls]}
@item{@racket[hoist-lambdas]}
@item{@racket[implement-closures]}
]

@emph{Minor modifications to passes}
@itemlist[
@item{@racket[check-exprs-lang]}
@item{@racket[uniquify]}
@item{@racket[implement-safe-primops]}
@item{@racket[specify-representation]}
]

@emph{No modifications required to any other passes.}

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:"
"chp-closures:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

@section{Exercises}

@exercise{Redesign and extend the implementation of @racket[uniquify].}
@exercise{Redesign and extend the implementation of @racket[implement-safe-primops].}
@exercise{Design and implement @racket[implement-safe-call].}
@exercise{Design and implement @racket[define->letrec].}
@exercise{Design and implement @racket[optimize-direct-calls].}
@exercise{Design and implement @racket[dox-lambdas].}
@exercise{Design and implement @racket[uncover-free].

You may find @racket[map-n] and @racket[map2] helpful for functional programming
with multiple return values.
You shouldn't feel like you must use it, nor try to use it if it doesn't occur
to you that you want such a function.
}
@exercise{Design and implement @racket[convert-closures].

You may find @racket[map-n] and @racket[map2] helpful for functional programming
with multiple return values.
You shouldn't feel like you must use it, nor try to use it if it doesn't occur
to you that you want such a function.
}
@exercise{Design and implement @racket[optimize-known-calls].}
@exercise{Design and implement @racket[hoist-lambdas].}
@exercise{Design and implement @racket[implement-closures].}
@exercise{Redesign and extend the implementation of @racket[specify-representation].}
