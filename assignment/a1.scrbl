#lang reader "assignment-lang.rkt"
@(require
  scribble/core
  scribble/examples
  racket/runtime-path
  (for-label rackunit)
  (for-label cpsc411/compiler-lib)
  (for-label cpsc411/langs/v1)
  (for-label cpsc411/reference/a1-solution))

@;todo{Reset automatically on new assignment.}
@(reset-exercise-counter!)
@(reset-challenge-counter!)

@title[#:tag "top" #:tag-prefix "a1:"]{Milestone 1: Abstracting x64 Boilerplate}

@section{Assignment Summary}

The goal of this assignment is to introduce the process of designing,
implementing, and reasoning about a compiler.
In this assignment, you will implement a small abstraction layer on top of
@ch1-tech{x64}, called @tech[#:tag-prefixes '("book:" "chp-boilerplate:")]{Paren-x64
v1}, which allows easily writing a subset of @ch1-tech{x64} programs while
ignoring some of the boilerplate and operating-system-specific details.

This milestone is due @(due 'a1).

You should start from the @tt{milestone-1} branch of the @tt{cpsc411-skeleton}
repository.

You can use the interrogator to get limited access to the reference solution:
@url{https://www.students.cs.ubc.ca/~cs-411/@|semester|/interrogator.cgi?an=a1}.

You can use the language diff tool to view differences between languages that
aren't typeset in the book:
@url{https://www.students.cs.ubc.ca/~cs-411/@|semester|/lang-differ.cgi}.

@subsubsub*section{Assignment Checklist}
Before you start, you should read:
@itemlist[
 @item{@secref{grading-design}; and}
 @item{@secref{grading-tests}.}
]
These lay out important expectations regarding grading and sharing of code.

@emph{New passes}
@typeset-passlist[
generate-x64
wrap-x64-run-time
wrap-x64-boilerplate
]

@emph{Optional passes}

You will not be required to maintain these passes in the rest of the compiler,
and so they're optional.
However, I strongly recommend implementing them as they will provide valuable practice,
insight into the semantics of our intermediate languages, and can be used in
testing and debugging your compiler.

@typeset-passlist[
check-paren-x64
check-paren-x64-syntax
check-paren-x64-init
interp-paren-x64
]

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:" "chp1:")]{top} and
@Secref[#:tag-prefixes '("book:" "chp-boilerplate:")]{top}.
This milestone description links to the documentation for each exercise in the
chapter for convenience, but you are responsible for the reading the entire
chapter.

You should read first and work the relevant exercises as you read.

@section{Exercises}
@exercise[#:optional #t]{@emph{Design} and implement the function
@racket[check-paren-x64-syntax].

It might help to start by writing the template, following the instructions from
@secref[#:tag-prefixes '("book:")]{sec:recipe} for templates for language
processors.

Unit test appropriately, with tests for both failure and success. You should
look into the @other-doc['(lib "rackunit/scribblings/rackunit.scrbl")] package.

All tests should be inside the special test submodule.
See @secref["main-and-test" #:doc '(lib "scribblings/guide/guide.scrbl")] for
more details.

You can check your implementation against the reference implementation
@racket[paren-x64-v1?].
}

@exercise[#:optional #t]{@emph{Design} and implement the function
@racket[check-paren-x64-init].}

@exercise[#:optional #t]{Design and implement the function
@racket[check-paren-x64], a validator for @tech[#:tag-prefixes '("book:"
"chp-boilerplate:")]{Paren-x64 v1}.
It should just compose @racket[check-paren-x64-init] and
@racket[check-paren-x64-syntax], if you've implemented both, but could also just
be an alias for @racket[check-paren-x64-syntax].}

@exercise[#:optional #t]{
@emph{Design} and implement the function @racket[interp-paren-x64], an
interpreter for @tech[#:tag-prefixes '("book:" "chp-boilerplate:")]{Paren-x64
v1}.

To properly implement arithmetic operations, you need to handle two's complement
arithmetic, which overflows on large positive numbers and underflows on small
negative numbers.
You may want to use @racket[x64-add] and @racket[x64-mul] from
@racketmodname[cpsc411/compiler-lib].

While testing, you may want to avoid hard-coding values modulo 256, as the
interpreter may change in later iterations to return values directly.

You can test your implementation against the reference implementation:
@racket[interp-paren-x64-v1].
}

@exercise{
Design and implement the function @racket[generate-x64] which compiles
a @tech[#:tag-prefixes '("book:" "chp-boilerplate:")]{Paren-x64 v1} represented
as an s-expression into an @tech[#:tag-prefixes
'("book:" "chp-boilerplate:")]{instruction sequence}.
}

@exercise{
Design and implement the function @racket[wrap-x64-run-time] which installs
the @tech[#:tag-prefixes '("book:" "chp-boilerplate:")]{Paren-x64 v1} run-time
system.

@racketmodname[cpsc411/compiler-lib] provides some definitions, such as
@racket[sys-exit], that are helpful for this.

Note that a similarly named function exists in that library, but will not
correctly implement the run-time system for this milestone.

For formatting strings in Racket, you may want to investigate @racket[format],
@racket[~a], and @racketmodname[at-exp].
}

@exercise{Design and implement a Racket function @racket[wrap-x64-boilerplate].

Note that a similarly named function exists in the support library, but will not
correctly implement the boilerplate this milestone.
}

@exercise[#:optional #t]{Test your compiler correctness statement by
running the same programs through your interpreter and through your compiler and
comparing the results.

While testing, you may want to avoid hard-coding values modulo 256, as the
run-time system may change in later iterations to return values directly.

You can test your implementation against the reference interpreter,
@racket[interp-paren-x64-v1].
}
