#lang reader "assignment-lang.rkt"
@(require
  cpsc411/compiler-lib
  (for-label cpsc411/reference/a10-solution))

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@title[#:tag "top" #:tag-prefix "a10:"]{Milestone 10: @;{Recursive Data and} Syntactic Sugar}

@section{Assignment Summary}
@;The goal of this assignment is to add recursive data to our language.
@;This allows us to create streams or cyclic graphs, which are useful in a variety
@;of algorithms (such as writing control-flow graph algorithms for compilers).
@;With this feature, we'll finally be able to completely collapse distinctions in
@;our surface syntax.

The goal of this milestone is to add syntactic sugar to our language.
@;We'll also add a little syntactic sugar to the surface language.
@;This isn't really related to recursive data, but the assignment is small if we
@;don't merge the two separate goals into a single milestone.

This assignment is due @(due 'a10).

You can use the reference solution here:
@url{https://www.students.cs.ubc.ca/~cs-411/@|semester|/a10-interrogator.cgi}

@subsubsub*section{Assignment Checklist}
@subsubsub*section{Checklist}

@emph{Completely new passes}
@itemlist[
@item{@racket[expand-macros]}
]

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:"
"chp-letrec:")]{top} and @Secref[#:tag-prefixes '("book:" "chp-macros:")]{top},
although we will only implement
@Secref[#:tag-prefixes
'("book:" "chp-macros:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

@section{Exercises}

@;@exercise{Redesign and extend the implementation of @racket[uniquify].
@;The source language is @tech{Racketish-Surface} and the target language is
@;@tech{Racketish-Surface-Unique}.
@;
@;To support implicit procedure calls, you'll probably want predicates for
@;determining when something is a macro id, and when something is a value keyword.
@;
@;To handle @object-code{'s-expr}, remember that it is implicitly elaborated to
@;@object-code{(quote s-expr)} by Racket.}
@;
@exercise{Design and implement the function @racket[expand-macros].
The source language is @ch11-tech{Racketish-Surface} and the target language
is @ch9-tech{Exprs-lang v9}.

Try designing the pass so that each of the macros is defined in its own
helper function, and make @racket[expand-macros] handle macros completely
generically.
You'll be part of the way to an implementation of a macro system that supports
user-defined macros.
If you exposed an interface in @ch11-tech{Racketish-Surface} and used
@racket[eval]...
}
