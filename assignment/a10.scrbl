#lang reader "assignment-lang.rkt"
@(require cpsc411/deprecated/a10-compiler-lib)

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@;TODO abstractions for each of these; no more copy/paste/modify
@(define eg
   (make-cached-eval
    "a10-eval"
    '(require
      cpsc411/v1-reference/a10-solution
      cpsc411/deprecated/a10-compiler-lib
      racket/pretty)
    '(current-stack-size 512)))

@title[#:tag "top" #:tag-prefix "a10:"]{Milestone 10: Recursive Data and Syntactic Sugar}

@section{Assignment Summary}
The goal of this assignment is to add recursive data to our language.
This allows us to create streams or cyclic graphs, which are useful in a variety
of algorithms (such as writing control-flow graph algorithms for compilers).
With this feature, we'll finally be able to completely collapse distinctions in
our surface syntax.

We'll also add a little syntactic sugar to the surface language.
This isn't really related to recursive data, but the assignment is small if we
don't merge the two separate goals into a single milestone.

This assignment is due @(due 'a10).

You can use the reference solution here:
@url{https://www.students.cs.ubc.ca/~cs-411/@|semester|/a10-interrogator.cgi}

@subsubsub*section{Assignment Checklist}
You should find a new repository in your
@url{https://github.students.cs.ubc.ca} account named @tt{a10_<team ids>} with a
code skeleton and a support library.
You should complete the assignment in that git repository and push it to the
GitHub Students instance.

You should first merge your solution to @secref[#:tag-prefixes '("a9:")]{top}
with the starter code provided.
The new starter code has the correct provides and includes a submodule to help
you run your compiler on the command line if you want.
The name of the skeleton is @share{a10-skeleton.rkt} to avoid accidentally
overwriting your files, but your file in the Git repository should be named
@tt{a10.rkt}.
