#lang reader "assignment-lang.rkt"

@;todo{Reset automatically on new assignment.}
@(reset-exercise-counter!)
@(reset-challenge-counter!)

@(require
  (for-label (except-in cpsc411/reference/a3-solution assign-homes))
  (for-label (only-in
              cpsc411/reference/a2-solution
              assign-homes))
  (for-label cpsc411/compiler-lib))

@(define (rtech . x) (apply tech #:tag-prefixes '("book:" "chp-reg-alloc:") x))

@title[#:tag "top" #:tag-prefix "a3:"]{Milestone 3: Register Allocation}

@section{Assignment Summary}

The goal of this assignment is to introduce (1) "optimizing" compilation (2)
register allocation, a critically important "optimization".
In this assignment, you will replace the @racket[assign-homes] pass with
@racket[assign-homes-opt], a version that tries hard to put variables into
registers instead of in the frame.

This assignment is due @(due 'a3).

You can use the interrogator to get limited access to the reference solution:
@url{https://www.students.cs.ubc.ca/~cs-411/@|semester|/interrogator.cgi?an=a3}.

@subsection{Learning Objectives}
@todo{Redo above summary to split out summary from learning objectives.}

@subsection{Assignment Checklist}

@emph{Completely new passes}

@typeset-passlist[
undead-analysis
conflict-analysis
assign-registers
assign-homes-opt
]

@emph{Minor modifications}
@typeset-passlist[
]

@emph{Remove passes}
@typeset-passlist[
]

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:" "chp-reg-alloc:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

@section{Exercises}

@exercise{Implement @racket[undead-analysis].
You may want to see @racketmodname[racket/set] for working with sets.

For working with sets, you may want to use @secref["sets" #:doc '(lib
"scribblings/reference/reference.scrbl")]
}

@exercise{Implement @racket[conflict-analysis].
We provide a functional graph library, @racketmodname[cpsc411/graph-lib], for
working with the conflict graph.
}

@exercise{Implement @racket[assign-registers] to perform graph-colouring
register allocation with spilling.
You should reference @racket[current-assignable-registers].

You may want to use @racket[sort].

It might be difficult to keep enough variables live at one time to test spilling.
You should use @racket[parameterize] and @racket[current-assignable-registers]
to test spilling.

When debugging your register allocator, you might try comparing your allocator
to the graph colouring algorithm provided by @other-doc['(lib
"graph/scribblings/graph.scrbl")].
This provides an implementation of graph colouring, although it does not perform
spilling so it will not work in general.
You cannot use this library in the implementation of your compiler; it may only
be used for testing.
Unfortunately, it uses a different representation of graphs, so you will need to
write conversion procedures.

You may also choose to preserve additional info fields in the output of
each pass, which could be used for more advanced testing.
If you do, you can use @racket[check-assignment] in your compiler pipeline to
detect bugs in the register allocator.
}

@exercise{Implement @racket[assign-homes-opt], which has the same interface and
performs the same function as @racket[assign-homes], but uses register
allocation instead of assigning all @ch2-tech{abstract locations} to
@ch2-tech{frame variables}.

@racket[assign-homes-opt] should be a drop-in replacement for
@racket[assign-homes].

Run your test suite and ensure you get the same outputs whether you use
@racket[assign-homes] or @racket[assign-homes-opt].
}

@exercise{Create two versions of your compiler by defining two functions,
@racket[compile-m2] and @racket[compile-m3].
The source language should be @ch3-tech{Values-lang v3} and the target should be
@ch1-tech{x64}, represented as a string.
@racket[compile-a2] should use @racket[assign-homes], while
@racket[compile-a3] should use @racket[assign-homes-opt] instead.
You should use @racket[parameterize] and @racket[current-pass-list].

Write a paragraph comparing the two compilers.
Consider the following questions:
@itemlist[
@item{What are benefits and the disadvantages of each?}
@item{Is one optimal, and if so, in what sense?}
@item{Try running and timing the same programs compiled with each compiler.
Are there any surprises?}
]
}


@;@todo{Do I want conflict graphs represented as part of a program? Not sure.
@;Always seemed a bit of a hack to me. OTOH, both Jeremy and Kent do it, and it
@;will be easier to decorate programs with live in sets once we add jumps...
@;
@;For now, always using the info-field style}
@;
@;@todo{This is a simpler register/frame allocation process than Kent's,
@;essentially the same as Jeremy and Ryan's; however, it's less realistic.}


@;@examples[#:eval eg
@;(eval:error
@; (check-assignment
@; '(begin ((locals (x.1 y.1))
@;          (conflicts ((x.1 (y.1)) (y.1 (x.1))))
@;          (assignment ((x.1 r9) (y.1 r9))))
@;         (set! x.1 42)
@;         (set! y.1 42)
@;         (set! x.1 (+ x.1 y.1))
@;         (halt x.1))))
@;
@;(eval:error
@; (check-assignment
@;  '(begin ((locals (x.1 y.1))
@;           (conflicts ((x.1 (y.1)) (y.1 (x.1))))
@;           (assignment ((x.1 r9))))
@;          (set! x.1 42)
@;          (set! y.1 42)
@;          (set! x.1 (+ x.1 y.1))
@;          (halt x.1))))
@;]

@exercise[#:optional #t]{
Design and implement a function @racket[bury-dead], which removes assignments
to unused abstract locations.
The source language is @rtech{Asm-lang v2/undead} and the target language is
@rtech{Asm-lang v2/undead}.
(Optimizations often have the same source and target language.)

An assignment to a variable is @rtech{dead} if it is not in the
@rtech{undead-out set} for the instruction.
}
