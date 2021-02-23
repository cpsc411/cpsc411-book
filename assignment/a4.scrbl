#lang reader "assignment-lang.rkt"

@(require
  (except-in "a2.scrbl" doc)
  (except-in "../chapter/structured-control.scrbl" doc)
  (for-label cpsc411/compiler-lib)
  (for-label cpsc411/reference/a4-solution)
  cpsc411/langs/v4)

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@title[#:tag "top" #:tag-prefix "a4:"]{Milestone 4: Adding Control Flow}

@section{Milestone Summary}
The goals of this milestone are to (1) practice exposing low-level features
from the target language to the compiler, (2) introduce designing abstractions
to tame low-level operations, (3) demonstrate the problems that control flow
causes with program analysis, (4) introduce linking, and (5) practice thinking
about programming at different levels of abstraction.
We'll expose labels and @object-code{jmp} instructions from @ch1-tech{x64},
provide abstractions for using them in the source language, compile the latter
abstraction to the former implementation, and practice reasoning about our new
source language.

This milestone is due @(due 'a4).

You can use the interrogator to get limited access to the reference solution:
@url{https://www.students.cs.ubc.ca/~cs-411/2020w2/interrogator.cgi?an=a4}.

@todo{Design component? Might be a heavy assignment already.}

@subsection{Learning Objectives}
@todo{Redo above summary to split out summary from learning objectives.}

@subsection{Checklist}
@todo{Should be able to generate this}

@emph{Completely new passes}

@typeset-passlist[
link-paren-x64
flatten-program
resolve-predicates
optimize-predicates
expose-basic-blocks
]

@emph{Major modifications}

@typeset-passlist[
interp-paren-x64
undead-analysis
]

@emph{Minor modifications}

@typeset-passlist[
generate-x64
patch-instructions
replace-locations
conflict-analysis
uncover-locals
select-instructions
canonicalize-bind
sequentialize-let
uniquify
]

@emph{No modifications}
@typeset-passlist[
assign-homes-opt
assign-registers
]

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:" "chp4:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

@;@todo{Remove all this, write our own material.}
@;You can find additional reading material on register allocation and undead
@;analysis at the resources below.  These comes from other courses that use a
@;compiler that is similar to, but different from, your compiler.  Their
@;intermediate languages are different, and their compiler pipeline is different.
@;There is no one universal truth of how these passes work for all compilers in
@;all situations, so you will need to generalize from these lessons and apply
@;them to your compiler.
@;
@;Still, the lessons should be helpful if used with care:
@;@itemlist[
@;@item{R. Kent Dybvig's notes on register allocation
@;@url{kents-notes-on-register-alloc.html}.
@;This language allows mixing abstract locations and real locations in the same
@;language, unlike your intermediate languages.
@;
@;These notes are courtesy of R. Kent Dybvig, author of the Chez Scheme compiler
@;@url{https://github.com/cisco/ChezScheme}.
@;}
@;@item{@emph{Essentials of Compilation}, Chapters 3.2, 3.3, and 3.4
@;@share{book.pdf}.
@;These chapters include notes on calling conventions and function call/return,
@;which your compiler should not support yet.
@;
@;This is a freely available book written by the group at Indiana University:
@;@url{https://github.com/IUCompilerCourse/Essentials-of-Compilation}}
@;]

@section{Exercises}

@exercise{Extend @racket[generate-x64] to support control-flow primitives.

To compile @object-code{halt}, it may help if you generate a labeled instruction
that essentially does nothing.
This way, you can insert a label at the end of your program.
Otherwise, you'll need to think carefully about how to arrange and generate
blocks.
}
@todo{Why is halt still in paren-x64?}

@exercise{Design and implement @racket[link-paren-x64], which resolves all
labels into the address of the instruction in the instruction sequence.
}

@exercise{Redesign and extend the implementation of @racket[interp-paren-x64]
to support control-flow primitives.

It should use @racket[link-paren-x64] to resolve all labels, and should use a
program counter to loop over the instruction sequence instead of folding over
the sequence.
It will help to struture the main loop to match the interface of something like
@racket[interp-loop], although you are not required to define it this way.
}

@exercise{Redesign and extend the implementation of @racket[patch-instructions]
to implement the new instructions jump and compare instructions that don't
exist in @ch1-tech{x64} by instruction sequences that are valid in
@ch4-tech{Paren-x64 v4}.

@;It will be tricky to implement the instruction @object-code{(compare addr addr)}
@;with only one auxiliary register.
@;You can do it in four @ch4-tech{Paren-x64 v4} instructions.

Remember to use the auxiliary registers from
@racket[current-patch-instructions-registers].
}

@exercise{Design and implement @racket[flatten-program] to flatten all
@ch4-tech{basic blocks} into straight-line code.
}

@challenge{
In @ch4-tech{Para-asm-lang v4}, it's unnecessary to have a jump when the target of the
jump is the next instruction.
Design and implement an optimization pass, called @racket[inline-jump], that
eliminates these unnecessary jumps.
The source language is @ch4-tech{Para-asm-lang v4} and target is
@ch4-tech{Para-asm-lang v4}
}

@challenge{
Branches are often thought of as expensive, so real compilers often perform
@emph{trace scheduling}, an optimization that rearranges blocks and takes
advantage of fall-through between blocks to reduce the number of jumps.

Implement a function @racket[trace-schedule] to perform this optimization.  The
source language is @ch4-tech{Block-asm-lang v4} and the target language is
@ch4-tech{Block-asm-lang v4}.

This optimization works in conjunction with the previous optimization.
}
@todo{Add these optimizations to chapter.}

@exercise{
Design and implement @racket[resolve-predicates], to resolve the new predicate
language.
}

@exercise{
Design and implement @racket[expose-basic-blocks], which creates new blocks of
any nested @object-code{tail}s and replaces those nested @object-code{tail}s
with jumps.

You may want to use @racket[fresh-label] from @racketmodname[cpsc411/compiler-lib].

In Racket, you can pass multiple return values using @racket[values], and bind them using
@racket[let-values], @racket[let-values*], or @racket[define-values].
You should use @racket[fresh-label] to generate new unique labels.
@todo{elaborate}

Alternatively, you may simply use mutable state.
See @racket[box], @racket[set-box!], and @racket[unbox].
}

@exercise{
Design and implement @racket[optimize-predicates].
You are not required to generate the "best" code (whatever that means).

You can do a better job by passing an environment and recording
known values, or sets of possible values, in @nested-asm-lang-v4[set!]
statements, although in general the value may be "unknown".

You can do an even better job by defining an abstract interpretation for each
relational and binary operator that attempts to compute a value even when one
operand may be unknown.
Abstracting this from the main traversal logic will give you powerful
optimization.
}


@exercise{
Redesign and extend the implementation of @racket[replace-locations] to support
blocks and the control-flow primitives.
}

@exercise{
Redesign and extend the implementation of @racket[assign-registers] to assign
registers for each block.

Recall that you may not use the register @object-code{rax}, since it is used to
patch instructions, or @object-code{rbp}, since it contains the frame pointer.
}

@exercise{
Redesign and extend the implementation of @racket[conflict-analysis] to
decorate each block with a conflict graph, then merge them and decorate the
module with a single conflict graph.
}

@exercise{
Redesign and extend the implementation of @racket[undead-analysis] to collect
@tech[#:tag-prefixes '("book:" "chp-reg-alloc:")]{undead-out sets} for each block.

Only the @object-code{info} field of each block of the output program is
modified, with the addition of the the @tech[#:tag-prefixes '("book:" "chp-reg-alloc:")]{undead-out set}.
}

@exercise{
Redesign and extend the implementation of @racket[uncover-locals] to add a list
of all variables used in each block to the info field of the block.
}

@exercise{
Redesign and extend the implementation of @racket[select-instructions] to
translate value-oriented operations into location-oriented instruction.
}

@exercise{
Redesign and extend the implementation of @racket[uniquify].

You may find this implementation of alpha equivalence for
@ch4-tech{Values-lang v4} helpful: @share{a4-alpha-equal.rkt}.
The code is annotated to demonstrate idiomatic Racket style, and explain feature
you may be unfamiliar with.
}

@exercise{
Design and implement the function @racket[interp-values-lang], an interpreter
for @ch4-tech{Values-lang v4}.
}
