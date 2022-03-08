#lang scribble/manual

@; TODO add a part for prereqs, where we can put some Racket basics, HtDP stuff,
@; maybe match, maybe stylistic things.
@; Want parts, so the chapters in the proper part can have numbers that match
@; the v1--v10 etc.
@title{Prerequisites}

@section{How to Design Programs}
This book assumes familiarity with the design philosophy and vocabulary of
@emph{How to Design Programs (2e)}.
This book is a wonderful systematic approach to how to design small programs, and
individual functions in particular, from scratch.
The guiding principle is that functions should not be written from whole-cloth,
but designed first from their data, and from the various design patterns
(@tech{templates}) often useful for processing, transforming, and computing over
data with particular structure.
That book applies this approach to introductory programming, and thus explains
these concepts from scratch.

\todo{Link properly}

This books applies the same approach to the design of compilers.
We spend the majority of this book discussing the design of the data over which
a compiler operates, and the design patterns required to transform that data
effectively.
In the terms of HtDP, we first design our data, then the @tech{signatures}, then
@tech{templates}; these are the steps of designing any function, including a
compiler pass.
This is where we spend our creative energy.
After we have and fully understand these aspects of a design, the implementation
is almost entirely systematic and straight-forward---each function almost writes
itself.

As we are aimed at experienced software engineers interested in the design and
implementation of compilers, we do not spend much time on the concepts in HtDP.
These concepts the reader is likely already familiar with, although perhaps by
different names.
We do, however, use the same vocabulary, so we briefly introduce this vocabulary
in context.

@todo{Introduce templates, signatures, lists with natural recursion + for/fold,
trees with natural recursion/natural helpers, accumulator template, genrec.
Maybe introduce some of our unique templates: template for imperative validator,
output directed template.

Demonstrate analogy between template and and visitor pattern; others?

Currently, some of this is in the appendix, but that's not really the right
place for it.}
