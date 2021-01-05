#lang scribble/manual

@(require "config.rkt")

@title[#:style '(toc)]{@|course-number| @|semester| -- @|course-title|}

The goal of this course is to give students experience designing, implementing,
and extending programming languages. Students will start from a machine
language, the x86-64 CPU instruction set with Linux system calls (x64), and
incrementally build a compiler for a subset of Racket to this machine language.
In the proceess, students will practice building, extending, and maintaining a
complex piece of software, and practice creating, enforcing, and exploiting
abstractions formalized in programming languages.

The course assumes familiarity with basic functional programming in Racket, and
some simple imperative programming in assembly.

The source and materials for this course are publically available on GitHub,
except for the reference solution.
Please report bugs, typos, etc at in the relevant repository:

@itemlist[
@item{Book - @url{https://github.com/cpsc411/cpsc411-book}}
@item{Support package - @url{https://github.com/cpsc411/cpsc411-pub}}
@item{Interrogator - @url{https://github.com/cpsc411/cpsc411-interrogator}}
@item{Skeletons and dockerfile - @url{https://github.com/cpsc411/cpsc411-skeletons}}
@item{Reference solution and private libraries - @url{https://github.com/cpsc411/cpsc411-priv-issues}}
]

@(local-table-of-contents #:style 'immediate-only)

@include-section{syllabus.scrbl}
@include-section{calendar.scrbl}
@;include-section{course-info.scrbl}
@include-section{project-logistics.scrbl}
@include-section{readings.scrbl}
@include-section{assignments.scrbl}
@include-section{appendix/credits.scrbl}
