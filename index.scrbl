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

@(local-table-of-contents #:style 'immediate-only)

@include-section{syllabus.scrbl}
@include-section{calendar.scrbl}
@;include-section{course-info.scrbl}
@include-section{project-logistics.scrbl}
@include-section{readings.scrbl}
@include-section{assignments.scrbl}
