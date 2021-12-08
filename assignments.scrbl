#lang scribble/manual

@title[#:style '(toc) #:tag "top" #:tag-prefix "milestone:"]{Project Milestones}

In this course you will build an optimizing ahead-of-time compiler from a
high-level language with functional and imperative features to x86-64.
While a single piece of software, the project is presented through a series of
milestones in which you will gradually build and refine your compiler.
After each milestone, you will have a complete compiler from some source
language to machine code.

The project proceeds bottom-up.
We start with the "lowest" level of abstraction, assembly language, and
gradually design and implement new layers of abstraction as languages.

The milestones have not been updated for this semester yet; you can view the
prior terms milestone at @url{https://www.students.cs.ubc.ca/~cs-411/2020w2/milestone_top.html}.
There will be some changes this semester.

@(local-table-of-contents #:style 'immediate-only)

@include-section{assignment/a0.scrbl}
@include-section{assignment/a1.scrbl}
@;include-section{assignment/a2.scrbl}
@;include-section{assignment/a3-wrong.scrbl}
@;include-section{assignment/a3.scrbl}
@;include-section{assignment/a4-wrong.scrbl}
@;include-section{assignment/a4.scrbl}
@;include-section{assignment/a5.scrbl}
@;include-section{assignment/a6.scrbl}
@;include-section{assignment/a7.scrbl}
@;include-section{assignment/a8.scrbl}
@;include-section{assignment/a9.scrbl}
@;include-section{assignment/a10.scrbl}
@;include-section{assignment/a11.scrbl}
