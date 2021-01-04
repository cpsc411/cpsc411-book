#lang scribble/manual

@(require
  "assignment/lib.rkt"
  (for-label racket/base))

@title{Course Project Logistics}

@section{Operating System Support}
Our run-time system aims to support multiple operating systems, but this has not
been tested thoroughly.
We therefore assume you are running on a Linux machine.
Your compiler must produce code for x64 using Linux system calls.
We encourage you to try to support other operating systems by modifying your
compiler to generate operating system specific parts based on the host machine,
@emph{but all assignments must run on x64 linux}.

@section{Project Submission}
We use GitHub for code management and code submission.

For milestones 0--2, you will work alone.
You should commit to the repository in your
@url{https://github.students.cs.ubc.ca} account named
@tt{cpsc411project_<userid>}.
Each milestone should be submitting by creating a new branch as directed in the
milestone description.
For assignment 0, the branch will be @tt{assignment-0}; for milestone 1, the branch
will be @tt{milestone-1}, etc.

These branches should exist in the repository when we provision it and include
some starter code.

For milestones 3--11, you'll work in teams.
We will create a new team, with a team repository named @tt{cpsc411project_<teamid>}.
Each milestone should be submitted by creating a new branch as directed in the
milestone description.
They will follow the same pattern as for the individual milestones.
Milestone 4 should be submitted in branch @tt{milestone-4}, for example.

These branches should exist in the repository when we provision it and include
some starter code.

We will mark and provide feedback on the project in the last commit in the
relevant branch when the due date elapses.
Please commit @emph{and push} early and often.

You may work in other branches, but be sure to merge and push to the correct
branch prior to the deadline.

@section{Organizing Code}
Each milestone must contain a file in the top-level directory of the branch
named @tt{compiler.rkt} that @racket[provide]s all the functions required in the
milestone.
Generally, this includes each pass defined in the milestone, and may include an
interpreters, type checkers, and program validators.

Per @secref[#:tag-prefixes '("book:" "prelim:" "style:")]{top}, you are not
required to keep all of your code in this single file; the file is an interface.
However, you must follow the instructions on organizing your code from
@secref[#:tag-prefixes '("book:" "prelim:" "style:")]{top}, as we will be
grading code on design as well as functionality and need to be able to navigate
code easily.

Regardless of how your code is organized, to simplify grading, you must add a
comment with the exercise number before the code for each of your exercises:
@;
@racketblock[
(code:comment "Exercise 20")
(define (values-lang-fib n) ...)
]

For written exercises, indicate the exercise in the same way followed by a
comment when the response to the exercise:
@racketblock[
(code:comment "Exercise 21")
(code:comment "When I compared the two implementations I found ...")
]

For non-obvious test cases, add a comment explaining which language feature or
edge case you are testing.
For trivial tests, such as
@;
@racket[(check-exn exn:fail? (thunk (check-values-lang '())))],
this is not necessary and will only make your code less readable.
