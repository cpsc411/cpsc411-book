#lang reader "assignment-lang.rkt"

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@(define (notes . _) (void))
@title[#:tag "top" #:tag-prefix "a11:"]{Assignment 11: The CPSC411 Compiler
Encore}

@section{Assignment Summary}
The goal of this assignment is to (1) give you a chance to polish your compiler
and learn from past mistakes (2) give you time to review the lessons of the past
four assignments prior to the final.

Due Friday, Apr. 24, 2020.
There are no extensions on this assignment; the late policy does not apply.

At the deadline, we will regrade your A10 code base.
You will earn up to 100% of marks back on end-to-end tests @emph{for all
assignments}, and up to 50% of marks back on individual exercises from a6--a10
whose fixes have been integrated into the a10 code base, if:
@itemlist[
@item{You have addressed the issue in your code. (Fixing bugs, adding tests,
redesigning code...)}
@item{You submit a short written document explaining what the problem was and
explaining the fix. If the exercise you lost points on required making a design
decision, you should also explain the design decision or the change in the
design decision.}
]

To receive marks back, you must follow the instructions above and below.
@emph{There will be no exceptions.}

If you have already fixed an issue in the code, do you best to note what was
fixed in your report.
For example, if you noticed a mistake in your A6 code base, but merged the fix
in A7 and the fix is include in your A10 code base, you should try to report
what you fixed during A7.
If you cannot remember many details, don't worry too much about it; we will give
you the benefit of the doubt.

@section{Fixing Your Code}
You should correct the code in the A10 repository and push to the master
branch on the repository.
For example, to fix the code for a6, you should fix the code in your a10 project
repository.

You are not required to fix every exercise; we will skip any code that has not
changed and any exercise not included in the report.

@section{Submitting Your Report}
In addition to fixing the code, you must write a short written report.
You only need one report documenting all fixes.

The report must be in a Markdown file, readable in plaintext, named
@tt{a11-regrade-report.md}, in the top-level directory of the
repository to be regraded.

The file should clearly and concisely describe what fixes you have applied to
your code.
You should label each assignment with a Markdown level-one section head, and
each exercise with a level-two section heading.
There should be only one heading per exercise per assignment.

For example, below is an example of a report for a11 in which I correct several
mistakes in a6 and a7:

@(nested #:style "boxed"
@verbatim{
# Assignment 6
## Exercise 1
- Added missing test purpose statement.
- Fixed bug that reversed the order of arguments on the stack for non-tail
  calls. This resulted from traversing the `new-frames` in the wrong order using
  for/fold, resulting in emitting the instruction sequence in reverse order.
  It's unclear how I managed to do this in exercise 1.

# Assignment 7
## Exercise 5
- Added tests for edge cases of fixnums.
}
)

For simple mistakes, like missing tests, your report can simply enumerate the
fix.
For non-trivial mistakes, your report should indicate that you understand the
problem and the fix.

For individual assignments, you should write this individually.
For team assignments, you should work as a team.

@section{Marking}
We will return up to half of the marks lost on each exercise if:

@itemlist[
@item{Your fix is correct.}
@item{Your report explains the problem and the fix completely and concisely.}
]

For end-to-end tests, we will replace @emph{each assignment's} end-to-end tests
score with the maximum of the score on a11 and the score on the original
assignment. You cannot lose marks by introducing mistakes in a11.
If you score 100% for end-to-end tests in a11, we replace your end-to-end
scoresfor a6, a7, a8, a9, and a10 with 100%.
