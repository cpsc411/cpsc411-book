#lang scribble/manual
@(require
  scribble/core
  "config.rkt"
  "assignment/lib.rkt")

@; TODO tag this properly
@title{Syllabus}

@section{Land Acknowledgement}
UBC’s Point Grey Campus is located on the traditional, ancestral, and unceded
territory of the xwməθkwəy̓əm (Musqueam) people.
The land it is situated on has always been a place of learning for the Musqueam
people, who for millennia have passed on their culture, history, and traditions
from one generation to the next on this site.

@hyperlink["https://aboriginal.ubc.ca/community-youth/musqueam-and-ubc/" "Musqueum and UBC"]

@section{Course Description}
The goal of this course is to give students experience designing, implementing,
and extending programming languages.
Students will start from a machine language, the x86-64 CPU instruction set with
Linux system calls (x64), and incrementally build a compiler for a subset of
Racket to this machine language.
In the process, students will practice building, extending, and maintaining a
complex piece of software, and practice creating, enforcing, and exploiting
abstractions formalized in programming languages.

The course assumes familiarity with basic functional programming in Racket, and
some simple imperative programming in assembly.

@section{Course Information}
@tabular[
#:style 'boxed
#:column-properties '(left left)
(list
  (list @bold{Course Title} course-title)
  (list @bold{Course Number} course-number))
]

@section{Prerequisites}
A passing grade in CPSC 311.

@subsection{What if I want to take it but can't fulfill the prerequisites?}
I'm willing to be flexible with students for whom taking CPSC 311 is not
practical or possible.
You should reach out to me well in advance, ideally a semester before hand, if
you think you will not be able to take CPSC 311 before CPSC 411.

@subsection{Judging your own background}
This course demands programming in a functional programming languages, learning
and applying new data structures and algorithms, and applying basic programming
languages theory to the implementation of programming languages.

To help judge whether you have the background to succeed in this course, I would
ask the following questions.
My goal is not to dissuade you from taking the course or to try to keep you out
of it.
I want to teach as many people as there are who want to take it.
But I also want to make sure you can make an informed choice about whether you
have and/or can pick up the background.

To gauge your functional programming background:
@itemlist[
@item{Would you be comfortable if I asked you to write a function that adds 1 to
every element of an immutable list using a fold?}
@item{Have you used pattern matching to destructure data?}
@item{Can you implement the factorial function as a pure (uses no mutable
variables) tail-recursive function?
What about the Fibonacci function?}
]

To gauge assorted CPSC background, usually introduced in CPSC 110, I would
consider:
@itemlist[
@item{Can you systematically describe the data a function takes as input and
produces as output? Particularly, recursive, tree-structured data.
By this I mean to come up with a description in some language of a new data
structure from an English description, give examples of the data
structure.
}
@item{Can you systematically design a function that operates on tree-structured
data?
That is, knowing only that a function consumes some tree-structured data, can
you come up with a skeleton of the function's code without knowing anything
about the function's purpose?
}
]

To gauge general programming languages background, the basic concepts introduced
in CPSC 311, I would ask:
@itemlist[
@item{Have you ever written an interpreter?}
@item{Do you understand the duality between code and data? Can you encode data
using code, or vice versa?}
@item{Do you understand lexical vs dynamic scope? Have you implemented interpreters
with each?}
@item{Can you translate an imperative program into a functional one, and vice versa?}
]

@subsection{Self-studying the background material}

The CPSC 311 material is usually online:
@url{https://www.students.cs.ubc.ca/~cs-311/2019W1/notes.html}
@url{https://www.students.cs.ubc.ca/~cs-311/2019W1/assignments.html}

Last I checked the notes, the following lectures were particularly relevant
background for CPSC 411: Lectures 1--13,16--22,27--30.
These cover the following topics:
@itemlist[
@item{Parsing}
@item{First Language: Concrete Syntax, abstract syntax, parsing and an interpreter}
@item{Identifiers}
@item{First-order function}
@item{Environments, static and dynamic scopes}
@item{First-class functions}
@item{Substitution}
@item{Mutable state}
@item{Call-by-reference semantics}
@item{Recursion via boxes}
@item{Continuations}
@item{Continuation-passings tyle}
@item{Types}
@item{Program rewriting}
@item{Object-oriented languages (encoding objects)}
]

In addition to to CPSC 311 notes, it might be helpful to work through some of
the chapters and exercises in "How to Design Programs".
The text is available for free online.
Below, I link to a particularly relevant chapter:
@url{https://htdp.org/2019-02-24/part_two.html#%28part._ch~3alists1%29}

It's an intro text so some of may be directed at someone with less programming
experience, but it's also introducing a different programming paradigm so it
might be a helpful resource.

CPSC 411 is in Racket.
We won't rely on very many of Racket's features, we also won't spend time introducing
the language, and I'll expect students to be capable of looking through
documentation and the standard library themselves if they need to.

If you think you can handle building the project in a new language and a new
programming paradigm, and picking up the background material, contact me and
I'll consider waiving the course requirement.
Email me and tell me your favorite meme to confirm you've read this section.

@section{Course Materials}
No textbook is required.
Weekly lecture notes will be posted, and additional reading from a freely
available electronic textbook may be suggested.

@(define (mailto addr)
   (hyperlink (format "mailto:~a" addr) addr))

@section{Contacts}
@tabular[
#:style 'boxed
#:column-properties '(center)
#:row-properties '(bottom-border)
(list
  (list @bold{Instructor} @bold{Contact Details} @bold{Office} @bold{Office Hours})
  (list "Prof. William J. Bowman" @mailto["wilbowma@cs.ubc.ca"] "ICCS 389" "TBD"))
]

@tabular[
#:style 'boxed
#:column-properties '(center)
#:row-properties '(bottom-border)
(list
(list @bold{TA} @bold{Contact Details} @bold{Office} @bold{Office Hours})
(list "Lily Bryant" @mailto["labryant@cs.ubc.ca"] "" "TBD")
(list "Paulette Koronkevich" @mailto["pletrec@cs.ubc.ca"] "" "TBD")
)
]

@subsection{Communication about the Course}
There are three primary means for communicating between those involved in this
course, in addition to the regular lecture sections.
In no particular order, these are:

@itemlist[
@item{Office hours}
@item{@hyperlink["https://piazza.com/ubc.ca/winterterm22021/cpsc411" "Piazza"]

    The Piazza course discussion board. The instructor, TAs, and all students are expected to read the discussion board on a very regular basis. Late breaking information about milestones, corrections to mistakes made in class, and generally useful information will often be communicated through Piazza. You are responsible for keeping up with posts on Piazza -- pleas of ignorance will not be received.

    Piazza is also a very effective means of communication if used correctly. Questions about the milestones should be posted to the Piazza rather than being emailed to the instructor or a TA, as everyone will see both the question and its answer. All students are encouraged to post answers to questions posed on Piazza as well.
}
@item{Email

    Email to the instructor or TAs is effective in limited circumstances. It is the most effective means of scheduling a consulting time when the regular hours are impossible or insufficient. It is an appropriate means of asking questions concerning the grading of milestones and examinations. It is not an appropriate means of asking questions about the milestones - see Piazza.
}
]

@section{Course Structure}
The course is structured around semester-long project in which students design
implement a programming language by building a compiler to x64.
The project is divided into weekly milestones designing and implementing an
extension to the previous week's compiler.
During lecture, we will walk through the limitations in the prior week's
language, design a new source language, and introduce necessary background
knowledge of algorithms, translations, optimizations, and machine implementation
details.

The milestones @emph{a0--a2} will be completed individually, and the rest of
the project will be in teams of three.

There will be a midterm and final that focus on the concepts covered in lecture
and in milestones.
The structure of the exams may be different than the milestones.
While the milestones focus on implementing a compiler, the exams will test you
on the concepts and design elements that are discussed in lecture and are
implicit in the milestones.

@subsection{Working in Groups}
How you organize work between yourself is ultimately up to you.
We recommend you use @hyperlink["http://en.wikipedia.org/wiki/Pair_programming"
"pair programming"] (recommended because it is more fun and helps each of you
learn from one another).
If, however, you decide to divide the workload between team members, this is not
strictly forbidden.
However, we expect that each team member completely understands the code
developed by other team members.
Each team member should be prepared to answer questions about all
specific code details or implementation decisions.
See also the section on Project Evaluation below.
If you cannot answer questions about the solution you submitted you should not
expect any credit for submitting it.

@section{Learning Objectives}
@itemlist[
  @item{Implement the basic translations necessary to compile modern high-level
  languages to modern register machine architectures.}

  @item{Identify and describe limitations in existing languages.}

  @item{Design and implement languages to lift limitations.}

  @item{Describe language features at different levels of abstractions,
  corresponding to different stages of compilation.}
  @;todo{Not sure about this one}

  @item{Differentiate between invariants that must be enforced, and
  invariants that can be relied upon.}
  @;todo{Abstractions? Invariants?}
]

@section{Schedule of Topics}

@itemlist[
#:style 'numbered
@item{
  Intro. to language design: understanding a language, statement-based language
  @itemlist[
    @item{x64}
    @item{interpreters}
    @item{type checking}
  ]
}

@item{
  Creating abstractions: variables and value-based language
  @itemlist[
  @item{the stack}
  @item{run-time systems}
  @item{instruction selection}
]
}

@item{
  The pros and cons of abstraction
  @itemlist[
    @item{register allocation}
    @item{program analysis}
    @item{graph coloring}
    @item{optimization}
  ]
}

@item{
  Exposing low-level control flow
  @itemlist[
   @item{jumps}
   @item{conditional statements}
   @item{basic blocks}
   @item{control flow and program analysis}
  ]
}

@item{
  Abstracting control flow
  @itemlist[
    @item{tail calls (non-returning functions)}
    @item{continuation-passing style}
    @item{calling conventions}
    @item{control flow and program analysis}
  ]
}

@item{
  Function call and return
  @itemlist[
    @item{non-tail calls}
  ]
}

@item{
  Exposing memory access
  @itemlist[
    @item{heap}
    @item{memory safety}
  ]
}

@item{
  Adding data structures and primitives
  @itemlist[
    @item{tagging}
    @item{immediate data: booleans, symbols, integers}
    @item{structured data: strings, lists, arrays}
    @item{dynamic typing}
    @item{memory management}
    @item{garbage collection}
  ]
  @;@todo{Garbage collection}
}

@item{
  First-class computations
  @itemlist[
    @item{closure conversion}
    @item{lexical scope}
    @item{closure optimizations}
  ]
}

@item{
  Surface language niceities
  @itemlist[
    @item{implicit casts}
    @item{syntactic sugar}
  ]
}
@;@item{
@;  Do what I mean: allow any expression anywhere
@;  @;@todo{Not exactly sure about this yet}
@;}
@;
@;@item{
@;???
@;@todo{Not exactly sure about this yet}
@;}
]

@section{Evaluation}
The following is a tentative grading scheme for the course, although it is
subject to change at the instructor's discretion.

@itemlist[
@item{Project -- 50%}
@item{Midterm -- 20%}
@item{Final -- 30%}
]

You must pass both the project and the final to pass the course.

@subsection{Late Policy}
The entire project must be handed in by the project due date at the end of the
semester; there will be no extensions.
If you have a local commit that was before the deadline, but you forgot to push
it, then it will not be graded.

We will provide weekly milestone deadlines, and will provide feedback on the
last commit on the milestone branch made prior to this deadline.
This feedback is not graded, but you will be given comments on the design and
implementation of your project, as well as summary feedback of "NS" (not
satisfactory), "S" (satsifactory), or "S+" (above satsifactory) on your progress
so far.
If you receive an "NS", you should meet with a TA or instructor to get ensure
you have a plan to get your project back on track.
If you miss a milestone deadline, you receive no feedback, and it does not
directly impact your grade.

@subsection{Project Evaluation}
The project, the compiler you write through this course, will be divided into
approximately 10 milestone.
At the end of the semester, your entire compiler will be thoroughly tested and
hand-graded for design elements.
This will constitute the entire project grade.

You are required to complete the first 7 milestones to pass the course project,
but you may choose to stop any time after completing milestone 7.
Your maximum grade on the project will be 10% for each milestone you choose to
complete; however, you will receive a bonus 10% on your project grade if your
milestone is perfect according to our rubric---it passes all of our tests
(including private tests), and the code is well designed.
The final 10% is all or nothing.
This means if you complete milestone 7 perfectly, you will receive an 80% on the
project; if you complete milestone 7 almost perfectly, you get approximately
70%.
If you complete milestone 8 perfectly, you will receive 90% on the project, but
80% or less for an imperfect milestone 8.
If you complete milestone 10 pefectly, you will receive 110% on the project, but
100% or less for an imperfect milestone 10.

By the end of the course, you must declare which milestone you intend to be your
final milestone.
You do not have to declare this to the instructors before the end of the
semester, but you and your group will need to agree before hand.
This should be clearly communicated in the README in the main branch of your
project.
We will grade to rubric and the test suite for the milestone you declare, and
grade the compiler passes included up to that milestone.

We will evaluate more than merely whether or not your code "works".
We may review your code and/or ask you as a group to demonstrate your compiler
as part of determining your final grade.

You should be prepared to:
@itemlist[
@item{Demonstrate that your solution works in a live demo.}
@item{Answer questions about all the code you submitted as a team.}
@item{Defend various implementation and design choices you have made during the course of the project.}
@item{Show that you can "think outside the box" about alternative implementation strategies or extensions to the project which may not have been part of the original project specification.}
]

If you used a division of labor rather than collectively developing the
compiler, you should be fully aware of the other member's work, and should be
able to answer questions about the whole project as if it was entirely your own
work.

It is very important that you think about different possible
implementation/design choices throughout the project and make informed
decisions.
It is often the case that no single choice is "the best".
Each strategy typically has some benefits and some drawbacks in terms of
efficiency, code maintainability, ease of implementation etc.

To demonstrate that you can think about these choices and their repercussions,
you should be able to explain alternatives and their relative benefits, as well
as justify the often subjective value judgements that are inherent in choosing
one solution over another.

@subsection[#:tag "grading-design"]{@elem[#:style (style "red" (list (color-property "red")))]{Important}: We will grade you on design}
In each milestone description, we use the word @emph{design} to remind you that you should
not merely write code, but @emph{design} it.
You should @emph{design} your code, a la the design recipe, for every
assessment in this class.
It should go without saying, but we have to say it.
So here, we are saying it very explicitly:

You must follow the design recipe; start @emph{designing first}, then implement
the exercises.
When you start by designing your code, you will force yourself to think through
your code carefully before writing it.
We will grade your design.
We will grade your design even if we forgot to say "design".
Early starter code will demonstrate good design.
However, even if the starter code does not demonstrate good design, or
explicitly say how to design the code, or list the parts of the design recipe,
@emph{you must design your compiler passes and not merely implement them}.

You must write a signature and purpose statement.
If the starter code does not have a signature or purpose statement, you must
write them.
The signature should say what kinds of data the inputs and outputs are.
The purpose statement should be a terse, plain English description of what code
is meant to accomplish.
You should write these first, as they help you think through the code you're
about to write.
You will lose marks if you do not write them.

You should follow the template for your input language.
The template will help ensure you cover all the cases, get you started writing
your code, and avoid unnecessarily complicated code.
You're not writing simple functions like in CPSC 110 now, so this is not a hard and fast
rule.
Sometimes, not very often, it will make sense to compose functions or write
higher-order functions, or use mutable variables.
But when in doubt, you should follow the template.

Write your tests before you write your code.
Use @other-doc['(lib "rackunit/scribblings/rackunit.scrbl")] for testing.
You must write tests.
@(require scribble/core)
You must write unit tests for each pass.
You must write integration tests for the entire compiler for each milestone.
Your tests must test each kind of expression in the source language.
Your tests must test edge cases---what happens with @code{0}, with @code{-2^63}, with
@code{2^63-1}, etc.
You will lose marks if you do not write thorough tests.
You will then lose marks again when we find bugs in your code.

We do not want you to lose marks.
We want you all to get an A and learn to be BAMF compiler hackers.
Designing your compiler will teach you much more than merely implementing it.
So design your code, then write it.

@;In addition to input/output tests, we encourage you to do property-based
@;testing.
@;Write checkers for intermediate languages, even if we don't tell you to.
@;(We will tell you in the first few assignments.)
@;Run your test suite with the checker as an extra pass.
@;It will catch bugs.
@;Write interpreters for intermediate languages, even if we don't tell you to.
@;Compare the output of your compiler to the output of your interpreter.
@;This will catch bugs.
@;This will be a valuable tool for quickly and easily catching subtle bugs.

See @secref[#:tag-prefixes '("book:")]{sec:recipe} for more on the design recipe in the context of
compilers.

@subsection[#:tag "grading-tests"]{Sharing Tests}
To encourage discussion of the design of your compilers, you may discuss and
share @emph{tests} on Piazza.
However, you may not copy more than half of the tests required for any
individual exercise from other students.
You or your group must write at least half of your test suite.

You may also discuss ideas for property-based testing, without sharing the
implementations for type checkers or interpreters.

No other code should be shared between individuals or teams.

@section{Copying and Plagiarism Policy}
Please read the departmental policy on Plagiarism and Collaboration
@hyperlink["http://www.cs.ubc.ca/about/policies/collaboration.shtml" "here"].

You might find it interesting to read Dr. Tamara Munzner's
@hyperlink["http://www.cs.ubc.ca/~tmm/courses/cheat.html" "Cheating: The List Of
Things I Never Want To Hear Again."]

Plagiarism means passing of someone else's work as your own.
Plagiarism is a serious offense and dealt with quite harshly by the university.
Cases are forwarded to the Dean's office and can result in suspension from the
university as well as an entry on your academic transcript.

The key to using other people's code without committing plagiarism is to make it
absolutely clear how much of your project is your own work and how much it is
based on someone else's work.
When copying something, or adapting it as part of your solution, @bold{you are fully
responsible to provide clear references to your sources, and explain to what
extent your solution is based on these sources}.
Code comments are a good way to do that for programming assignments.

For the individual assignments, you are expected to work alone.
If you do rely on other people's work, for whatever reason, and properly explain
the nature and extent of the collaboration, this is not plagiarism (@emph{i.e.}, with
proper explanation, you are not implying that the work is your own).
You can submit such a properly commented solution for partial credit.

For the rest of the project you will work in teams.
It is expected that the work submitted as a team is collaborative work.
As such we will treat the code as being developed by all team members together.
We do not require you to provide comments about the details of exactly who wrote
every single line of code.
However, during the demo, if we sense that there is an imbalance in the
contributions made by each team member, we may ask for clarification and we may
reflect this imbalance in your final marks.

You are not supposed to copy code or otherwise collaborate with people other
than those on your own project team.
If you do, then you can still submit your work for partial credit, provided that
you provide adequate documentation of your sources and the extent to which your
solution is derived from them.

When you are provided starter code, as is the case in most of our assignments,
it is automatically assumed that you did not write the starter code yourself.
It is therefore not necessary to provide explicit documentation to explain this.
On the other hand, it is automatically assumed that @bold{you are claiming any code
that differs from the original starter code as entirely your own work.
If this is not the case, you are fully responsible to provide a clear and
complete explanation of this fact.}
Any failure to do so will be considered academic misconduct. Forgetfulness is
not an excuse.

It is also consider academic misconduct to share your own code outside your
group.
Do not share your solutions, either privately or by making them publically
accessible on code sharing websites such as GitHub.
