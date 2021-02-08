#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a3-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v2
  (for-label cpsc411/langs/v2)
  cpsc411/langs/v2-reg-alloc)

@(provide
  (except-out (all-defined-out) sb))

@(define sb
  (make-cached-eval
  "ch-reg-alloc-eval"
  '(require cpsc411/reference/a3-solution cpsc411/compiler-lib)
  '(current-stack-size 512)))

@declare-exporting[cpsc411/reference/a3-solution]
@todo{Triple check undead-in vs undead-out sets}
@todo{Version numbers out of sync}

@define[reg-alloc-graph
@dot->svg{
digraph {

node [ fontname="Courier", shape="box", fontsize=12 ]

/* The Languages */

L4 [label="Asm-lang-v2"];

L62 [label="Nested-asm v2"];

/* Register allocator */

subgraph DoNotcluster0 {
  graph [labeljust=right,
    style=filled,
    color=lightgrey,
    fontname="Courier",
    fontsize=12,
    label = "assign-homes";
  ];
  edge [fontname="Courier", fontsize=10, labeljust=right]

  L5 [label="Asm-lang v2/locals"];
  L51 [label="Asm-lang v2/undead"];
  L52 [label="Asm-lang v2/conflicts"];
  L6 [label="Asm-lang v2/assignments"];
}

edge [fontname="Courier", fontsize=12, labeljust=left]

L4 -> L5 [label=" uncover-locals"];
L5 -> L51 [label=" undead-analysis"];
L51 -> L52 [label=" conflict-analysis"];
L52 -> L6 [label=" assign-registers"];
L6 -> L62 [label=" replace-locations"];

L4 -> L62 [label=" assign-homes"];
}
}
]

@title[#:tag "top" #:tag-prefix "chp-reg-alloc:"]{Register Allocation}

@section{Preface: What's wrong with our language?}
With @ch2-tech{Asm-lang v2}, we introduced @ch2-tech{abstract locations} to free
the programmer from thinking about @ch2-tech{physical locations}.
Unfortunately, our implementation strategy has a severe limitation.
While it's simple and works, it's extremely slow!
Each and every variable assignment or reference accesses memory.
While memory accesses have improved a lot compared to old computers due to
caching, accessing memory are still orders of magintude slower than accessing a
register when our variable is not in the cache (and, in general, it won't be in
cache).
Our compiler will have better performance if we help the machine out by using
registers as much as possible.

Assigning variables to registers automatically is a non-trivial task.
There's a reason we started out by compiling to @ch2-tech{frame variables}.
There are infinitely many @ch2-tech{frame variables}, but only 16 registers.
To assign an @ch2-tech{abstract location} a new @ch2-tech{frame variable} is
trivial---just pick a new one, there's always new one.
It's a simple, systematic algorithm.
To assign an @ch2-tech{abstract location} to a register, however, we need to
pick a register that isn't currently in use.
Since there's only 16 registers, we very quickly run out of registers... unless
we're clever.

In general, being clever should be a last resort.

@section{Optimizing Location Assignment}
Conceptually, register allocation is a simple idea.
@itemlist[
@item{Undead analysis: figure out which @ch2-tech{abstract locations}
might still be needed after each instruction.
}
@item{Conflict analysis: figure out which @ch2-tech{abstract locations}
cannot be assigned to the same register.
}
@item{Register allocation: assign each @ch2-tech{abstract locations} to a
register that is different from any conflicting @ch2-tech{abstract locations}.
}
@item{Spilling: if we fail to find a register for an @ch2-tech{abstract
locations}, put it in a @ch2-tech{frame variable}.
}
]

Our @ch2-tech{Asm-lang v2} compiler is already a trivial implementation of this
algorithm.
It assumes every @ch2-tech{abstract location} might be needed forever, and is in
conflict with every other @ch2-tech{abstract location}.
It doesn't bother trying, and thus fails, to find a register for every
@ch2-tech{abstract location}, and so spills everything to the frame.
But we want to optimize this process.

In general, we will never do a perfect job, due to Rice's Theorem.
@margin-note*{Rice's Theorem is a huge buzz kill.
It tells us, formally, that we cannot decide anything "interesting" about any
Turing-complete program.
Formally, "interesting" is defined as any property that is not either true for
every program, or false for every program.
"Decide" means write a program that returns either true or false on all programs.
The halting program is an instance of Rice's Theorem.

Rice's Theorem tells us we will @emph{never} be able to do a perfect job---not
when optimizing, not when analyzing, not when asking whether two programs are
equivalent, not when deciding which program is "better".
We will only be able to make trade-offs.
}
We want to try to do a better job.
Well, not better---we can never decide what is "better" due to Rice's Theorem.
But we'd like to produce code that runs faster, and are willing to trade a more
complex and slower compiler for it.

Since register allocation is a modification of our existing @ch2-tech{abstract
location} assignment strategy, we already have a source and target language in mind.
We start from an existing language @ch2-tech{Asm-lang v2/locals}, which is
reproduced below:

@bettergrammar*[asm-lang-v2/locals]

This language, the output of @racket[uncover-locals], records which
@ch2-tech{abstract locations} are referenced in the program and thus need to
be assigned @ch2-tech{physical locations}.
The register allocator takes over from here to perform that assignment.

And we use @ch2-tech{Asm-lang v2/assignments}, reproduced below.

@bettergrammar*[asm-lang-v2/assignments]

After this, our existing pass @racket[replace-locations] should handle replacing
@ch2-tech{abstract locations} by the assigned @ch2-tech{physical locations}.

An optimization should be seen as a drop-in replacement for some existing
functionality, but one that performs "better" (for some definition of "better").
We will replace @racket[assign-homes] with @racket[assign-homes-opt].

@nested[#:style 'inset]{
@defproc[(assign-homes-opt [p asm-lang-v2?]) paren-asm-v2?]{
Compiles @tech{Asm-lang v2} to @tech{Paren-asm v2}, replacing each
@tech{abstract location} with a @tech{physical location}.
This version perform @tech{graph-colouring register allocation}.
}
}

@section{Undeadness Analysis}
We begin the register allocation by figuring out which locations might still
be needed after each instruction.
This process is often called liveness analysis, but we refer to it as undeadness
analysis for reasons we explain shortly.

A variable (either @ch2-tech{abstract location} or @ch2-tech{physical location})
with a @emph{particular} value that @emph{definitely} will be used is considered
@deftech{live}, and any variable (with a particular value) that
@emph{definitely} won't be used is considered @deftech{dead}.
Recall that due to Rice's Theorem, we know it's generally impossible to decide
whether a variable is @tech{dead} or @tech[#:key "live"]{alive}.
This means that when writing an analysis, we must assume partial knowledge.
The result is that we ignore @tech{live}ness entirely.
@todo{Want to make clear the variable-at-a-point-in-time abstraction is what
we're analyzing, not merely variable.
This is why SSA is valuable; makes variables = variables-at-point-in-time.}
@margin-note{
@tech{asm-lang-v2/locals} is a simple enough language that we can tell whether a
variable is @tech{dead} or @tech[#:key "live"]{alive}.
Later, when we add new instructions, we will modify the @tech{undead} analysis
and find variables that aren't necessarily @tech{live} or @tech{dead}, and must
be assumed to be @tech{undead}.
This is also necessary if we want to handle linking, or separate compilation.
}

We cannot in general decide the value of a variable at a given instruction.
Instead, we focus on analyzing each @emph{assignment} to a variable, which
change the value of the variable.

We assume that any variable that gets used, or might get used, might @emph{not}
be @tech{dead}, @ie we assume it is @deftech{undead}, and consider a variable
@tech{dead} only when we have conclusive proof---like witnessing an instruction
driving a new value through its heart, er, storing a new value in its location.

We collect the @tech{undead} variables into sets.
The @deftech{undead-out set} of an instruction is a set of variables that are
@tech{undead} after executing that instruction.
@margin-note{
Most compilers call these live-out or live-after sets.
This suggests that variables are definitely alive, and that the analysis is
computing sets of variables that are definitely alive, neither of which is true.
@tech{Undead} is not exactly the same as not-definitely-dead, except in horror
movies and video games, but it's more suggestive of reality for compilers.
}

@todo{This is but one algorithm. Should rewrite to make that clear and cite algorithm}
To determine whether a variable is in the @tech{undead-out set} for an
instruction, we analyze the program by looping over the instruction sequence
backwards, starting with the last statement.
@margin-note*{We don't @emph{need} to analyze the program backwards, but it's
faster.}
The loop takes an instruction and its @tech{undead-out set}.
We analyze each instruction with its @tech{undead-out set} and compute an
@deftech{undead-in set} for the instruction, which is the same as the
@tech{undead-out set} of the preceding instruction.
That is, the @tech{undead-in set} for an instruction @object-code{s_i} is the
@tech{undead-out set} for the instruction @object-code{s_{i-1}} in the program.

Each iteration of the loop performs the following analysis on a particular
instruction.
We start making by assuming the @tech{undead-in set} is the same as the
@tech{undead-out set}, then update it depending on what happens in the
instruction.
If a variable is @emph{used} in the instruction, it @emph{ought to be}
@tech{live}---we don't actually know, since that instruction's result itself
might not be used, but the variable is at least acting like its
@tech{live}---and is added to the @tech{undead-in set}.
If a variable is @emph{assigned}, @ie its value is overwritten, in the
instruction, it is @emph{definitely} @tech{dead} at that point and we remove it
from the @tech{undead-in set}.


To start the loop, this algorithm requires a default @tech{undead-out set} for
the last instruction; the default @tech{undead-out set} for
@tech{Asm-lang v2/locals} is empty.
In general, the default set may not be empty, because we may assume that some
values are live after the program.
For example, in @tech[#:tag-prefixes '("book:" "chp-boilerplate:")]{Paren-x64
v1}, we assume @object-code{rax} is live out.

This algorithm creates the @tech{undead-out sets} for each instructions so that
later passes can associate each instruction with its @tech{undead-out set}.
There are many ways to associate the @tech{undead-out sets} with instructions.
A simple way is to create a data structure that maps each set to an instruction.

Since our programs are simple lists of instructions, a list of sets is a good
representation of the @tech{undead-out sets}.
We define the data @deftech{undead-set list} as a list of @tech{undead-out
sets}, where the position in the list maps the @tech{undead-out set} to an
instruction in a corresponding @tech{instruction sequence}.
The first element of the list represents @tech{undead-out set} for the first
instruction, the second element represents the @tech{undead-out set} for the
second instruction, and so on.
An @tech{undead-set list} @object-code{(undead-out-set? ...)} together with a
list of instructions @object-code{(s ...)} can be traversed together using the
template for for two lists simultaneously:
@racketblock[
(define (fn-for-ss-and-undead-outs ss undead-outs)
  (match (cons ss undead-outs)
    [(cons '() '())
     (... case-for-empty ...)]
    [(cons `(,s ,rest-ss ...) `(,undead-out ,rest-undead-outs ...))
     (... (fn-for-s-and-undead-out s undead-out)
          (fn-for-ss-and-undead-outs rest-ss rest-undead-outs))]))
]


To describe the output of the analysis, we define a new @tech{administrative
language}.
We collect the @tech{undead-set list} a new @asm-lang-v2/undead{info} field.
Below, we define @deftech{Asm-lang v2/undead}.
The only change compared to @ch2-tech{Asm-lang v2/locals} is in the
@asm-lang-v2/undead[info] field, so we typeset the difference in the
@asm-lang-v2/undead[info] field.

@bettergrammar*-diff[asm-lang-v2/locals asm-lang-v2/undead]

@nested[#:style 'inset]{
@defproc[(undead-analysis [p asm-lang-v2/locals?]) asm-lang-v2/undead?]{
Performs undeadness analysis, decorating the program with @tech{undead-set
lists}.
Only the info field of the program is modified.

@examples[#:eval sb
(undead-analysis
 '(module ((locals (x.1)))
    (begin
      (set! x.1 42)
      (halt x.1))))

(undead-analysis
 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
    (begin
      (set! v.1 1)
      (set! w.2 46)
      (set! x.3 v.1)
      (set! p.1 7)
      (set! x.3 (+ x.3 p.1))
      (set! y.4 x.3)
      (set! p.1 4)
      (set! y.4 (+ y.4 p.1))
      (set! z.5 x.3)
      (set! z.5 (+ z.5 w.2))
      (set! t.6 y.4)
      (set! p.1 -1)
      (set! t.6 (* t.6 p.1))
      (set! z.5 (+ z.5 t.6))
      (halt z.5))))
]}}

An important corner case to consider is what happens when unused variables
appear in a program.
@examples[#:eval sb
(undead-analysis
 '(module ((locals (x.1 y.1)))
    (begin
      (set! y.1 42)
      (set! x.1 5)
      (halt x.1))))

(undead-analysis
 '(module ((locals (x.1 y.1)))
    (begin
      (set! x.1 5)
      (set! y.1 42)
      (halt x.1))))
]

In a realistic compiler, unused variables should be removed be an optimization.

@;@challenge{
@;Design and implement the function @racket[bury-dead], which removes assignments
@;to unused abstract locations.
@;The source language is @tech{Undead-loc-lang} and the target language is
@;@tech{Undead-loc-lang}.
@;(Optimizations often have the same source and target language.)
@;
@;An assignment to a variable is @tech{dead} if it is not in the @tech{undead-out
@;set} for the instruction.
@;}


@section{Conflict Analysis}
To assign @ch2-tech{abstract locations} to @ch2-tech{physical locations}
efficiently, we need to know when any two variables are @emph{in conflict}, @ie
cannot be assigned to the same @ch2-tech{physical location} because different
values might be in those variables at the same time.

We start by defining what a conflict is precisely.

@emph{True Definition of Conflict}
Any variable that gets a new value during an instruction is @deftech{in
conflict} with every variable that (1) has a different value at the same time
and (2) will still be used after that instruction.

Unfortunately, due to Rice's Theorem, we cannot decide either property.
We cannot figure out the value of every variable before run time; if we could,
compiling would not be necessary.
We also do not know variables are @tech{live}, only which are @tech{undead}.
We therefore only approximate conflicts.

To approximate conflicts, we ignore values, and once more focus on assignments
to variables.
An assignment to the variable this means the variable @emph{might} take on a
new value that @emph{might} be different than the value of any variable which
@emph{might} be @tech{live} at that point.
We have already approximate liveness via @tech{undead-out sets}, so what reminds
is to approximate when a variable takes on a new value.
We describe this, and slightly refine this criteria, below.

We represent conflicts in a data structure called a @deftech{conflict graph}.
Interpreted as an undirected graph, the variables as represented as nodes (also
known as vertexes), and conflicts between variables are represented as an edge
the variable to each of the variables in the associated set.
If there is an edge between any two nodes, then they are in conflict.
Interpreted as a dictionary, the @tech{conflict graph} maps each variable to a
set variables with which it is in conflict.

@todo{Assigned vs defined; also "assigned" not defined before use (above and in
undead analysis). "Assigned" is also bad since it overlaps with "register
assignments".}
We create a @tech{conflict graph} from the @tech{undead-out sets} as follows.
Any variable that is @emph{defined} during an instruction is in conflict with
every variable (except itself) in the @tech{undead-out set} associated with that
instruction.
For example, the variable @asm-lang-v2[x.1] is defined in @asm-lang-v2[(set! x.1
(+ x.1 x.2))], while the variables @asm-lang-v2[x.2] and @asm-lang-v2[x.1] are
referenced.
No variable can conflict with itself, since it always has the same value as
itself.
We approximate the values of variables by assuming each @emph{definition}
assigns a new unique value to the variable, and by assuming each variable in the
@tech{undead-out set} also has a unique value.
This approximation tells us that @asm-lang-v2[x.1] cannot be assigned the same
@ch2-tech{physical location} as any variables in the @tech{undead-out set}.
If @asm-lang-v2[x.2] is @tech[#:key "undead-out set"]{undead-out} at this
instruction, and we try to put @asm-lang-v2[x.1] in the same @ch2-tech{physical
location} as @asm-lang-v2[x.2], then the value of @asm-lang-v2[x.2] would have
been overwritten by the value of @asm-lang-v2[(+ x.1 x.2)].

We can reduce the number of conflicts, and thus possibly fit more variables into
registers, by observing that one instruction does tell us that two values will
be the same.
A move instruction, such as @object-code{(set! x.1 x.2)}, is an instruction that
simply defines the value of one variables to be that of another.

@emph{Approximation of Conflict}
@itemlist[
@item{
Any variable @emph{defined} during a non-move instruction is in conflict with
every variable (except itself) in the @tech{undead-out set} associated with the
instruction.
}
@item{
Any variable @emph{defined} during a move instruction is in conflict with
every variable in the @tech{undead-out set} associated with the instruction,
except itself and the variable referenced in the move.
}
]

To implement conflict analysis, we design the language @deftech{Asm-lang
v2/conflicts} to capture the conflict graph.
We typeset the difference compared to @tech{Asm-lang v2/undead}.


@bettergrammar*-diff[#:include (info) asm-lang-v2/undead asm-lang-v2/conflicts]

The @asm-lang-v2/conflicts[info] field extended with a @tech{conflict graph},
represented as an association list from a variable to @tech{undead-out sets}.

As in @tech{Asm-lang v2/undead}, the @asm-lang-v2/conflicts[info] field also
contains a declaration of the @ch2-tech{abstract locations} that may be used in the
program, and (as usual) possibly other non-required but useful information.

To implement conflict analysis, we simultaneously traverse an @tech{instruction
sequence} with its @tech{undead-out set}, and analysis each instruction
according to the approxiate conflict definition above.
We start with an graph that initially contains a node for every
@ch2-tech{abstract location} in the @asm-lang-v2/conflicts[locals] set, and
extend the graph with conflicts as we discover them.

@nested[#:style 'inset]{
@defproc[(conflict-analysis [p asm-lang-v2/undead?]) asm-lang-v2/conflicts?]{
Decorates a program with its @tech{conflict graph}.

@examples[#:eval sb
(conflict-analysis
 '(module ((locals (x.1))
           (undead-out ((x.1) ())))
    (begin
      (set! x.1 42)
      (halt x.1))))

#;(conflict-analysis
 '(module ((locals (a.1 b.2 c.3 d.4 e.5))
           (undead-out ((a.1) (c.3 a.1) (c.3 a.1) (b.2 c.3) (d.4) ())))
    (begin
      (set! a.1 1)
      (set! c.3 2)
      (set! b.2 a.1)
      (set! b.2 (+ a.1 c.3))
      (set! d.4 (* b.2 c.3))
      (halt d.4))))


(conflict-analysis
 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
           (undead-out
            ((v.1)
             (v.1 w.2)
             (w.2 x.3)
             (p.1 w.2 x.3)
             (w.2 x.3)
             (y.4 w.2 x.3)
             (p.1 y.4 w.2 x.3)
             (y.4 w.2 x.3)
             (z.5 y.4 w.2)
             (z.5 y.4)
             (t.6 z.5)
             (t.6 z.5 p.1)
             (t.6 z.5)
             (z.5)
             ())))
    (begin
      (set! v.1 1)
      (set! w.2 46)
      (set! x.3 v.1)
      (set! p.1 7)
      (set! x.3 (+ x.3 p.1))
      (set! y.4 x.3)
      (set! p.1 4)
      (set! y.4 (+ y.4 p.1))
      (set! z.5 x.3)
      (set! z.5 (+ z.5 w.2))
      (set! t.6 y.4)
      (set! p.1 -1)
      (set! t.6 (* t.6 p.1))
      (set! z.5 (+ z.5 t.6))
      (halt z.5))))
]}}

@section{Register Allocation}
Register allocation, as in the, the step the actually assigns @ch2-tech{abstract
locations} to @ch2-tech{physical locations}, takes the set of @ch2-tech{abstract
locations} to assign homes, the conflict graph, and some set of assignable
registers, and tries to assign the most @ch2-tech{abstract locations} to
registers.
As usual, Rice's Theorem tells us we'll never be able to decide the maximal
number of variables we can fit in register.
We'll have to approximate.

@define[variable @ch2-tech{abstract location}]
@define[variables @ch2-tech{abstract locations}]

We'll use graph-colouring register allocation, an algorithm that is quadratic
(and slows compile time down quite a lot), but usually assigns more @variables
to registers than other faster algorithms.
@margin-note{It's worth noting that since this core pass is quadratic, compile
time is dominated by this single pass.
One might be tempted to try to fuse many of our small compiler passes to save
compile time, but most of our compile passes are linear, and have essentially no
effect on compile time compared to graph-colouring register allocation.
}

The core algorithm has a straight-forward recursive description.
We recur over the set of @asm-lang-v2/conflicts[locals] and producing an
@emph{assignment}, @ie a dictionary mapping @ch2-tech{abstract locations} to
@ch2-tech{physical locations}.
@itemlist[
@item{If the set of @ch2-tech{abstract locations} is empty, return the empty
assignment.}
@item{Otherwise, choose a @tech{low-degree} @variable from the input set of
@|variables|, if one exists.
Otherwise, pick an arbtrary @ch2-tech{abstract locations} from the set.

A @deftech{low-degree} @ch2-tech{abstract locations} is one with fewer than
@tt{k} conflicts, some for pre-defined @tt{k}.
We pick @tt{k} to be the number of registers in the set of assignable registers.
}
@item{Recur with the chosen @ch2-tech{abstract locations} removed from the input set and removed from
the conflict graph.
The recursive call should return an assignment for all the remaining @|variables|.}
@item{Attempt to select a register for the chosen @|variable|.
You cannot select registers to which conflicting @variables were assigned by the
recursive call.
This attempt succeeds if a low-degree @variable was chosen, and @emph{might} fail
otherwise (but it depends on which registers got allocated in the recursive
call).
@itemlist[
@item{If you succeed in select a register, then add the assignment for
the chosen @variable to the result of the recursive call.}
@item{Otherwise, we cannot assign the choosen @variable to a register.
Instead, we @emph{spill it}, @ie we assign it a fresh @ch2-tech{frame variable}.}
]}
]
@margin-note{
This algorithm is due to R. Kent Dybvig, itself an adaptation of the optimistic
register allocation described in "Improvements to graph coloring register
allocation" (ACM TOPLAS 6:3, 1994) by Preston Briggs, et al.
}

We can simplify the implementation of this algorithm by separting it into two
parts: first, sort all @variables in degree order, then assign each register in
sorted order.

To describe the output of the regsiter allocator, we reuse @ch2-tech{Asm-lang
v2/assignments}.
Below, we typeset the changes compared to @tech{Asm-lang v2/conflicts}.
Note only the @asm-lang-v2/assignments{info} field changes.

@bettergrammar*-diff[#:include (info) asm-lang-v2/conflicts asm-lang-v2/assignments]

@nested[#:style 'inset]{
@defproc[(assign-registers [p asm-lang-v2/conflicts]) asm-lang-v2/assignments?]{
Performs @tech{graph-colouring register allocation}.
The pass attempts to fit each of the @ch2-tech{abstract location} declared in
the locals set into a register, and if one cannot be found, assigns it a
@ch2-tech{frame variable} instead.

@examples[#:eval sb
(assign-registers
 '(module ((locals (x.1))
           (conflicts ((x.1 ()))))
   (begin
     (set! x.1 42)
     (halt x.1))))

(parameterize ([current-assignable-registers '(r9)])
  (assign-registers
   '(module ((locals (x.1))
             (conflicts ((x.1 ()))))
     (begin
       (set! x.1 42)
       (halt x.1)))))

(parameterize ([current-assignable-registers '()])
  (assign-registers
   '(module ((locals (x.1))
             (conflicts ((x.1 ()))))
     (begin
       (set! x.1 42)
       (halt x.1)))))

#;(assign-registers
 '(module ((locals (a.1 b.2 c.3 d.4 e.5))
           (conflicts ((a.1 (b.2 c.3 d.4 e.5))
                       (b.2 (a.1 c.3 e.5))
                       (c.3 (a.1 b.2 e.5))
                       (d.4 (a.1 e.5))
                       (e.5 (a.1 b.2 c.3 d.4)))))
   (begin
    (set! a.1 1)
    (set! c.3 2)
    (set! b.2 a.1)
    (set! b.2 (+ a.1 c.3))
    (set! d.4 (* b.2 c.3))
    (halt d.4))))

#;(parameterize ([current-assignable-registers '(rbx rcx r9 r10)])
  (assign-registers
   '(module ((locals (a.1 b.2 c.3 d.4 e.5))
             (conflicts ((a.1 (b.2 c.3 d.4 e.5))
                         (b.2 (a.1 c.3 e.5))
                         (c.3 (a.1 b.2 e.5))
                         (d.4 (a.1 e.5))
                         (e.5 (a.1 b.2 c.3 d.4)))))
     (begin
       (set! a.1 1)
       (set! c.3 2)
       (set! b.2 a.1)
       (set! b.2 (+ a.1 c.3))
       (set! d.4 (* b.2 c.3))
       (halt d.4)))))

(assign-registers
 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
           (conflicts
            ((x.3 (z.5 p.1 y.4 v.1 w.2))
             (w.2 (z.5 p.1 y.4 v.1 x.3))
             (v.1 (w.2 x.3))
             (y.4 (t.6 z.5 p.1 w.2 x.3))
             (p.1 (t.6 z.5 y.4 w.2 x.3))
             (z.5 (t.6 p.1 y.4 w.2 x.3))
             (t.6 (z.5 p.1 y.4)))))
    (begin
      (set! v.1 1)
      (set! w.2 46)
      (set! x.3 v.1)
      (set! p.1 7)
      (set! x.3 (+ x.3 p.1))
      (set! y.4 x.3)
      (set! p.1 4)
      (set! y.4 (+ y.4 p.1))
      (set! z.5 x.3)
      (set! z.5 (+ z.5 w.2))
      (set! t.6 y.4)
      (set! p.1 -1)
      (set! t.6 (* t.6 p.1))
      (set! z.5 (+ z.5 t.6))
      (halt z.5))))
]}}


@section{Appendix: Compiler Overview}

@figure["fig:reg-alloc-graph" "Overview of Register Allocation Optimization" reg-alloc-graph]

@section{Appendix: Language Definitions}

@declare-exporting[cpsc411/langs/v2-reg-alloc]

@deflangs[
asm-lang-v2
asm-lang-v2/locals
asm-lang-v2/undead
asm-lang-v2/conflicts
asm-lang-v2/assignments
nested-asm-lang-v2
]
