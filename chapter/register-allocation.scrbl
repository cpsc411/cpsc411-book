#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label
    (except-in cpsc411/reference/a3-solution uncover-locals replace-locations assign-homes))
  (for-label
    (only-in cpsc411/reference/a2-solution uncover-locals replace-locations assign-homes))
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v1
  cpsc411/langs/v2
  (for-label cpsc411/langs/v2)
  (for-label cpsc411/langs/v2-reg-alloc)
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

L62 [label="Nested-asm-lang v2"];

/* Register allocator */

subgraph DoNotcluster0 {
  graph [labeljust=right,
    style=filled,
    color=lightgrey,
    fontname="Courier",
    fontsize=12,
    label = "assign-homes-opt";
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

L4 -> L62 [label=" assign-homes-opt"];
}
}
]

@title[#:tag "top" #:tag-prefix "chp-reg-alloc:"]{Register Allocation}

@section{Preface: What's wrong with our language?}
With @ch2-tech{Asm-lang v2}, we introduced @ch2-tech{abstract locations} to free
the programmer from thinking about @ch2-tech{physical locations}.
Unfortunately, our implementation strategy has a severe limitation.
While it's simple and works, generates extremely slow code!
Each and every variable assignment or reference accesses memory.
While memory accesses have improved a lot compared to old computers due to
caching, accessing memory are still orders of magnitude slower than accessing a
register when our variable is not in the cache (and, in general, it won't be in
cache).
Our compiler will have better performance if we help the machine out by using
registers as much as possible.

Assigning @ch2-tech{abstract locations} to registers automatically is a
non-trivial task.
We started out by compiling to @ch2-tech{frame variables} because there are
infinitely many @ch2-tech{frame variables}, but only 16 registers.
Actually, fewer than 16, since the compiler and run-time system reserve some of
those for various purposes.
To assign an @ch2-tech{abstract location} a new @ch2-tech{frame variable} is
trivial---just pick a new one, there's always new one, just like
@ch2-tech{abstract locations}.
This one-to-one mapping between abstractions means assigment is a simple,
systematic algorithm.
To assign an @ch2-tech{abstract location} to a register, however, we need to
pick a register that isn't currently in use, and figuring out which registers
aren't in use requires a clever program analysis.

In general, being clever should be a last resort.

@section{Optimizing Location Assignment}
Conceptually, register allocation is a simple idea.
@itemlist[
@item{Undead analysis: figure out which @ch2-tech{abstract locations}
might still be needed after each instruction.
}
@item{Conflict analysis: figure out which @ch2-tech{abstract locations}
cannot be assigned to the same @ch2-tech{physical location} because they
both contain values that are needed at the same time.
}
@item{Register allocation: assign each @ch2-tech{abstract locations} to a
register that is different from any conflicting @ch2-tech{abstract locations}.
}
@item{Spilling: if we fail to find a register for an @ch2-tech{abstract
location}, put it in a @ch2-tech{frame variable} instead.
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

@bettergrammar*-ndiff[
#:labels ("Diff" "Asm-lang v2/assignments" "Asm-lang v2/locals")
(asm-lang-v2/locals asm-lang-v2/assignments)
(asm-lang-v2/assignments)
(asm-lang-v2/locals)
]

After this, our existing pass @racket[replace-locations] should handle replacing
@ch2-tech{abstract locations} by the assigned @ch2-tech{physical locations}.

Any compiler optimization should be seen as a transformation between programs in
the @emph{same language}, @ie, an @emph{intra-language transformation}.
The point is not to introduce a new layer of abstraction, but to improve some
performance metric of a program in some language.
In this case, the optimization happens in @ch2-tech{Asm-lang v2/assignments}; all
the programs we generate are equivalent to a program that only uses
@ch2-tech{frame variable} in its @asm-lang-v2/assignments[assigments] field.
In this case, performing the optimization requires additional steps of
analysis, so we introduce several administrative languages prior to
@ch2-tech{Asm-lang v2/assignments}.
@todo{Make this actually intra-language.}

To emphasize that this is an optimizatiom, we design @racket[assign-homes-opt]
as a drop-in replacement for @racket[assign-homes].

@nested[#:style 'inset]{
@defproc[(assign-homes-opt [p asm-lang-v2?]) nested-asm-lang-v2?]{
Compiles @ch2-tech{Asm-lang v2} to @ch2-tech{Nested-asm-lang v2}, replacing each
@ch2-tech{abstract location} with a @ch2-tech{physical location}.
This version performs graph-colouring register allocation.
}
}

This pass is still a composition of several passes, which we design next.

@section{Undeadness Analysis}
We begin the register allocation by figuring out which locations might still
be needed after each instruction.
This process is often called liveness analysis, but we refer to it as undeadness
analysis for reasons we explain shortly.

A @deftech{variable} (either @ch2-tech{abstract location} or @ch2-tech{physical
location}) with a @emph{particular} value that @emph{definitely} will be used is
considered @deftech{live}, and any @tech{variable} (with a particular value)
that @emph{definitely} won't be used is considered @deftech{dead}.

Recall that due to Rice's Theorem, we know it's generally impossible to decide
whether a @tech{variable} is @tech{dead} or @tech[#:key "live"]{alive}.
This means that when writing an analysis, we must assume partial knowledge.
The result is that we ignore @tech{live}ness entirely, which is hard to prove,
and focus on @tech{dead}ness, which is easy to prove.

@todo{Want to make clear the variable-at-a-point-in-time abstraction is what
we're analyzing, not merely variable.
This is why SSA is valuable; makes variables = variables-at-point-in-time.}
@margin-note{
@ch2-tech{Asm-lang v2/locals} is a simple enough language that we can tell
whether a @tech{variable} is @tech{dead} or @tech[#:key "live"]{alive}.
Later, when we add new instructions, we will modify the @tech{undead} analysis
and find @tech{variables} that aren't necessarily @tech{live} or @tech{dead},
and must be assumed to be @tech{undead}.
This is also necessary if we want to handle linking, or separate compilation.
}

We cannot in general decide the value of a @tech{variable} at a given
instruction.
Instead, we focus on analyzing each @emph{assignment} to a @tech{variable},
which might change the value of the @tech{variable}.

We assume that any @tech{variable} that gets used, or might get used, might be
@emph{not} @tech{dead}, @ie we assume it is @deftech{undead}, and consider a
@tech{variable} @tech{dead} only when we have conclusive proof---like witnessing
an instruction driving a new value through its heart, er, storing a new value in
that @tech{variable}.

We collect the @tech{undead} @tech{variables} into sets.
The @deftech{undead-out set} of an instruction is a set of @tech{variables} that
are @tech{undead} after executing that instruction.
@margin-note{
Most compilers call these live-out or live-after sets.
This suggests that @tech{variables} are definitely alive, and that the analysis
is computing sets of @tech{variables} that are definitely alive, neither of
which is true.
@tech{Undead} is not exactly the same as not-definitely-dead, but it's more
suggestive of reality for compilers.
}

@todo{This is but one algorithm. Should rewrite to make that clear and cite algorithm}
We design an algorithm for computing the @tech{undead-out sets} by taking a
single linear pass over the program.
Our algorithm is fast, but slightly counter-intuitive to implement.

To compute the @tech{undead-out sets} for each instruction, we analyze the
program by looping over the instruction sequence backwards, starting with the
last statement.
The loop takes an instruction and its @tech{undead-out set} as input.
We analyze the instruction with its @tech{undead-out set} and compute an
@deftech{undead-in set} for the instruction, which is the same as the
@tech{undead-out set} of the preceding instruction in the program.
That is, the @tech{undead-in set} for an instruction @object-code{s_i} is the
@tech{undead-out set} for the instruction @object-code{s_{i-1}} in the program.

To start the loop, this algorithm requires a default @tech{undead-out set} for
the last instruction in the scope of our analysis.
The default @tech{undead-out set} for @ch2-tech{Asm-lang v2/locals} is the empty
set---no variables are @tech[#:key "live"]{alive} after the end of the program,
are we are analyzing the entire program at once.
In general, the default set may not be empty, depending on the language and the
scope of the analysis.
For example, in @tech[#:tag-prefixes '("book:" "chp-boilerplate:")]{Paren-x64
v1}, we assume @paren-x64-v1[rax] after the program finishes.
When we introduce functions, we will assume that return value locations are live
at the end of functions.
If we had global variables, we might need to assume they are live at the end of
a program.

Each iteration of the loop performs the following analysis on a particular
instruction.
We start by assuming the @tech{undead-in set} is the same as the
@tech{undead-out set}, then update it depending on what happens in the
instruction.
If a @tech{variable} is @emph{assigned}, @ie its value is overwritten in the
instruction, it is @emph{definitely} @tech{dead} upon entry to this intruction,
so we remove it from the @tech{undead-in set}.
If a @tech{variable} is @emph{referenced} in the instruction, it @emph{ought to
be} @tech{live}---we don't actually know, since that instruction's result itself
might not be used, or the instruction itself might never be executed, but the
@tech{variable} is at least acting like it's @tech{live}---and is added to the
@tech{undead-in set}.

This algorithm creates the @tech{undead-out sets} for each instruction so that
later passes can associate each instruction with its @tech{undead-out set}.
There are many ways to associate the @tech{undead-out sets} with instructions.
We choose a representation that implicitly associates each set with an
instruction.
Since our programs are trees of instructions, we collect the @tech{undead-out
sets} for each instruction into a tree of @tech{undead-out sets}, whose
tree structure mirrors the program's tree structure.
We define the data @tech{undead-set tree} to mirror the structure of
@ch2-tech{Asm-lang v2} programs.

An @deftech{undead-set tree} is one of:
@itemlist[
@item{an @tech{undead-out set} @asm-lang-v2[(aloc ...)], corresponding to the
@tech{undead-out set} for a single instruction such as @asm-lang-v2[(halt
triv)] or @asm-lang-v2[(set! aloc triv)].}
@item{a list of @tech{undead-set tree}s, @asm-lang-v2[(undead-set-tree?_1 ...
undead-set-tree?_2)], corresponding to a @asm-lang-v2[begin] statement
@asm-lang-v2[(begin effect_1 ... effect_2)] or @asm-lang-v2[(begin effect_1 ...
tail)].
The first element of the list represents @tech{undead-set tree} for the first
@asm-lang-v2[effect], the second element represents the @tech{undead-set tree}
for the second @asm-lang-v2[effect], and so on.
}
]
We always traverse an @tech{undead-set tree} together with a corresponding
program; the @tech{undead-set tree} cannot be interpreted in isolation.
For example, given a list of @tech{undead-set tree}s @racket[undead-outs] and a
list of effects @racket[ss], we would traverse with the following template:
@racketblock[
(define (fn-for-s-and-undead-outs ss undead-outs)
  (match (cons ss undead-outs)
    [(cons '() '())
     (... case-for-empty ...)]
    [(cons `(,s ,rest-ss ...) `(,undead-out ,rest-undead-outs ...))
     (... (fn-for-s-and-undead-out s undead-out)
          (fn-for-ss-and-undead-outs rest-ss rest-undead-outs))]))
]

To describe the output of the analysis, we define a new @ch2-tech{administrative
language}.
We collect the @tech{undead-set tree} a new @asm-lang-v2/undead[info] field.
Below, we define @deftech{Asm-lang v2/undead}.
The only change compared to @ch2-tech{Asm-lang v2/locals} is in the
@asm-lang-v2/undead[info] field, so we typeset the difference in the
@asm-lang-v2/undead[info] field.

@bettergrammar*-ndiff[
#:labels ("Diff" "Asm-lang v2/undead" "Asm-lang v2/locals")
(asm-lang-v2/locals asm-lang-v2/undead)
(asm-lang-v2/undead)
(asm-lang-v2/locals)
]

@nested[#:style 'inset]{
@defproc[(undead-analysis [p asm-lang-v2/locals?]) asm-lang-v2/undead?]{
Performs undeadness analysis, decorating the program with @tech{undead-set
tree}.
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

We could use the undead information to design an optimization to remove dead
@tech{variables}.
However, to separate concerns, we should not do this during register allocation.
Instead, later, we'll design more general optimization pass that happens before
register allocation.

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

@emph{True Definition of Conflict:}
Any variable that gets a new value during an instruction is @deftech{in
conflict} with every variable that (1) has a different value at the same time
and (2) will still be used after that instruction.

Unfortunately, due to Rice's Theorem, we cannot decide either property.
We cannot figure out the value of every variable before run time; if we could,
compiling would not be necessary.
We also do not know which variables are @tech{live}, only which are @tech{undead}.
Therefore, we can only approximate conflicts.

To approximate conflicts, we ignore values, and once more focus on assignments
to variables.
An assignment to the variable means the it @emph{might} take on a
new value that @emph{might} be different from the value of any variable which
@emph{might} be @tech{live} at that point.
We have already approximated liveness via @tech{undead-out sets}, so what remains
is to approximate when a variable takes on a new value.
Below, we describe how to approximate conflcits and slightly refine these criteria.

We represent conflicts in a data structure called a @deftech{conflict graph}.
Interpreted as an undirected graph, the variables are represented as nodes (also
known as vertexes), and conflicts between variables are represented as an edge
from the variable to each of the variables in the associated set of conflicts.
If there is an edge between any two nodes, then they are in conflict.
Interpreted as a dictionary, the @tech{conflict graph} maps each variable to a
set of variables with which it is in conflict.

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
@ch2-tech{physical location} as any other variable in the @tech{undead-out set}.
If @asm-lang-v2[x.2] is @tech[#:key "undead-out set"]{undead-out} at this
instruction, and we try to put @asm-lang-v2[x.1] in the same @ch2-tech{physical
location} as @asm-lang-v2[x.2], then the value of @asm-lang-v2[x.2] would be
overwritten by the value of @asm-lang-v2[(+ x.1 x.2)].

We can reduce the number of conflicts, and thus possibly fit more variables into
registers, by observing that one instruction does tell us that two values will
be the same.
A move instruction, such as @object-code{(set! x.1 x.2)}, is an instruction that
simply defines the value of one variable to be that of another.

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

The @asm-lang-v2/conflicts[info] field is extended with a @tech{conflict graph},
represented as an association list from a variable to @tech{undead-out sets}.

As in @tech{Asm-lang v2/undead}, the @asm-lang-v2/conflicts[info] field also
contains a declaration of the @ch2-tech{abstract locations} that may be used in the
program, and (as usual) possibly other non-required but useful information.

To implement conflict analysis, we simultaneously traverse a program with its
@tech{undead-set tree}, and analyze each instruction according to the
approxiate conflict definition above.
We start with a graph that initially contains a node for every
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
Register allocation, as in the step that actually assigns @ch2-tech{abstract
locations} to @ch2-tech{physical locations}, takes the set of @ch2-tech{abstract
locations} to assign homes, the conflict graph, and some set of assignable
registers, and tries to assign the most @ch2-tech{abstract locations} to
registers.
As usual, Rice's Theorem tells us we'll never be able to decide the maximal
number of variables we can fit in registers.
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
We recur over the set of @asm-lang-v2/conflicts[locals] and produce an
@emph{assignment}, @ie a dictionary mapping @ch2-tech{abstract locations} to
@ch2-tech{physical locations}.
@itemlist[
@item{If the set of @ch2-tech{abstract locations} is empty, return the empty
assignment.}
@item{Otherwise, choose a @tech{low-degree} @variable from the input set of
@|variables|, if one exists.
Otherwise, pick an arbitrary @ch2-tech{abstract location} from the set.

A @deftech{low-degree} @ch2-tech{abstract location} is one with fewer than
@tt{k} conflicts, for some for pre-defined @tt{k}.
We pick @tt{k} to be the number of registers in the set of assignable registers.
}
@item{Recur with the chosen @ch2-tech{abstract location} removed from the input set and
the conflict graph.
The recursive call should return an assignment for all the remaining @|variables|.}
@item{Attempt to select a register for the chosen @|variable|.
You cannot select registers to which conflicting @variables were assigned by the
recursive call.
This attempt succeeds if a low-degree @variable was chosen, and @emph{might} fail
otherwise (but it depends on which registers got allocated in the recursive
call).
@itemlist[
@item{If you succeed in selecting a register, then add the assignment for
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

To describe the output of the register allocator, we reuse @ch2-tech{Asm-lang
v2/assignments}.
Below, we typeset the changes compared to @tech{Asm-lang v2/conflicts}.
Note only the @asm-lang-v2/assignments[info] field changes.

@bettergrammar*-diff[#:include (info) asm-lang-v2/conflicts asm-lang-v2/assignments]

@nested[#:style 'inset]{
@defproc[(assign-registers [p asm-lang-v2/conflicts]) asm-lang-v2/assignments?]{
Performs graph-colouring register allocation.
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
