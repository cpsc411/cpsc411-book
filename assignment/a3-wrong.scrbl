#lang reader "assignment-lang.rkt"

@;todo{Reset automatically on new assignment.}
@(reset-exercise-counter!)
@(reset-challenge-counter!)

@(define eg
  (make-cached-eval "a3-wrong-eval"
  '(require cpsc411/v1-reference/a3-wrong-solution cpsc411/deprecated/a3-compiler-lib)
  '(current-stack-size 512)))

@title[#:tag "top" #:tag-prefix "a3:wrong:"]{Compiler 3: Register Allocation (Original)}

@section{Assignment Summary}

The goal of this assignment is to introduce (1) "optimizing" compilation (2)
register allocation, perhaps the most important "optimization".  In this
assignment, you will replace the @racket[assign-homes] pass with
@racket[assign-homes-opt], a version that tries hard to put variables into
registers instead of in the frame.

This assignment is due Friday January 31, 2020 at 11:59pm.

@subsubsub*section{Assignment Checklist}

You should find a new repository in your @url{https://github.students.cs.ubc.ca}
account named @tt{a3_<team ids>} with a code skeleton and a support library.
You should complete the assignment in that git repository and push it to the
GitHub Students instance.

You should first copy your solution to @secref[#:tag-prefixes '("a2:")]{top}
into the starter code provided.
You can start from the code base of any of your team members.

If we patch the code skeleton or library after the release, the most recent
version will be available here at @share{a3-skeleton.rkt} and
@share{a3-compiler-lib.rkt}, and the functional graph library
@share{a3-graph-lib.rkt}.
The name of the skeleton is @share{a3-skeleton.rkt} to avoid accidentally
overwriting your files, but your file in the Git repository should be named
@tt{a3.rkt}.

@itemlist[
@item{Design and implement @racket[assign-registers], a compiler from
@tech{Conflict-lang} to @a2-tech{Loc-assigned-lang}.}
@item{Design and implement @racket[conflict-analysis], a compiler from
@tech{Undead-loc-lang} to @tech{Conflict-lang}.}
@item{Design and implement @racket[undead-analysis], a compiler from
@a2-tech{Loc-locals-lang} to @tech{Undead-loc-lang}.}
@item{Design and implement @racket[assign-homes-opt], a compiler from
@a2-tech{Loc-locals-lang} to @a2-tech{Loc-assigned-lang}.}
@item{Write a paragraph comparing two versions of your compiler: one with
@racket[assign-homes] and one with @racket[assign-homes-opt].}
]

@;Related reading:
@;@itemlist[
@;@item{Chapter 3.2 "Essentials of Compilation"}
@;]

@;@todo{Could be better to delay register allocation until after they have at
@;least tail calls. OTOH, iterating might be a good software engineering point.
@;Regardless, it *ought* to be possible to move this assignment around pretty easily...}


@section{Language Diagram}

@dot->svg{
digraph {

   node [ shape="box" ]

   /* The Languages */

   L0 [label="Loc-locals-lang"];


/* WARNING: the subgraph name "clusterN" is meaningful in GraphViz!  */
/* to turn on clustering, switch subgraph name by switching comments */
subgraph DoNotcluster0 {
      graph [labeljust=right,
	     style=filled,
	     color=lightgrey,
	     fontname="Courier",
	     fontsize=10,
             label = "assign-homes-opt";
      ];
      edge [fontname="Courier"]

      L1 [label="Undead-loc-lang"];
      L2 [label="Conflict-lang"];


   }

   L3 [label="Loc-assigned-lang"];

   /* The Passes */

   edge [fontname="Courier"]

   L0 -> L1 [label=" undead-analysis"];

   L1 -> L2 [label=" conflict-analysis"];

   L2 -> L3 [label=" assign-registers"];

   /* If clustering is turned on, comment out the next edge */
   L0 -> L3 [label=" assign-homes-opt"];
}
}

@section{Register Allocation}

To assign abstract locations (variables) to physical locations efficiently, we
need to know when any two variables are @emph{in conflict}, @emph{i.e.}, cannot
be assigned to the same physical location because they might both be used at the
same time.

We start by designing a language, @deftech{Conflict-lang}, in which we assume we
already know all the variables that are in use and which other variables they
are in conflict with.  In @tech{Conflict-lang}, the program is annotated with:
(1) a list of local variables declarations, like in @a2-tech{Loc-locals-lang},
and (2) a @tech{conflict graph}, represented as an association list from a
variable to the set of variables with which it conflicts.  The language is
essentially the same as @deftech{Loc-locals-lang} except for the addition of the
conflict graph to the info field.

@racketgrammar*[
  [p     (begin info s ...)]
  [info  ((locals (aloc ...))
          (conflicts ((aloc (aloc ...)) ...))
          any ...)]
  [s     (set! aloc int64)
         (set! aloc aloc)
         (set! aloc (binop aloc aloc))
         (halt aloc)]
  [binop * +]
]

The @object-code{(conflicts ((aloc (aloc ...)) ...))} form in the info field
represents a @deftech{conflict graph}.  Interpreted as an undirected graph, the
variables represent nodes and there is an edge from each initial variable to
each of the variables in the associated set.  If there is an edge between any
two nodes, then they are deemed to be in conflict.  Interpreted as a
dictionary, this form maps each variable to a set of its conflicts.

As in the previous assignment, the info field also contains a declaration of the
@object-code{(locals (aloc ...))} abstract locations that may be used in the
program, and (as usual) possibly other non-required but useful information.

The register assignment algorithm takes a @tech{Conflict-lang} program and
produces a @a2-tech{Loc-assigned-lang} program, annotating the program with an
assignment of variables to physical locations in the info field.

The core algorithm has a straight-forward recursive description, recurring over
the list of @object-code{locals} and producing an @emph{assignment}, i.e., an
association list mapping variables to physical locations.
@itemlist[
@item{If the list of variables is empty, return the empty assignment.}
@item{Choose a low-degree variable from the input list of variables, if one
exists. Otherwise, pick an arbtrary variable (such as the first one in the
list).

A low-degree variable is one with fewer than @object-code{k}
conflicts.
We pick @object-code{k} to be the number of registers available for use during
register allocation.}
@item{Recur with the chosen variable removed from the input list and the
conflict graph.
The recursive call should return an assignment for all the remaining variables.}
@item{Attempt to select a register for the chosen variable. You cannot select
registers to which conflicting variables were assigned by the recursive call.
This attempt succeeds if a low-degree variable was chosen, and
@emph{might} fail otherwise (but it depends on which registers got allocated in
the recursive call).
@itemlist[
@item{If you succeed in assigning a register, then add the assignment for
the chosen variable to the result of the recursive call.}
@item{Otherwise, we cannot assign the choosen variable to a register.
Instead, we @emph{spill it}, i.e., we allocate a fresh frame location using a
displacement mode operand.}
]}
]
This algorithm is an adaptation of the optimistic register allocation described
in "Improvements to graph coloring register allocation" (ACM TOPLAS 6:3, 1994)
by Preston Briggs, et al.

@exercise{Design and implement the @racket[parameter?]
@racket[current-assignable-registers], which stores the list of @object-code{k}
registers available to the register allocator.

The default should be all registers except the following.
You may not use the register @object-code{rax}, since it is used to patch
instructions, or @object-code{rbp}, since it contains the frame pointer.
}

@exercise{
Design and implement the function @racket[assign-registers], which performs
graph-coloring register allocation.
The source language is @tech{Conflict-lang} and the target language is
@a2-tech{Loc-assigned-lang}.
The pass should attempt to fit each of the variables declared in
@object-code{(locals (aloc ...))} into a free register, and if one cannot be
found, should allocate the variable a fresh frame location.

We provide a functional graph library, @share{a3-graph-lib.rkt}, for working
with the conflict graph.

This algorithm is somewhat tedious to write purely functionally.
You may use a single mutable variable in your implementation.
Use @racket[box], @racket[unbox], and @racket[set-box!] for an idomatic way of
using mutable variables in Racket.

It will be difficult to keep enough variables live at one time to test spilling.
You should use @racket[parameterize] and @racket[current-assignable-registers] to
test spilling.
}

Below we give some examples.
However, your register allocator may produce different assignments for the same
program and still be correct.
This is because the register allocation algorithm makes arbitrary choices in
some places.
@emph{Do not overfit to these examples.}
@examples[#:eval eg
(assign-registers
 '(begin ((locals (x.1))
          (conflicts ((x.1 ()))))
    (set! x.1 42)
    (halt x.1)))

(parameterize ([current-assignable-registers '(r9)])
  (assign-registers
   '(begin ((locals (x.1))
            (conflicts ((x.1 ()))))
           (set! x.1 42)
           (halt x.1))))

(parameterize ([current-assignable-registers '()])
  (assign-registers
   '(begin ((locals (x.1))
            (conflicts ((x.1 ()))))
      (set! x.1 42)
      (halt x.1))))

(assign-registers
 '(begin ((locals (a.1 b.2 c.3 d.4 e.5))
          (conflicts ((a.1 (b.2 c.3 d.4 e.5))
                      (b.2 (a.1 c.3 e.5))
                      (c.3 (a.1 b.2 e.5))
                      (d.4 (a.1 e.5))
                      (e.5 (a.1 b.2 c.3 d.4)))))
   (set! a.1 1)
   (set! c.3 2)
   (set! b.2 a.1)
   (set! b.2 (+ a.1 c.3))
   (set! d.4 (* b.2 c.3))
   (halt d.4)))

(parameterize ([current-assignable-registers '(rbx rcx r9 r10)])
  (assign-registers
   '(begin ((locals (a.1 b.2 c.3 d.4 e.5))
            (conflicts ((a.1 (b.2 c.3 d.4 e.5))
                        (b.2 (a.1 c.3 e.5))
                        (c.3 (a.1 b.2 e.5))
                        (d.4 (a.1 e.5))
                        (e.5 (a.1 b.2 c.3 d.4)))))
      (set! a.1 1)
      (set! c.3 2)
      (set! b.2 a.1)
      (set! b.2 (+ a.1 c.3))
      (set! d.4 (* b.2 c.3))
      (halt d.4))))

(assign-registers
 '(begin ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
          (conflicts
           ((x.3 (z.5 p.1 y.4 v.1 w.2))
            (w.2 (z.5 p.1 y.4 v.1 x.3))
            (v.1 (w.2 x.3))
            (y.4 (t.6 z.5 p.1 w.2 x.3))
            (p.1 (t.6 z.5 y.4 w.2 x.3))
            (z.5 (t.6 p.1 y.4 w.2 x.3))
            (t.6 (z.5 p.1 y.4)))))
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
     (halt z.5)))
]

When debugging your register allocator, you might try comparing your allocator
to the graph coloring algorithm provided by @other-doc['(lib
"graph/scribblings/graph.scrbl")].
This provides an implementation of graph coloring, although it does not perform
spilling so it will not work in general.
You cannot use this library in the implementation of your compiler; it may only
be used for testing.
Unfortunately, it uses a different representation of graphs, so you will need to
write conversion procedures.

You may also choose to preserve additional info fields in the output of
@racket[assign-registers].
We define @deftech{Loc-assigned-lang v2} below, which assumes that you preserve
the @racket[conflicts] and @racket[locals] info fields.
Note that @tech{Loc-assigned-lang v2} also meets the specification for
@a2-tech{Loc-assigned-lang}, so every @tech{Loc-assigned-lang v2} program is a
@a2-tech{Loc-assigned-lang} program.
@racketgrammar*[
[p     (begin info s ... (halt aloc))]
[info  ((assignment ((aloc rloc) ...))
        (locals (aloc ...))
        (conflicts ((aloc (aloc ...)) ...))
        any ...)]
[s (code:comment "Same as Loc-assigned-lang")]
]

If you do, you can use @racket[check-assignment] in your compiler pipeline to
detect bugs in the register allocator.
@racket[check-assignment] expects a @tech{Loc-assigned-lang v2} programs and
returns a @a2-tech{Loc-assigned-lang} program, or an error if a bad assignment
is detected.
@examples[#:eval eg
(eval:error
 (check-assignment
 '(begin ((locals (x.1 y.1))
          (conflicts ((x.1 (y.1)) (y.1 (x.1))))
          (assignment ((x.1 r9) (y.1 r9))))
         (set! x.1 42)
         (set! y.1 42)
         (set! x.1 (+ x.1 y.1))
         (halt x.1))))

(eval:error
 (check-assignment
  '(begin ((locals (x.1 y.1))
           (conflicts ((x.1 (y.1)) (y.1 (x.1))))
           (assignment ((x.1 r9))))
          (set! x.1 42)
          (set! y.1 42)
          (set! x.1 (+ x.1 y.1))
          (halt x.1))))
]

@;todo{Use "variables" in a2 for abstract locations.}

@section{Conflict Analysis}
We have just assumed that we know which variables are in conflict, so now we
need to discharge that assumption and build conflict graphs.

To figure out when two variables are in conflict, we need to know which
variables @emph{might} be in-use on entry to an instruction.
We therefore make an assumption that the input program is decorated with this
information, called the @tech{undead-in sets}.
The @deftech{undead-in sets} are a list of sets of variables @racket[(undead
((aloc ...) ...))], with each set associated with an instruction in the program.
Each undead set represents, on entry to the associated instruction, which
variables are @tech{undead}---variable that might not technically be alive, but
aren't conclusively proven to be dead.
The first element of the list represents the set of variables that are undead
when the first instruction begins executing, the second element represents the
set of undead variables when the second instruction begins executing, and so on.
Any variable that is referenced during an instruction is in conflict with every
variable in the @tech{undead-in set} associated with the instruction.

We design @deftech{Undead-loc-lang} to contain the undead-in sets for each
instruction in a program.
The language is essentially the same as @tech{Conflict-lang} except for the
info field.

@racketgrammar*[
  [p     (begin info s ...)]
  [info  ((locals (aloc ...))
          (undead ((aloc ...) ...))
          any ...)]
  [s     (set! aloc int64)
         (set! aloc aloc)
         (set! aloc (binop aloc aloc))
         (halt aloc)]
  [binop * +]
]

@exercise{
Design and implement the function @racket[conflict-analysis], which
decorates a program with its @tech{conflict graph}.
The source language is @tech{Undead-loc-lang} and the target language is
@tech{Conflict-lang} program.

For working with sets, you may want to use
@secref["sets" #:doc '(lib "scribblings/reference/reference.scrbl")].
}

@examples[#:eval eg
(conflict-analysis
 '(begin ((locals (x.1))
          (undead ((x.1) ())))
    (set! x.1 42)
    (halt x.1)))

(conflict-analysis
 '(begin ((locals (a.1 b.2 c.3 d.4 e.5))
          (undead ((a.1) (c.3 a.1) (c.3 a.1) (b.2 c.3) (d.4) ())))
         (set! a.1 1)
    (set! c.3 2)
    (set! b.2 a.1)
    (set! b.2 (+ a.1 c.3))
    (set! d.4 (* b.2 c.3))
    (halt d.4)))


(conflict-analysis
 '(begin ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
          (undead
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
         (halt z.5)))

]

@section{Undead Analysis}
Again we have made an assumption so again we must discharge it.
We must analysis a program to determine which variables are @tech{undead}.
The source language is the same @a2-tech{Loc-locals-lang} from last week.

Any (assignment to a) variable that @emph{definitely} will be used is considered
@deftech{live}, and (assignment to a) variable that @emph{definitely} won't be
used is considered @deftech{dead}.
Due to Rice's theorem,
a huge buzz kill, we know it's generally impossible to tell whether
a variable is @tech{dead} or @tech[#:key "live"]{alive}.
This means that when writing an analysis, we must assume partial knowledge.
We ignore @tech{live}ness entirely.
Instead, we assume that any variable that gets used, or might get used, might
@emph{not} be @tech{dead}, that is, we assume it is @deftech{undead}, and
consider a variable @tech{dead} only when we have conclusive proof---like
witnessing an instruction driving a new value through its heart.
@margin-note{
Most compilers and textbooks call our @tech{undead-in sets} "live-in sets" or
"live-before sets".
This suggests that variables are definitely alive, and that the analysis is
computing sets of variables that are definitely alive, neither of which is true.
@tech{Undead} is not exactly the same as not-definitely-dead, except in horror
movies and video games, but it's more suggestive of reality for compilers.
}

To determine whether a variable is in the @tech{undead-in set}, we analyze the
program by looping over the instruction sequence backwards, starting with the
last statement.
The main part of the analysis takes an instruction and its @deftech{undead-out
set}, the set of variables undead after that instruction executes.
To start the process, we need a default @tech{undead-out set} for the last
instruction; the default @tech{undead-out set} for @tech{Loc-locals-lang} is empty.
We analyze each instruction with its @tech{undead-out set} and compute an
@tech{undead-in set} for the current instruction, which is the same as the
@tech{undead-out set} of the preceding instruction in the instruction sequence.
That is, the @tech{undead-in set} for an instruction @object-code{s_i} is the
@tech{undead-out set} for the instruction @object-code{s_{i-1}} in the program.
We use the output, the @tech{undead-in set}, for the info field, and use it
as the input for analyzing the previous instruction in the sequence in the next
iteration of the loop.

The analysis on a particular instruction knows which variables are
@tech{undead} after that instruction, and computes which are @tech{undead}
before executing the instruction.
If a variable is used in the instruction, it @emph{ought to be} @tech{live} (but
we don't actually know) and is added to the @tech{undead-in set}---@tech{live}
is a subset of @tech{undead}.
If a variable is overwritten in the instruction, it is @tech{dead} at that
point and removed from the @tech{undead-in set}.

@a2-tech{Loc-locals-lang} is a simple enough language that we can tell whether a
variable is @tech{dead} or @tech[#:key "live"]{alive}.
Later, when we add new instructions, we will modify the @tech{undead} analysis
and find variables that aren't necessarily @tech{live} or @tech{dead}, and must
be assumed to be @tech{undead}.

@exercise{
Design and implement the function @racket[undead-analysis], which decorates a
program with its list of @tech{undead-in sets}; only the info field of the program
is modified.
The source language is @a2-tech{Loc-locals-lang} and the target language is
@tech{Undead-loc-lang}.
}

@examples[#:eval eg
(undead-analysis
 '(begin ((locals (x.1)))
         (set! x.1 42)
         (halt x.1)))

(undead-analysis
 '(begin ((locals (a.1 b.2 c.3 d.4 e.5)))
    (set! a.1 1)
    (set! c.3 2)
    (set! b.2 a.1)
    (set! b.2 (+ a.1 c.3))
    (set! d.4 (* b.2 c.3))
    (halt d.4)))

(undead-analysis
 '(begin ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
     (halt z.5)))
]

An important corner case to consider is what happens when unused variables
appear in a program.
@examples[#:eval eg
(undead-analysis
 '(begin ((locals (x.1 y.1)))
    (set! y.1 42)
    (set! x.1 5)
    (halt x.1)))

(undead-analysis
 '(begin ((locals (x.1 y.1)))
    (set! x.1 5)
    (set! y.1 42)
    (halt x.1)))
]

You can assume that unused variable @emph{do not appear}.
In a realistic compiler, they should be removed be an optimization.
@challenge{
Design and implement the function @racket[bury-dead], which removes assignments
to unused abstract locations.
The source language is @a2-tech{Loc-locals-lang} and the target language is
@a2-tech{Loc-locals-lang}.
Optimizations often have the same source and target language.
}

@section{Optimizing @racket[assign-homes]}
We can now assign registers for all @a2-tech{Loc-locals-lang} programs from last
week.
By assigning variables to registers, @racket[patch-instructions] will be able to
produce much faster code because it will not need to move values to and from
memory on every single instruction.

@exercise{
Define @racket[assign-homes-opt], which can be used as a drop-in replacement for
@racket[assign-homes].
The interface is the same as @racket[assign-home]: the source language is
@tech{Loc-locals-lang} and the target language is @a2-tech{Loc-assigned-lang}.

Run your test suite and ensure you get the same outputs whether you use
@racket[assign-homes] or @racket[assign-homes-opt].
}

@examples[#:eval eg
(define p
  '(begin ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
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
     (halt z.5)))

(assign-homes-opt p)
(assign-homes p)

(define (loc-assigned-execute f)
  (lambda (p)
    (parameterize ([current-pass-list
                    (list
                     f
                     replace-locations
                     patch-instructions
                     check-paren-x64
                     generate-x64
                     wrap-x64-boilerplate
                     wrap-x64-run-time)])
      (execute p))))

(define loc-lang-execute-opt (loc-assigned-execute assign-homes-opt))
(define loc-lang-execute (loc-assigned-execute assign-homes))

(loc-lang-execute-opt p)
(loc-lang-execute p)
]

@exercise{Create two versions of your compiler by defining two functions,
@racket[compiler-a2] and @racket[compiler-a3].
The source language should be @a2-tech{Values-lang} and the target should be
@a0-tech{x64-linux}, represented as a string.
@racket[compiler-a2] should use @racket[assign-homes], while
@racket[compiler-a3] should use @racket[assign-home-opt] instead.
You should use @racket[parameterize] and @racket[current-pass-list].

Write a paragraph comparing the two compilers.
Consider the following questions:
@itemlist[
@item{What are benefits and the disadvantages of each?}
@item{Is one "optimal", and if so, in what sense?}
@item{Try running the same programs compiled with each compiler. Are there any
surprises?}
]
}

@;@todo{Do I want conflict graphs represented as part of a program? Not sure.
@;Always seemed a bit of a hack to me. OTOH, both Jeremy and Kent do it, and it
@;will be easier to decorate programs with live in sets once we add jumps...
@;
@;For now, always using the info-field style}
@;
@;@todo{This is a simpler register/frame allocation process than Kent's,
@;essentially the same as Jeremy and Ryan's; however, it's less realistic.}
