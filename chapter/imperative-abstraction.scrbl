#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a2-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v2
  (for-label cpsc411/langs/v2)
  cpsc411/langs/v3
  (except-in "../assignment/a1.scrbl" doc)
  (except-in "intro-abstraction.scrbl" doc))

@(provide
  (except-out (all-defined-out) sb))

@(define sb
  (make-cached-eval "ch3-eval"
   '(require cpsc411/reference/a2-solution cpsc411/compiler-lib cpsc411/2c-run-time)
   '(current-stack-size 512)))

@declare-exporting[cpsc411/reference/a2-solution]

@define[v3-graph
@dot->svg{
digraph {

node [ fontname="Courier", shape="box", fontsize=12 ]

/* The Languages */

L0 [label="Values-lang v3"];
L1 [label="Values-unique-lang v3"];
L2 [label="Imp-mf-lang v3"];
L3 [label="Imp-cmf-lang v3"];
L4 [label="Asm-lang v2"];

L62 [label="Nested-asm v3"];
L7 [label="Para-asm v2"];
L8 [label="Paren-x64-fvars v2"];
L9 [label="Paren-x64 v2"];
L10 [label="x64"];
L11 [label="integer"];

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
  L6 [label="Asm-lang v2/assignments"];
}

edge [fontname="Courier", fontsize=12, labeljust=left]

L4 -> L5 [label=" uncover-locals"];
L5 -> L6 [label=" assign-fvars"];
L6 -> L62 [label=" replace-locations"];

/* The Passes */

L0 -> L1 [label=" uniquify"];
L1 -> L2 [label=" sequentialize-let"];
L2 -> L3 [label=" normalize-bind"];
L3 -> L4 [label=" select-instructions"];
L4 -> L62 [label=" assign-homes"];
L62 -> L7 [label=" flatten-begins"];
L7 -> L8 [label=" patch-instructions"];
L8 -> L9 [label=" implement-fvars"];
L9 -> L10 [label=" generate-x64"];
L10 -> L11 [label=" execute"];
L9 -> L11 [label = "interp-paren-x64"];
}
}
]

@; ----- end of Language Defs ------


@title[#:tag "top" #:tag-prefix "chp3:"]{Imperative Abstractions}
@todo{Feel like I can make a joke or point about imperative and sequentiality.
I don't care when things happen, so long as they happen, in a value-oriented
language.}

@todo{Really, this chapter is about an introduction to composition. "Composition
abstractions?"}

@section{Preface: What's wrong with our language?}
In @ch2-tech{Asm-lang v2}, which abstracted away from machine-specific details
such as what @tech{physical locations} exist and machine-specific restrictions
on instructions.
This is often a goal of compiler implementation---we want a more portable
language to easily retarget to different machines.

However, the source language is still not something we want to program.
In order to program, we, as @ch2-tech{Asm-lang v2} programmers, must manually
keep track of where a value is located in order to perform computation on it.
We must move values into particular locations before computing.
For example, when doing arithmetic, we cannot simply write @racket[(+ 2 2)],
@ie "Add the values 2 and 2".
Instead, we must write "first move 2 into a location, then move 2 into a
different location, now add the contents of the two locations".

@todo{Do some example Paren-x64 v1 programming}

We want to move towards a @deftech{value-oriented} language, @ie a language
where operations consume and produce values directly, and away from
@deftech{imperative language} that manipulates some underlying machine state
that the programmer must always keep in mind and manually manipulate.
This would free the programmer from keeping the state of the machine in mind at
all times and manually manipulating it to perform computation.

To this end we design a new source language, @tech{Values-lang v3}.
Implementing this language is the goal for this chapter.
The language is designed primarily to address the above limitations.

@bettergrammar*[values-lang-v3]

We can see the language has changed significantly.
The binary operations now act uniformly on @values-lang-v3[triv]s, which
represent expressions that directly compute to values, rather than
@asm-lang-v2[alocs], which represent locations on the machine where values are
stored.
Intermediate operations can be named using @values-lang-v3[let], which
binds an arbitrary number of independent computations to @tech{names}.

A @deftech{name}, or @deftech{lexical identifier}, is a placeholder that is
indistinguishable from the value of the expression it names.
Like @tech{abstract locations}, @tech{names} can be created at will.
Unlike @tech{abstract locations}, @tech{names} obey lexical binding, shadowing
the same name if an existing name is reused but without overwriting the old
value.
For example, the following example uses the same @tech{names} multiple times,
but this doesn't overwrite any other use of the @tech{name}.

@nested[#:style 'inset
@defproc[(interp-values-lang (p values-lang-v3?))
int64?]{
Interprets the @tech{Values-lang v3} program @racket[p] as a value.
For all @racket[p], the value of @racket[(interp-values-lang p)] should equal
to @racket[(execute p)].
}
]

@examples[#:eval sb
(interp-values-lang
 '(module
    (let ([x (let ([y 1]
                   [x 2])
               (+ y x))])
      (let ([y (let ([x 3]) x)])
        (+ x y)))))
]

This gives the programmer the ability to name sub-computations in a non-imperative way.
When we need some sub-computation, we are able to make up a brand new @tech{name}
without fear that we will erroneously overwrite some existing value; at worst,
we locally shadow it.
We also do not manually move values into locations, but rather name them and let
the language sort out where values live.

Some of the names for non-terminals are counter-intuitive.
For example, @values-lang-v3[triv] for trivial values makes sense, but
why is the expression @values-lang-v3[(+ 2 2)] a
@values-lang-v3[value]?
In this language, we use the name of a non-terminal to indicate in which
@emph{context} it is valid, not to merely describe the non-terminal's
productions.
@values-lang-v3[(+ 2 2)] is valid in any context that expects a value,
since it computes to a value and is indistinguishable from a value by the
operations of our language.
One context that expects a value is the right-hand side of a
@values-lang-v3[let] binding.
Only a @values-lang-v3[value] or a @values-lang-v3[let] whose body is a
@values-lang-v3[tail] is valid in tail context.

In @tech{Values-lang v3}, the @values-lang-v3[let] expression implements a
particular kind of lexcial binding.
The same @tech{lexical identifier} can be shadowed by nested
@values-lang-v3[let] expressions, and nested @values-lang-v3[let] expressions
can refer to the bindings of prior @values-lang-v3[let] expressions.
However, in the @emph{same} @values-lang-v3[let] expression, the binding are
considered @emph{parallel}---they do not shadow, and cannot refer to each other.
This means we can freely reorder the bindings in a single @values-lang-v3[let]
statement.
This can be useful for optimization.
However, it means we cannot allow duplicate bindings or allow reference to
bindings in the same @values-lang-v3[let] statement.
Otherwise, the ability to reorder could introduce @tech{undefined behaviour}.

To guard against this undefined behaviour, we introduce a validator.

We must also check for reference to unbound variables, since these may be
compiled into reference to uninitialized memory, and resulting in undefined
behaviour.

@nested[#:style 'inset
@defproc[(check-values-lang (p any/c))
values-lang-v3?]{
Takes an arbitrary value and either returns it, if it is a valid
@tech{Values-lang v3} program, or raises an error with a descriptive
error message.


@examples[#:eval sb
(check-values-lang
  '(module
     (let ([x 5]
           [y 6])
       x)))
(check-values-lang
 '(module
    (let ([x 5]
          [y 6])
      (let ([y x])
        y))))
(eval:error
  (check-values-lang
   '(module
      (let ([x 5]
            [y x])
        y))))
(eval:error
  (check-values-lang
   '(module
      (let ([x 5]
            [x 6])
        x))))
(check-values-lang
 '(module (let () 5)))
(eval:error
  (check-values-lang
    '(module (let () x))))
]
}
]

@;@todo{Patch instructions vs select instructions seems to come up naturally as I
@;design this assignment.
@;Re-adding it seems hard since the unspillable fixpoint iteration crosses so many
@;boundaries.
@;Would need to work it in starting this assignment, and ideally ending in the
@;next one.
@;}

@section{Abstracting from Assembly}
With @ch2-tech{Asm-lang v2}, we abstracted all machine details---there are no
more registers, no more odd machine-specific restrictions, just a
location-oriented abstract assembly language.

The language is a still an extremely imperative, non-compositional language.
We first add the ability to more easily compose imperative expression, before
moving to composing non-imperative expressions representing values.

But the assembly language is still an (@emph{extremely}) imperative machine
language.
The operations it provides are: (1) move a value to a location (2) perform a
binary operation on a location.
This requires the programmer to always remember the values of @tech{abstract
locations} and manipulate this underlying state to program.


We design @deftech{Imp-cmf-lang v3}, an imperative language that allows
expressing operations on values directly, and composing them by sequencing
these computation through @tech{abstract location}.
This removes some amount of imperativity from the language.
The programmer can reason about each primitive operation using only values, and
needs only to think about the state of the machine when composing operations.

@margin-note{@tech{Imp-cmf-lang v3} is in a variant of @deftech{a-normal form}
(@deftech{ANF}), a syntactic form that restricts all operations to trivial
values, and forbids nesting.
It is roughly equivalence to other compiler intermediate forms, such as
static-single assignment.}
@todo{Add ANF citation}
@todo{This is no longer ANF}

@bettergrammar*-diff[asm-lang-v2 imp-cmf-lang-v3]

We add the @imp-cmf-lang-v3[value] non-terminal to represent operations that
produce values at run time.
Now, @imp-cmf-lang-v3[set!] simply assigns any value, or operation that
produces a value, to an @tech{abstract location}.
At the top-level, a program is a @imp-cmf-lang-v3[tail], which represents the
last computation executed in a program.
It is either an operation that produces a value, or a sequence of such
operations composed by storing the result of the @imp-cmf-lang-v3[value] in an
intermediate @tech{abstract locations}.
The @tech{Imp-cmf-lang v3} program implicitly halts with the final value of the
@imp-cmf-lang-v3[tail].

To implement this language, we compile each operation to a sequence of
@ch2-tech{Asm-lang v2} instructions.
This involves select inserting an explicit @asm-lang-v2[halt] in the final value
of the @imp-cmf-lang-v3[tail], and selecting instruction sequences to implement
primitive operations, and possibly introducing auxiliary @tech{abstract locations}.

@nested[#:style 'inset
@defproc[(select-instructions (p imp-cmf-lang-v3))
         asm-lang-v2]{
Compiles @tech{Imp-cmf-lang v3} to @ch2-tech{Asm-lang v2}, selecting appropriate
sequences of abstract assembly instructions to implement the operations of the
source language.

@examples[#:eval sb
(select-instructions '(module (+ 2 2)))

(select-instructions
 '(module
    (begin (set! x.1 5) x.1)))

(select-instructions
 '(module
    (begin
      (set! x.1 (+ 2 2))
      x.1)))

(select-instructions
 '(module
    (begin
      (set! x.1 2)
      (set! x.2 2)
      (+ x.1 x.2))))
]
}
]

@tech{Imp-cmf-lang v3} forces us to carefully linearize all effects at the
top-level.
This is an annoying detail that the language could manage instead.

Below, we design @deftech{Imp-mf-lang v3}, which allows nesting effects in most
contexts.

@bettergrammar*-diff[imp-cmf-lang-v3 imp-mf-lang-v3]

@todo{Why not support empty begin?}
@todo{Where else should (nop) be added?}

@digression{@tech{Imp-mf-lang v3} is in @deftech{monadic form} (@deftech{MF}),
a syntactic form that allows composing operations that operate on values and
have no side-effect (such as changing the value of an @tech{abstract location}),
but requires explicit sequencing any effectful operations.
This allows additional nesting compared to @tech{ANF}.
@tech{Monadic form} is often used in high-level functional languages to support
reasoning about when side-effecting operations happen, and is also a useful
compiler intermediate form for the same reason.

@tech{ANF} @emph{almost} corresponds to a @deftech{canonical form} for
@tech{MF}, a syntactic form in which equal programs (for some notion of
equality) have the same representation.
The form is canonical in the sense that there is one right way to represent
every program.
All @tech{ANF} programs are also in @tech{MF}, but not all @tech{MF} programs
are in @tech{ANF}.

Writing transformations and optimizations over @tech{canonical forms} is often
easier since we do not have to manually consider two equal programs as they
have the same representation.
Unfortunately, transformations over @tech{canonical forms} are often tricky
because the a transformation may not preserve canonicity.
In the case of of @tech{MF} and @tech{ANF}, it is often easier to write the same
optimization over @tech{MF}, since @tech{MF} frees the optimization from
attempting to unnest operations.
On the other hand, selecting instructions over @tech{ANF} is simpler since
assembly features no nesting, and @tech{ANF} guarantees that no nesting exists.

Strictly speaking, our @tech{Imp-cmf-lang v3} is a variant of @tech{ANF} that is
not truely canonical, since we can nest @imp-cmf-lang-v3[tail]s in two different
ways:
@imp-cmf-lang-v3[(begin (begin (set! x.1 5)) x.1)] and
@imp-cmf-lang-v3[(begin (set! x.1 5) x.1)] are the same program.
However, this little bit of non-canonicity simplifies the work of the compiler
in some cases, and only complicates a single pass in the short-term---once we
add new features, it will not complicate anything, since we will be forced to
deal with the complication anyway.

This design choice is an example of where the compiler design, like most
software design, benefits from @emph{iteration}.
While the bottom-up approach of building layers on abstraction is often
beneficial, some design decisions may not be obvious until the software evolves.
}
@todo{Add citations for the ANF and MF papers}

@nested[#:style 'inset
@defproc[(normalize-bind (p imp-mf-lang-v3?))
          imp-cmf-lang-v3?]{
Compiles @tech{Imp-mf-lang v3} to @tech{Imp-cmf-lang v3}, pushing
@imp-mf-lang-v3[set!] under @imp-mf-lang-v3[begin] so that the right-hand-side of each
@imp-mf-lang-v3[set!] is simple value-producing operation.
This normalizes @tech{Imp-mf-lang v3} with respect to the equations
@tabular[
(list
(list
@imp-mf-lang-v3[(set! aloc (begin effect_1 ... value))]
"="
@imp-mf-lang-v3[(begin effect_1 ... (set! aloc value))])
)
]
}
]

@todo{This seems to be missing the most important equations...
(set! aloc (begin effect ... value)) = (begin effect ... (set! aloc value)).
(set! aloc (if pred value value) = (if pred (set! aloc value) (set! aloc value)))

And we don't want to remove nested begin, because when we add return in effect
context, we'll want nested begin.
}

@section{Be, not Do}
Now we want to abstract further, away from locations and focus on values and
operations on values.
This allows us to express computation as something that represents a value, not
a sequence of operations to compute a value.
That is, to declare what @emph{is}, not instruct what @emph{to do}.
This is a move toward @emph{declarative} programming.

@tech{Values-lang v3} is the final source language for this week.
It abstracts away from operations on locations and instead has operations on
values.
It also include a feature for binding values to names for reuse.
The value of a @tech{Values-lang v3} program is the final value of an
expression.

But dealing with names is hard, so we make a simplifying assumption.
We assume that actually, someone has already resolved all @tech{names} into
@tech{abstract locations}.
This gives us the language @deftech{Values-unique-lang v3}, defined below.

@bettergrammar*[values-unique-lang-v3]

This language is similar to @tech{Imp-mf-lang v3}, but uses
@values-unique-lang-v3[let] to compose operations rather than sequential,
imperative @imp-mf-lang-v3[set!] instructions.
These two operations have different semantics.
All of the bindings in a single @values-unique-lang-v3[let] are assumed to be
independent of each other, and can happen in any order.
They should not be thought of as sequential, but simply declarations that some
bindings to some values exist.
For example, the following two @tech{Values-unique-lang v3} programs are
equivalent:
@tabular[
(list
 (list
  @values-unique-lang-v3[(let ([x.1 5] [y.2 6]) (+ x.1 y.2))]
  "="
  @values-unique-lang-v3[(let ([y.2 6] [x.1 5]) (+ x.1 y.2))]))
]

However, the apparently similar @tech{Imp-mf-lang v3} programs are not, since
@imp-mf-lang-v3[set!]s are required to happen in-order:
@tabular[
(list
 (list
  @imp-mf-lang-v3[(begin (set! x.1 5) (set! y.2 6) (+ x.1 y.2))]
  "≠"
  @imp-mf-lang-v3[(begin (set! y.2 6) (set! x.1 5) (+ x.1 y.2))]))
]

This means we can implement optimizations over @tech{Values-unique-lang v3} that
are not possible in @tech{Imp-mf-lang v3}, although no such optimizations are
apparent yet.

@nested[#:style 'inset
@defproc[(optimize-let-bindings (p Values-unique-lang-v3.p))
          Values-unique-lang-v3.p]{
Optimizes @values-unique-lang-v3[let] bindings by reordering them to minimize or
maximize some metric.
}
]

To implement @tech{Values-unique-lang v3}, we must sequentalize @values-unique-lang-v3[let].

@nested[#:style 'inset
@defproc[(sequentialize-let (p Values-unique-lang-v3.p))
          imp-mf-lang-v3.p]{
Compiles @tech{Values-unique-lang v3} to @tech{Imp-mf-lang v3} by picking a
particular order to implement @values-unique-lang-v3[let] expressions using
@imp-mf-lang-v3[set!].
}
]

Finally, we must discharge our assumption that all names are unique.
Below we define @deftech{Values-lang v3}, a value-oriented language with simple
binary expressions and lexical binding.

@bettergrammar*-diff[values-unique-lang-v3 values-lang-v3]

@values-lang-v3[x] is a short-hand nonterminal for names, which are arbitrary
symbols.
To ensure transform names into abstract location, it suffices to append a unique
number to each.

To implement this language, we must resolve all lexical binding and replace
@tech{names} by @tech{abstract locations}

@nested[#:style 'inset
@defproc[(uniquify (p Values-lang-v3.p))
          Values-unique-lang-v3.p]{
Compiles @tech{Values-lang v3} to @tech{Values-unique-lang v3} by resolving all
@tech{lexical identifiers} to @tech{abstract locations}.

@examples[#:eval sb
(uniquify '(module (+ 2 2)))

(uniquify
 '(module
    (let ([x 5])
      x)))

(uniquify
 '(module
    (let ([x (+ 2 2)])
      x)))

(uniquify
 '(module
    (let ([x 2])
      (let ([y 2])
        (+ x y)))))

(uniquify
 '(module
    (let ([x 2])
      (let ([x 2])
        (+ x x)))))]}]

@section{Appendix: Overview}

@figure["fig:v3-graph" "Overview of Compiler Version 3" v3-graph]
