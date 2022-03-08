#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a7-solution)
  (for-label (prefix-in v5: cpsc411/reference/a5-solution))
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v6
  cpsc411/langs/v6-5
  (for-label cpsc411/langs/v6 cpsc411/langs/v6-5))

@(provide (all-defined-out))


@(define (ch-v6-tech . rest)
   (apply tech #:tag-prefixes '("book:" "chp-return:") rest))

@declare-exporting[cpsc411/reference/a7-solution]

@(define sb
   (make-cached-eval
    "ch6-5-eval"
    '(require racket/pretty cpsc411/reference/a7-solution cpsc411/compiler-lib)))

@define[v6.5-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

Lx [label="Exprs-lang v6.5"];
Ly [label="Exprs-unique-lang v6.5"];
L1 [label="Values-unique-lang v6"];
L3 [label="Imp-mf-lang v6"];
L2 [label="Proc-imp-cmf-lang v6"];
L4 [label="Imp-cmf-lang v6"];
L5 [label="Asm-pred-lang v6"];
L6 [label="Asm-pred-lang v6/locals"];
L7 [label="Asm-pred-lang v6/undead"];
L8 [label="Asm-pred-lang v6/conflicts"];
L81 [label="Asm-pred-lang v6/pre-framed"];
L82 [label="Asm-pred-lang v6/framed"];
L83 [label="Asm-pred-lang v6/spilled"];
L9 [label="Asm-pred-lang v6/assignments"];
L10 [label="Nested-asm-lang-fvars v6"];
L10_1 [label="Nested-asm-lang v6"];
L11 [label="Block-pred-lang v6"];
L12 [label="Block-asm-lang v6"];
L12_1 [label="Para-asm-lang v6"];
L14 [label="x64"];
L15 [label="integer"]

/* Register allocator */

edge [fontname="Courier", fontsize=12, labeljust=right]

L5 -> L6 [label=" uncover-locals"];
L6 -> L7 [label=" undead-analysis"];
L7 -> L8 [label=" conflict-analysis"];
L8 -> L81 [label= " assign-call-undead-variables"];
L81 -> L82 [label=" allocate-frames"];
L82 -> L83 [label=" assign-registers"];
L83 -> L9 [label=" assign-frame-variables"];
L9 -> L10 [label=" replace-locations"];

Lx -> Lx [label=" check-exprs-lang"];
Lx -> Ly [label=" uniquify"];
Ly -> L1 [label=" remove-complex-opera*"];
L1 -> L3 [label=" sequentialize-let"];
L3 -> L2 [label=" normalize-bind"];
L2 -> L4 [label=" impose-calling-conventions"]
L4 -> L5 [label=" select-instructions"];

L10 -> L10_1 [label=" implement-fvars"];
L10_1 -> L11 [label=" expose-basic-blocks"];
L11 -> L12 [label=" resolve-predicates"]
L12 -> L12_1 [label=" flatten-program"];
L12_1 -> L16 [label=" patch-instructions"];
L16 -> L14 [label=" generate-x64"];
L14 -> L15 [label=" execute"];

subgraph DoNotcluster1 {
  graph [labeljust=right,
    style=filled,
    color=lightgrey,
    fontname="Courier",
    fontsize=10,
    label = "interp-paren-x64";
  ];
  edge [fontname="Courier"]

  L16 [label="Paren-x64 v6"];
  L17 [label="Paren-x64-rt v6"];
}

  L16 -> L17 [label=" link-paren-x64"];
  L17 -> L15 [label=" interp-loop"];
  L16 -> L15 [label=" interp-paren-x64"];
}
}
]

@title[#:tag "top" #:tag-prefix "chp-ae:"]{Algebraic Expressions}

@section{Preface: What's wrong with @ch-v6-tech{Values-lang v6}}
@ch-v6-tech{Values-lang v6} gained the ability to express non-tail calls, an
important step forward in writing real programs.
However, when writing programs, we have to manually sequence computation in an
annoying and unnatural way.
We cannot merely write the natural definition of factorial:
@exprs-lang-v6.5-block[
(module
  (define fact
    (lambda (n)
      (if (= n 0)
          1
          (* n (fact (- n 1))))))
  (fact 5))
]

In @ch6-tech{Values-lang v6}, we're forced to, manually, systematically, rewrite
this to introduce a bunch of temporary names and bind all the intermediate
computations:
@values-lang-v6-block[
(module
  (define fact
    (lambda (n)
      (if (= n 0)
          1
          (let ([y (- n 1)])
            (let ([z (fact y)])
              (* n z))))))
  (fact 5))
]

As compiler designers, any tedious, systematic transformation should scream to
us "this is a job for a compiler!".

In this chapter, we add @deftech{algebraic expressions}---the ability to nest
expressions arbitrarily, rather than manually sequence and names their values.
We exclude predicate position, since predicates are still their own sub-language
until we have a boolean data type.
@margin-note{
@emph{Algebraic} here refers to the fact that these expressions can be modeled
mathematically as an algebraic structure---they have a set of elements (the
64-bit intergers), and some operations on those elements (*, +, -, etc) that
satsify various algebraic laws, including commutativity, associativity, etc.
This allows the user of such expressions to reason algebraically, manipulating
expressions according to those laws, instead of reasoning about execution steps
of the underlying machine.
}

@section{Algebraic Expressions}
We design @deftech{Exprs-lang v6.5}, that allows @tech{algebraic
expressions} in most positions.
Now, there is one top-level context: the value context.
Nearly every position, aside from the operator in a @exprs-lang-v6.5[call], and
the predicate position of @exprs-lang-v6.5[if], expects a value-producing
expression.
@exprs-lang-v6.5[tail] is eliminated, as the user of this language no longer
needs to know about the distinction between @exprs-lang-v6.5[value] and
@exprs-lang-v6.5[tail] context.
The predicate position of @exprs-lang-v6.5[if] expressions is still
restricted, since we cannot introduce algebraic expressions in predicate
position without a run-time representation of their value, @ie without
booleans.
Similarly, @exprs-lang-v6.5[call] must still be statically restricted until we
can dynamically distinguish procedures from machine integers.

@bettergrammar*-ndiff[
#:labels ("Diff vs v6" "Exprs-lang v6.5")
(values-lang-v6 exprs-lang-v6.5)
(exprs-lang-v6.5)
]

we can see a significant change in the syntax.
In essense, previously, @values-lang-v6[value] had to be explicitly bound by a
@values-lang-v6[let], and operands to our primitive operations had to be
trivial.
Now, operands can be arbitrarily nested expressions that appeared in
@values-lang-v6[value] context.

Ignoring @racket[uniquify] for a moment, consider how to implement this in terms
of @ch-v6-tech{Values-unique-lang v6}.
Looking at this grammar, it may not be obvious how to transform this.
We can see the structure of the pass more clearly if we expand the grammar
slightly.
Below, we define the expanded grammar @deftech{Exprs-unique-lang v6.5/context}.
We typeset the differences compared to @ch-v6-tech{Values-unique-lang v6}

@bettergrammar*-ndiff[
#:labels ("Diff vs v6" "Exprs-unique-lang v6.5/context")
(values-unique-lang-v6 exprs-unique-lang-v6.5/context)
(exprs-unique-lang-v6.5/context)
]

Now we can see the the main difference, semantically, is replacing all trivial
@exprs-unique-lang-v6.5[opands].
Calls and binary operations can now take nested @exprs-unique-lang-v6.5[value]
expressions rather than @exprs-unique-lang-v6.5[opand]s.
While this means we can collapse the syntax, separating the syntax semantically
helps us see the true difference and see that the essence of the transformation
is @exprs-unique-lang-v6.5[let]-binding intermediate results to make all
operands trivial.

To transform this into @tech{Values-unique-lang v6}, we need to perform the
monadic-form translation.
In essence, we recursively translate any nested expression that is not a
@values-unique-lang-v6[value] into one by let-binding all intermediate
computations.
For example, we would transform a call @racket[`(call ,e_1 ,e_2)] into something
like the following:
@racketblock[
`(let ([,x_1 ,(normalize e_1)])
    (let ([,x_2 ,(normalize e_2)])
      (call ,x_1 ,x_2)))
]
If you've written any examples or tests in @ch-v6-tech{Values-lang v6}, you've
probably done this tranformation by hand many times.

@digression{
The above examples is intuitively all we need to do, and would produce valid
output.
However, we can do a better job if we design each function around the template
for the @emph{output language} while processing the @emph{input language}.
In this way, instead of designing a function merely to process the input
@exprs-lang-v6.5[value], we design a function to output
@exprs-unique-lang-v6.5[aloc], from some input.
These functions describe in what context the current term is being processed.

For example, consider transforming @exprs-unique-lang-v6.5[(lambda (aloc ...) value)].
We must produce a @exprs-unique-lang-v6.5[tail] (from the input
@exprs-unique-lang-v6.5[value]).
While we could do this by producing @exprs-unique-lang-v6.5[(let ([x value])
x)], by designing a function that expects to produce something in
@exprs-unique-lang-v6.5[tail] context, we can recognize that any input is valid in
@exprs-unique-lang-v6.5[tail], so no intermediate binding is required.

On the other hand, when processing @exprs-unique-lang-v6.5[(binop value_1
value_2)], we want to process @exprs-unique-lang-v6.5[value_1] in
@exprs-unique-lang-v6.5[opand] context, since that's the restriction in the
target language.
Since the function is designed around the output, it knows it can avoid binding
certain inputs, but not others.

If we were following the template for the source, both of these would merely
call a helper for @exprs-unique-lang-v6.5[value]s; instead, each of them should call a
helper for the target context: @exprs-unique-lang-v6.5/context[tail] or
@exprs-unique-lang-v6.5/context[opand], respectively.
}

@section{Implementing Algebraic Expressions}
Implementing @tech{algebraic expressions} via the above translation is quite
easy, and we really could have done it any time before now.
The primary reason we haven't is because, so far, we haven't needed them.
We have been designing and writing compiler passes rather than programs, and
most of our compiler passes necessarily needed to target low-level languages
with low-level instructions, and not languages with @tech{algebraic
expressions}.

We'll add a new pass, which we call this pass @racket[remove-complex-opera*],
because it makes complex operands (and later, operators) trivial.
This pass will be just after @racket[uniquify], so only new high-level
abstractions will be able to take advantage of it.

We must still rule out many undefined uses of procedures, so we still forbid
@tech{algebraic expressions} in operator position, and require a validator to
check procedure calls.
The heuristics remain the same as in @racket[v5:check-values-lang].

@nested[#:style 'inset]{
@defproc[(check-values-lang [p exprs-lang-v6.5?]) exprs-lang-v6.5?]{
Validates that the @tech{Exprs-lang v6.5} is well bound and well typed: all
procedure calls pass the correct number of arguments, and all
@exprs-lang-v6.5[binop] and @exprs-lang-v6.5[relop] are never used on
procedures.
}
}

As always, we implement @racket[uniquify].
Collapsing the syntax and following the template for @tech{Exprs-lang v6.5}
simplifies the design and implementation, without any cost.

@nested[#:style 'inset]{
@defproc[(uniquify [p exprs-lang-v6.5?])
         exprs-unique-lang-v6.5?]{
Resolves top-level @ch3-tech{lexical identifiers} into unique labels, and all
other @ch3-tech{lexical identifiers} into unique @ch2-tech{abstract locations}.
}
}

Finally, we implement @racket[remove-complex-opera*], which transforms each
complex operand by explicitly unnesting and let-binding all intermediate
computation.
We have to pick an evaluation order to do this, which is arbitrary.
While this is not observable yet, it can quickly become observable if we ever
add mutation, printing, or other kinds of statements to the source language.
To ensure consistency, we decide that all operands should be evaluated in order
from left to right.

This pass is simpler to design following the template for the source, but
produces fewer unnecessary expressions if we follow the template for the target.

@nested[#:style 'inset]{
@defproc[(remove-complex-opera* [p exprs-unique-lang-v6.5?])
         values-unique-lang-v6?]{
Performs the monadic form transformation, unnesting all non-trivial operators
and operands to @exprs-unique-lang-v6.5[binop]s, and
@exprs-unique-lang-v6.5[call]s making data flow explicit and and simple to
implement imperatively.
}
}

All the other passes remain completely unchanged from
@Secref[#:tag-prefixes '("book:" "chp-return:")]{top}.

@;We've already seen a transformation that can elaborate this into
@;@tech{Values-bits-lang v7}: the A-normal form translation.
@;The idea is essentially to transform expressions like
@;@racket[`(apply ,e_1 ,e_2)] into
@;@racketblock[
@;`(let ([,x_10 ,v_10])
@;   ...
@;    (let ([,x_1 ,v_1])
@;      (let ([,x_20 ,v_20])
@;        ...
@;        (let ([,x_2 ,v_2])
@;          (apply ,x_1 ,x_2)))))
@;]
@;
@;@todo{make self-contained; removed references to class}
@;As we discussed in class, dealing with code duplication for in
@;@exprs-bits-lang-v7[if] expressions is a bit annoying in ANF, so our compiler is
@;designed a bit differently.
@;In monadic, we can simply this and let a later pass deal with
@;@exprs-bits-value[if] when

@;I recommended that you use a procedural accumulator, @ie write the
@;transformation in CPS.
@;This will turn your mind inside out, but after that, the code writes itself.
@;
@;The translation of @object-code{if} is tricky.  There are two ways to
@;accomplish it, but one of them requires first-class functions, so we must use
@;the naive form for now.
@;You must essentially push the surrounding context into the branches of the
@;@object-code{if}:
@;@racketblock[
@;(equal?
@;  (a-normalize `(module (+ (if (eq? 1 1) 5 6) 5)))
@;  `(module
@;     (if (eq? 1 1)
@;         (+ 5 5)
@;         (+ 6 5))))
@;]
@;This is straightforward, particularly in CPS, and correct, but can cause
@;considerable code duplication.
@;We'll address that later.
@;@;Or, you can introduce a @deftech{join-point}, a function that abstracts over the
@;@;context and is called in both branches:
@;@;@racketblock[
@;@;(equal?
@;@;  (a-normalize `(module (+ (if (eq? 1 1) 5 6) 5)))
@;@;  `(module
@;@;     (define L.jp.1 (lambda (x) (+ 5 x)))
@;@;     (if (eq? 1 1)
@;@;         (apply L.jp.1 5)
@;@;         (apply L.jp.1 6))))
@;@;]
@;@;This introduces a function call, but it's a known function in tail position, so
@;@;it can be heavily optimized.
@;
@;A-normal form was introduced in
@;@hyperlink["https://slang.soe.ucsc.edu/cormac/papers/pldi93.pdf"]{this paper},
@;which you might be interested in reading, and it is still of active interest in
@;the compiler construction and research community.

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v6.5-graph" "Overview of Compiler Version 6.5" v6.5-graph]
