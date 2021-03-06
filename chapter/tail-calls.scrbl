#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a5-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v2
  cpsc411/langs/v3
  cpsc411/langs/v4
  cpsc411/langs/v5)

@(provide
  (all-defined-out)
  #;(except-out (all-defined-out) sb))

@declare-exporting[cpsc411/reference/a5-solution]

@;{(define sb
    (make-cached-eval
     "ch5-eval"
     '(require cpsc411/v1-reference/a5-solution cpsc411/compiler-lib)
     '(current-stack-size 512)))}

@; ----- Language Defs ------

@define[v5-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

L0 [label="Values-lang v5"];
L1 [label="Values-unique-lang v5"];
L2 [label="Proc-imp-mf-lang v5"];
L3 [label="Imp-mf-lang v5"];
L4 [label="Imp-cmf-lang v5"];
L5 [label="Asm-pred-lang v5"];
L10 [label="Nested-asm-lang v5"];
L11 [label="Block-pred-lang v5"];
L12 [label="Block-asm-lang v4"];
L12a [label="Para-asm-lang v4"];
L13 [label="Paren-x64-fvars v4"];
L14 [label="x64"];
L15 [label="integer"]

/* Register allocator */

subgraph DoNotcluster0 {
  graph [labeljust=right,
    style=filled,
    color=lightgrey,
    fontname="Courier",
    fontsize=12,
    label = "assign-homes-opt";
  ];
  edge [fontname="Courier", fontsize=10]

  L6 [label="Asm-pred-lang v5/locals"];
  L7 [label="Asm-pred-lang v5/undead"];
  L8 [label="Asm-pred-lang v5/conflicts"];
  L9 [label="Asm-pred-lang v5/assignments"];
}

edge [fontname="Courier", fontsize=12, labeljust=right]

L5 -> L6 [label=" uncover-locals"];
L6 -> L7 [label=" undead-analysis"];
L7 -> L8 [label=" conflict-analysis"];
L8 -> L9 [label=" assign-registers"];
L9 -> L10 [label=" replace-locations"];

L5 -> L10 [label=" assign-homes-opt"];


L0 -> L0 [label=" check-values-lang"];
L0 -> L1 [label=" uniquify"];
L1 -> L2 [label=" sequentialize-let"];
L2 -> L3 [label=" impose-calling-conventions"]
L3 -> L4 [label=" canonicalize-bind"];
L4 -> L5 [label=" select-instructions"];

L10 -> L11 [label=" expose-basic-blocks"];
L11 -> L12 [label=" resolve-predicates"]
L12 -> L12a [label=" flatten-program"]
L12a -> L13 [label=" patch-instructions"];
L13 -> L16 [label=" implement-fvars"];
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

  L16 [label="Paren-x64 v4"];
  L17 [label="Paren-x64-rt v4"];
}

  L16 -> L17 [label=" link-paren-x64"];
  L17 -> L15 [label=" interp-loop"];
  L16 -> L15 [label=" interp-paren-x64"];
}
}
]

@; --------------------------

@title[#:tag "top" #:tag-prefix "chp5:"]{Procedural Abstraction: Call}
@todo{In language defs: why rloc as pred instead of either reg or fvar?}

@section{Preface: What's wrong with our language?}
In the last chapter, we designed the language @ch4-tech{Values-lang v4} with
support for structured control-flow operations.
This enables our programs to make run-time decisions that influence what code is
executed.
However, the language is missing a key feature necessary for practical
programming: the ability to reuse code.

In this chapter, we introduce a common method of reusing code: procedural
abstraction.
We introduce a limited form of @tech{procedure} that is essentially similar to
the @ch4-tech{basic blocks} from the last chapter.
This feature is a thin layer of abstraction over the @tt{jmp} instruction that we
can @emph{safely} expose in the source language.
Our low-level language already exposes @tt{jmp}, so we have all the machinery we
need from our low-level languages.
However, we need to solve a critical problem: designing a @tech{calling
convention}.
@margin-note{Procedures are sometimes called functions.
This is often a misnomer, since "function" evokes a purely mathematical
construct specifying input and output pairs that does not necessarily give rise
to an algorithm, while a procedure is code that executes algorithmically, and may
rely on machine state in addition to its declared inputs and outputs.}

Normally, @tech{procedures} are thought about as composed of two features:
@tech{call} and @ch6-tech{return}.
However, these are two separate abstractions.
In this chapter, we introduce only the @tech{call} abstraction.
@tech{Procedures} can be called, but never @ch6-tech{return}.
We limit where such as call can happen so we do not have to answer
inconvenient questions.

@section{Designing a source language with call}
Below, we define @deftech{Values-lang v5} by extending @ch4-tech{Values-lang-v4}
with a @deftech{tail calls}---a procedure @emph{call} in @values-lang-v5[tail]
position.

@bettergrammar*-diff[values-lang-v4 values-lang-v5]

A program now begins with a set of declared @deftech{procedures}, named blocks
of code that are parameterized over some data indicated by the list of names
@values-lang-v5[(x ...)], called the @deftech{parameters}.
Each of these @tech{procedures} can be used by the @deftech{call} abstraction
@values-lang-v5[(call name value ...)], which takes the name of a
@tech{procedure} and a list of values, called the @deftech{arguments}, with
which to instantiate the @tech{parameters}.
At run-time, the @tech{call} unconditionally transfers control to the code
declared in the definition of the @tech{procedure} @values-lang-v5[name].

Notice this @tech{call} is restricted to @values-lang-v5[tail] context.
If we allowed a @tech{call} in value context, for example, then we could have a
call on the right-hand side of a @values-lang-v5[let].
This would transfer control with a @values-lang-v5[tail] still remaining to be
executed.
Defining what this means in a sensible way is difficult, and requires
introducing some abstraction to transfer control back from a procedure to the
middle of some existing computation.
Recall that a @values-lang-v5[value] in @values-lang-v5[tail] context is
implicitly the final answer, and is compiled to a @imp-mf-lang-v5[halt]
instruction.
In terms of the lower-level languages, we can understand a @tech{tail call} as
jumping until the program reaches a @imp-mf-lang-v5[halt] instruction.

We already have the machinery to compile procedure definitions and calls.
Procedure definitions are transformed into basic blocks, and calls into,
essentially, @tt{jmp} instructions.

The only question is how to pass @tech{arguments}.
The call instruction needs to know in which locations to store the
@tech{arguments}, and the called @tech{procedure} needs to know from which
locations to read its @tech{parameters}.
The problem is deciding how to ensure the locations end up the same.

@todo{Be careful with arguments vs parameters}

@section{Calling Conventions Introduction}
Introducing a @tech{calling convention} solves the problem mentioned above.

The @deftech{calling convention} gives us a pattern to set up a call to @emph{any}
procedure.
We fix a set of @emph{@ch2-tech{physical locations}}.
Both the caller and the callee agree that those locations are the only thing
they need to know about.
Every @tech{call} will first set those locations and pass control to the @tech{procedure}.
@;In the process, the caller will need to ensure that it does not need the values
@;in those physical locations.
Every @tech{procedure} will read from those locations on entry, and move the
@tech{arguments} into its own @ch2-tech{abstract locations}.
This way, no procedure needs to know about another's @ch2-tech{abstract
locations}, and we maintain the per-block regsiter allocator we've used until
now.

@digression{
@todo{This digression seems... more important than a digression.}
Strictly speaking, we don't need to use @ch2-tech{physical locations} for our
calling convention.
What we need is a set of global, shared locations, whose names are unique across
the entire program, and any program that we might link with.
The register allocator needs to assign all uses of these shared locations to the
same @ch2-tech{physical locations}.

A simple implementation of these global shared location is to use
@ch2-tech{physical locations}.
@ch2-tech{Physical locations} are automatically globally consistent and unique, and
automatically known by all programs we might link with.
If we allow those @ch2-tech{physical locations} to be registers, we can generate
very fast procedure calls by keeping memory out of the picture as much as
possible.
Unfortunately, using them requires that we expose @ch2-tech{physical locations}
through every layer of abstraction up to the point where we implement the
calling convention.
@;That's every language from @a4-tech{Para-asm v2} to @a4-tech{Block-lang}.
This makes all our abstractions only partial abstractions, and injects undefined
behavior back into our intermediate languages.

We could also use the stack to implement the calling convention.
This is simpler, as we can keep registers abstract and need to expose
memory high in the compiler pipeline anyway, but slower since every procedure
call must now access memory.

We could try to create global @ch2-tech{abstract locations}.
However, then our register allocator will only work over whole programs; we
won't be able to support separate compilation, without implementing a separate,
lower-level calling convention.
This would create unnecessary indirection and be less efficient.

We could try to introduce abstract call-setup and return-from-call
instructions, and leave it to the register allocator to implement these in terms
of physical locations.
This complicates the register allocator, an already complicated pass.
It also mixes concerns: assigning @ch2-tech{abstract locations} to
@ch2-tech{physical locations}, and generating the instructions to implement call
and return.
At some level, the register allocator will have to perform the transformation
we're already suggesting: first setup physical locations for the call,
create conflicts between those physical locations and abstract locations, then
assign registers.
At very least, we need to do this before conflict analysis, so we might as well
do it before all the analyses.
This leads us right back to the original design: expose @ch2-tech{physical
locations} through the register allocator, high in the compiler pipeline.
}

Our calling convention is based on the @|x64-abi|.
We deviate from it slightly and develop a similar, but simplified, calling
convention.
The calling convention is defined by parameters in
@racketmodname[cpsc411/compiler-lib].

Our calling convention passes the first @racket[n] arguments as registers,
using the set of registers defined in the parameter
@racket[current-parameter-registers].
The default value is @racket['(unsyntax (current-parameter-registers))], which
is defined by the @|x64-abi| to be where the first 6 arguments of any
@tech{procedure} are stored.
To deal with an arbitrary number of arguments, we may need more than the
@racket[n] registers we have available for parameters.
For the rest, we use fresh frame variables.

Since @tech{tail calls} never return, we do not need to worry about what is on
the frame before a call.
We can simply overwrite all existing frame variables.
This means recursive @tech{tail calls} have the same performance characteristic
as loops: they use a constant amount of stack space, compile directly to jumps,
and only need registers as long as they use fewer than 6 arguments.

@subsection[#:tag "design-convention-translation"]{Designing the Calling Convention Translation}
To design our calling convention translation, we start by looking at how we want
to translate terms into abstraction we already know about.
We then redesign our intermediate languages to support our translation.

@digression{
We must be careful to design a translation that eliminates some abstraction
layer, or we risk developing a translation that simply kicks the real problem
down to the next language.
A good heuristic here is to avoid designing a translation that introduces any
new abstractions.
If we need new abstractions, we should look at the problem bottom up---designing
the proposed abstraction as an abstraction of some features expressible in the
target language.
}

Intuitively, we know how to compile calls.
We want to instantiate the @tech{parameters} of the @tech{procedure}, moving
@tech{arguments} to the shared locations, then jump to the label of the
@tech{procedure}.
Concretely, we want to perform the following transformations.

@itemlist[

@item{
When transforming a procedure @racket[`(lambda (,x_0 ... ,x_n-1 ,x_n ...
,x_n+k-1) ,body)], we generate:
@racketblock[
`(begin
   (set! ,x_0 ,r_0)
   ...
   (set! ,x_n-1 ,r_n-1)
   (set! ,x_n ,fv_0)
   ...
   (set! ,x_n+k-1 ,fv_k-1)
   ,body)
]
where:
@itemlist[
@item{@racket[fv_0 _... fv_k-1] are the first @racket[k] @ch2-tech{frame variable}.
}
@item{@racket[r_0 _... r_n-1] are the @racket[n] physical locations from
@racket[current-parameter-registers].}
]

The order of the @imp-mf-lang-v5[set!]s is not important for correctness, but
can influence optimizations.
We want limit the live ranges of registers to help the register allocator make
better use of registers, so we should generate accesses to registers first to
limit their live ranges.
}

@item{When transforming a @tech{tail call}, @racket[`(call ,v
,v_0 ... ,v_n-1 ,v_n ... ,v_n+k-1)], we generate:
@racketblock[
`(begin
   (set! ,fv_0 ,v_n) ...
   (set! ,fv_k-1 ,v_n+k-1) ...
   (set! ,r_0 ,v_0) ...
   (set! ,r_n-1 ,v_n-1) ...
   (jump ,v ,fbp ,r_0 ... ,r_n-1 ,fv_0 ... ,fv_k-1))
]
where:
@itemlist[
@item{@racket[fbp] is the physical location storing the frame base pointer,
@racket[current-frame-base-pointer-register].}
@item{all other meta-variables are the same as in the case for transforming
@object-code{lambda}.}
]
@;Note that form a tail call, we do not modify the stack for the caller.
@;The caller is able to reuse the callee's frame and return address, since there
@;is no more computation left in this function.

Again, the order of the @imp-mf-lang-v5[set!]s is not important for correctness,
but can enable better use of registers and optimizations.
This time, we move values into the registers last, to keep there lives as short
as possible.

Here, we decorate the @imp-mf-lang-v5[jump] instruction with its
@ch-ra-tech{undead-out set}.
Going top-down, it is not obvious why we would do this.
@todo{Do I want to include this or leave it for lecture? I think I want the book
to be self-contained, but I don't want to give away the question from the
previous chapter... eats a good assignment question. OTOH, students probably
won't read ahead.}
@list{
We know we want to compile a call to a series of assignments and a jump.
But, since we're going top-down, we need to ask: will the lower-level language
be able to implement jump?
We must look bottom-up at how we would expose jump from the lower level languages.

We will need to expose @block-pred-lang-v5[jump] from @tech{Block-pred-lang v5},
all the way up to whatever this transformation targets.
This means at least exposing @block-pred-lang-v5[jump] through the regsiter
allocator.
In @racket[undead-analysis], we will need to decide what set of locations is
@ch-ra-tech{undead} after a @block-pred-lang-v5[jump].
In general, if we want to support separate compilation, we cannot answer that
question.
To design this @block-pred-lang-v5[jump] abstraction in a way we will be able to
implement requires explicitly annotating it with its @ch-ra-tech{undead-out set}.
}

@;By loading the return address first, we keep live range of @racket[tmp-rp]
@;limited.
}
]

Now that we know the translation we would like to perform, we need to figure out
where to fit the new translation in our compiler pipeline.
To do this, we need to ask a few questions.

First: which target languages support the features needed in the target of our
translation?
If we look at the pipeline from the last chapter, @Secref[#:tag-prefixes
'("book:" "chp4:")]{sec:overview}, we know the translation must come at least after
@racket[sequentialize-let], since the target language will need to use
imperative features introduced in that pass.
Introducing calling conventions also generates @imp-mf-lang-v5[begin] statement
in tail position, and generates @imp-cmf-lang-v4[set!] expressions with a
combination of values and locations for operands.
This last feature tells us we should place the new pass before
@racket[select-instructions], since we want the unrestricted
@imp-cmf-lang-v4[set!] feature @racket[select-instructions] provides.

Second: which translations might be disrupted by introducing new features in
their source language?
If we add the new translation before @racket[canonicalize-bind], then we won't
need to extend @racket[canonicalize-bind] to support the @values-lang-v5[call]
construct, but we will need to make sure @racket[canonicalize-bind] handles the
@block-pred-lang-v5[jump] construct.
This might be a problem, if @block-pred-lang-v5[jump] appears in effect
context, but note that since @values-lang-v5[call] appears in tail context, so
must the @block-pred-lang-v5[jump] that @values-lang-v5[call] compiles to.
This suggests we could place the new pass either right before or right
after @racket[canonicalize-bind].

Third, we try to future proof our design decisions: will the answer to any of
the above questions change if we add new features in the future?
This is hard to predict; the future is vast so the search space is large.
We can limit our search by focusing on limitations in features we're now adding.
We just added @values-lang-v5[call] in tail context.
Are we likely to lift this restriction and allow calls in other contexts later?
Yes, that seems likely; most languages allow calls in non-tail context.
If we allowed calls in effect context, @eg then our translation would need to
generate a @imp-mf-lang-v5[begin] in effect context, and not just tail context.
This tells us we want to support nested @imp-mf-lang-v5[begin] in the target
language of our new pass.
Since we changed the target of @racket[canonicalize-bind] to allow nested
@imp-cmf-lang-v5[begin], we can still place our new pass before or after
@racket[canonicalize-bind].

Finally (and it's important to consider last), we consider minor matters of
performance.
Since the new pass will introduce many new instructions, it will increase the
size of code each pass must transform, and thus slow down the compiler.
If we place the new pass after @racket[canonicalize-bind], then 1 fewer pass has
to run on the larger code.

We conclude the new pass should go just after @racket[canonicalize-bind].
Since we're proceeding top-down, we start by extending the first few passes down
to @racket[canonicalize-bind].

@section{Extending front-end with support for call}

We start by formally defining @deftech{Values-lang v5}, the new source language.
We typeset the difference compared to @ch4-tech{Values-lang v4}.

@bettergrammar*-diff[values-lang-v4 values-lang-v5]

Modules now define a set of @tech{procedures} at the top-level before the
initial @values-lang-v5[tail].
The program begins executing the @values-lang-v5[tail] following the set of
definitions.

The defined @tech{procedures} can be used anywhere in the module.
We support mutually recursive calls, so @tech{procedures} defined later in the
module can be referenced by definitions earlier in the module.
For example, the following is a valid @tech{Values-lang v5} program:
@; TODO Requires preserving srcloc etc, which ought to have been preserved...
@;@nested[#:style 'code-inset]{
@;@values-lang-v5[
@;(module
@;  (define odd?
@;    (lambda (x)
@;      (if (= x 0)
@;          0
@;          (let ([y (- x 1)])
@;            (even? y)))))
@;  (define even?
@;    (lambda (x)
@;      (if (= x 0)
@;          1
@;          (let ([y (- x 1)])
@;            (odd? y)))))
@;  (even? 5))
@;]
@;}

@codeblock{
(module
  (define odd?
    (lambda (x)
      (if (= x 0)
          0
          (let ([y (+ x -1)])
            (call even? y)))))
  (define even?
    (lambda (x)
      (if (= x 0)
          1
          (let ([y (+ x -1)])
            (call odd? y)))))
  (call even? 5))
}

We continue to require that the source program is well bound: all @ch3-tech{lexical
identifiers} are defined before they are used.
We also restrict procedure to not bind the same identifier twice.
We could allow this and define a shadowing order, but this would always
introduce a dead variables and is probably a mistake in the source language.

We have not introduced a method for dynamically checking that a procedure is
used correctly yet.
To avoid exposing undefined behaviour, we make an additional restriction to
@tech{calls}.
@tech{Calls} must call a statically known procedure with exactly the right
number of arguments.
Otherwise, our desired transformation will leave some @ch2-tech{physical
locations} uninitialized, resulting in undefined behaviour.

We now have two data types exposed in the source language: integers, and
procedures.
This introduces more opporutnities for undefined behaviour.
If we try to compare a procedure to an integer, we will generate code with
undefined behaviour.
We therefore must check the source program does not use labels in this way.

To validate these assumptions, we implement @racket[check-values-lang].


@racket[check-values-lang] must necessarily rule out many well-defined programs,
since without modifying the source language, we cannot tell the types of all
procedure parameters.
We design a conservative approximation that is sound, @ie it never accepts a
program with undefined behaviour, but incomplete, @ie some well-defined programs
are rejected.
This is a normal trade off in compilers, and yet another consequence of Rice's
Theorem.

We use the following heuristics to implement @racket[check-values-lang]:
@itemlist[
  @item{A procedure's name can only appear in application position of a
  @values-lang-v5[call], or bound in the right-hand side of a
  @values-lang-v5[let].}
  @item{The parameters to a procedure are assumed to be integers.}
  @item{A call @values-lang-v5[(call x triv ...)] is only well typed if @racket[x]
  is bound to a procedure with @racket[n] @tech{parameters} and there are exactly
  @racket[n] @tech{arguments} in @values-lang-v5[call].}
  @item{A binary operation @values-lang-v5[(binop triv_1 triv_2)] is only well
  typed, and has type integer, if both @values-lang-v5[triv_1] and
  @values-lang-v5[triv_2] have type integer.}
  @item{A relational operation @values-lang-v5[(relop triv_1 triv_2)] is only well
  typed if both @values-lang-v5[triv_1] and @values-lang-v5[triv_2] have type
  integer.}
  @item{An @values-lang-v5[if] expression @values-lang-v5[(if pred tail_1
  tail_2)] is only well typed if @values-lang-v5[pred] is a well-typed
  predicate.}
  @item{Every procedure must return an @values-lang-v5[int64].}
]

Finally, we have one restriction imposed by the run-time system: the final
result of the program must be an @values-lang-v5[int64].

@nested[#:style 'inset
@defproc[(check-values-lang (p Values-lang-v5?))
          Values-lang-v5?]{
Validates that the @tech{Values-lang v5} is syntactically well-formed, well
bound and well typed: all procedure calls pass the correct number of arguments,
and all @values-lang-v5[binop] and @values-lang-v5[relop] are never used with
labels.
You may want to separet this into two problems: first checking syntax, then type
checking.
}
]

Next we need to resolve @ch3-tech{lexical identifiers}.
This is slightly complicated by introducing @tech{procedures}.
We want to compile @tech{procedures} to labeled blocks and jumps, so we need to
compile their names to labels rather than @ch2-tech{abstract locations}.

First, we design @deftech{Values-unique-lang v5}.
We typeset the differences compared to @tech{Values-lang v5}.

@bettergrammar*-diff[values-lang-v5 values-unique-lang-v5]

As usual, we change local @ch3-tech{lexical identifiers} to @ch2-tech{abstract
locations}.
However, we also change top-level @ch3-tech{lexical identifiers} into
@values-unique-lang-v5[label]s.
@values-unique-lang-v5[label]s are @values-unique-lang-v5[triv]s, although we
assume they are only used according to the typing rules imposed by
@racket[check-values-lang].

@nested[#:style 'inset
@defproc[(uniquify (p Values-lang-v5?))
         Values-unique-lang-v5?]{
Compiles @tech{Values-lang v5} to @tech{Values-unique-lang v5} by resolving
top-level @ch3-tech{lexical identifiers} into unique labels, and all other
@ch3-tech{lexical identifiers} into unique @ch2-tech{abstract locations}.
}
]

Next we design @deftech{Proc-imp-mf-lang v5}, an imperative language in monadic
form with procedures.
We typeset the differences compared to @tech{Values-unique-lang v5}.

@bettergrammar*-diff[values-unique-lang-v5 proc-imp-mf-lang-v5]

There are no interesting changes.
We simply propagate the new procedure forms form down one more level of abstraction.
@todo{Can I omit these languages and passes, for completely boring unchanged
things, from the main chapter?}

@nested[#:style 'inset
@defproc[(sequentialize-let (p values-unique-lang-v5?))
         proc-imp-mf-lang-v5?]{
Compiles @tech{Values-unique-lang v5} to @tech{Proc-imp-mf-lang v5} by picking a
particular order to implement @values-unique-lang-v5[let] expressions using
@proc-imp-mf-lang-v5[set!].
}
]

@section{Implementing Calling Conventions}

Now we can design @deftech{Imp-mf-lang v5}.
The design follows the needs of the translation we designed in @Secref{design-convention-translation}.

@bettergrammar*-diff[proc-imp-mf-lang-v5 imp-mf-lang-v5]

@todo{Probably want to move that module info field into the blocks earlier, and
regularize definitions. But that requires introducing an order on definitions,
which is annoying ....}

We remove the @proc-imp-mf-lang-v5[call] form and replace it by the
@imp-mf-lang-v5[jump] form.
As described in @Secref{design-convention-translation}, all @tech{calls} are
compiled to a sequence of @imp-mf-lang-v5[set!]s moving the @tech{arguments}
followed by a @imp-mf-lang-v5[jump], and all @tech{procedure} definitions are
compiled to a block that assigns the @tech{parameters}, as directed by the
calling convention.

Note that we now require @tech{physical locations} in the target language, so we
must gradually expose @ch2-tech{physical locations} up to this language from the rest
of the compiler.

@nested[#:style 'inset
@defproc[(impose-calling-conventions (p proc-imp-mf-lang-v5))
         imp-mf-lang-v5.p?]{
Compiles @tech{Proc-imp-mf-lang v5} to @tech{Imp-mf-lang v5} by imposing calling
conventions on all calls and procedure definitions.
The parameter registers are defined by the list
@racket[current-parameter-registers].
}
]

Finally, we design @deftech{Imp-cmf-lang v5}.

@bettergrammar*-diff[imp-mf-lang-v5 imp-cmf-lang-v5]

There are no interesting changes.

@nested[#:style 'inset
@defproc[(canonicalize-bind (p imp-mf-lang-v5.p))
          imp-cmf-lang-v5.p]{
Compiles @tech{Imp-mf-lang v5} to @tech{Imp-cmf-lang v5}, pushing
@imp-mf-lang-v5[set!] under @imp-mf-lang-v5[begin] so that the right-hand-side
of each @imp-mf-lang-v5[set!] is simple value-producing operation.

This canonicalizes @tech{Imp-mf-lang v5} with respect to the equations:
@tabular[
(list
(list
@imp-mf-lang-v5[(set! aloc (begin effect_1 ... value))]
"="
@imp-mf-lang-v5[(begin effect_1 ... (set! aloc value))])
(list
@imp-mf-lang-v5[(set! aloc (if pred value_1 value_2))]
"="
@imp-mf-lang-v5[(if pred (set! aloc value_1) (set! aloc value_2))]))
]
}
]

@section{Exposing Jumps}
Implementing our calling convention requires exposing jumps quite high in the
compiler pipeline, in @tech{Imp-mf-lang v5}, while in the previous chapter we
hid jumps behind an abstraction boundary in @ch4-tech{Block-pred-lang v4}.
Thankfully, it is not very difficult to propagate jumps up the compiler
pipeline.
The main challenge is in adjusting the register allocator, but we have already
done the design work to simplify that.

First, we design @deftech{Asm-pred-lang v5}, the target of our
@racket[select-instructions] pass.
To see how to extend @racket[select-instructions], we should view the
difference, we typeset the differences compared to @ch4-tech{Asm-pred-lang v4}.

@bettergrammar*-diff[asm-pred-lang-v4 asm-pred-lang-v5]

The main difference is in the addition of the @racket[jump] instruction.
Note that the "arguments" to the jump are not part of meaning of the
instruction; they are just metadata used later by the compiler.

@nested[#:style 'inset
@defproc[(select-instructions (p imp-cmf-lang-v5.p))
         asm-pred-lang-v5.p]{
Compiles @tech{Imp-cmf-lang v5} to @tech{Asm-pred-lang v5}, selecting
appropriate sequences of abstract assembly instructions to implement the
operations of the source language.
}
]

@subsection{Extending Register Allocation}

First, we extend @racket[uncover-locals] to analyze jumps.
We design the administrative language @deftech{Asm-pred-lang v5/locals}
(Asm-pred-lang v5 with locals) below.
Note that the only difference is in the specification of the
@asm-pred-lang-v5[info] field.

@bettergrammar*-diff[#:include (info p) asm-pred-lang-v5 asm-pred-lang-v5/locals]

Note that because the source language now includes blocks, we need to perform
the local analysis over each block.
Each block gets its own @asm-pred-lang-v5[info] field, with its own locals set.
The locals set for the initial tail of the module is stored in the module's info
field.

@nested[#:style 'inset
@defproc[(uncover-locals (p asm-pred-lang-v5.p?))
         asm-pred-lang-v5/locals.p?]{
Compiles @tech{Asm-pred-lang v5} to @tech{Asm-pred-lang v5/locals}, analysing
which @ch2-tech{abstract locations} are used in each block, and each block and the
module with the set of variables in an @racket[info?] fields.
}
]

Now our undead algorithm must change to analyze jumps.
@deftech{Asm-pred-lang v5/undead} defines the output of @racket[undead-analysis].

@bettergrammar*-diff[#:include (info tail) asm-pred-lang-v5/locals asm-pred-lang-v5/undead]

When analyzing a @asm-pred-lang-v5[jump] statement, we need to compute its
@ch-ra-tech{undead-out set}.

@todo{Could be a design digression instead}
In general, this is difficult.
In general, we may not know the destination of the jump, so we would either have
to conservatively approximate and say "anything could be live, @ie everything is
undead", or analyze the control flow of the program, following jumps and
analyzing the destination.
@todo{reference to alternative chapter 4?}

Thankfully, none of that is necessary.
Because jumps in our language only come from procedure calls, and our calling
convention translation decorated the jump with the locations used by the
procedure call, our undead analysis is trivial.
The @ch-ra-tech{undead-out set} of a jump statement @asm-pred-lang-v5[(jump triv_1
triv_2 ...)] is the set @racket[triv_2 ...].
@;We simply move this set into the @a3-tech{undead-set tree} for the jump instruction,
@;discarding it from the instruction in the process.
@;@todo{If I use Kent's fixpoint algorithm, this can't be discarded here.}

This requires no changes to the @ch-ra-tech{undead-set tree}.

Again, we also need to modify the analysis slightly to perform local analysis on
each block, and store @ch-ra-tech{undead-set trees} in the info field for the
corresponding block.

@nested[#:style 'inset
@defproc[(undead-analysis (p Asm-pred-lang-v5/locals?))
         Asm-pred-lang-v5/undead?]{
Performs undead analysis, compiling @tech{Asm-pred-lang v5/locals} to
@tech{Asm-pred-lang v5/undead} by decorating programs with their @ch-ra-tech{undead-set trees}.
}
]

Next we need to compute the conflict graph.

Below, we design @deftech{Asm-pred-lang v5/conflicts} below with structured
control-flow.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v5/undead asm-pred-lang-v5/conflicts]

The @racket[conflict-analysis] does not change significantly.
We simply extend the algorithm to support jump statements.
Note that @asm-pred-lang-v5[jump] only references but never defines an
@ch2-tech{abstract location}.

Again, the analysis should perform local analysis on each block separately.

@nested[#:style 'inset
@defproc[(conflict-analysis (p Asm-pred-lang-v5/undead?))
         Asm-pred-lang-v5/conflicts?]{
Performs conflict analysis, compiling @tech{Asm-pred-lang v5/undead}
to @tech{Asm-pred-lang v5/conflicts} by decorating programs with their conflict
graph.
}
]

The graph colouring register alloctor does not need major changes.
Below we define @deftech{Asm-pred-lang v5/assignments}, which only changes in
the @asm-pred-lang-v5[info] field as usual.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v5/conflicts asm-pred-lang-v5/assignments]

The allocator should run the same algorithm as before, but this time, on each
block separately.

@nested[#:style 'inset
@defproc[(assign-registers (p Asm-pred-lang-v5/conflicts?))
         Asm-pred-lang-v5/assignments?]{
Performs graph-colouring register allocation, compiling
@tech{Asm-pred-lang v5/conflicts} to @tech{Asm-pred-lang v5/assignments} by
decorating programs with their register assignments.
}
]

Finally, we actually replace @ch2-tech{abstract locations} with
@ch2-tech{physical locations}.

@bettergrammar*-diff[asm-pred-lang-v5/assignments nested-asm-lang-v5]

We need to extend the implementation to traverse each block, and support jump
statements.
In the process, we also discard the undead annotations on the jump instruction.

@nested[#:style 'inset
@defproc[(replace-locations [p asm-pred-lang-v5/assignments.p?])
         para-asm-lang-v5.p?]{
Compiles @tech{Asm-pred-lang v5/assignments} to @tech{Asm-pred-lang v5} by replacing all
@ch2-tech{abstract location} with @ch2-tech{physical locations} using the assignment
described in the @asm-pred-lang-v5[assignment] info field.
}
]

@subsection{Exposing Basic Blocks}
The last update need to make is to @racket[expose-basic-blocks].
We design the source, @deftech{Nested-asm-lang v5} below, typeset compared to
@ch4-tech{Nested-asm-lang v4}

@bettergrammar*-diff[nested-asm-lang-v4 nested-asm-lang-v5]

The main difference is the inclusion of @nested-asm-lang-v5[jump] expressions and
block definitions.
These do not complicate the process of exposing basic blocks much.
We simply need to traverse each block, exposing new blocks in the process.

Note that we again need to impose the convention that execution begins with the
first basic block, and move the initial @nested-asm-lang-v5[tail] into an explicit
block.

The target language is @deftech{Block-pred-lang v5}, typeset compared to
@ch4-tech{Block-pred-lang v4} below.

@bettergrammar*-diff[block-pred-lang-v4 block-pred-lang-v5]

@nested[#:style 'inset
@defproc[(expose-basic-blocks (p nested-asm-lang-v5?))
          block-pred-lang-v5]{
Compile the @tech{Nested-asm-lang v5} to @tech{Block-pred-lang v5}, eliminating
all nested expressions by generate fresh basic blocks and jumps.
}
]

Nothing else in the compiler needs to change.

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v5-graph" "Overview of Compiler Version 5" v5-graph]

@section{Appendix: Languages}

@declare-exporting[cpsc411/langs/v5]

@deflangs[
values-lang-v5
values-unique-lang-v5
proc-imp-mf-lang-v5
imp-mf-lang-v5
imp-cmf-lang-v5
asm-pred-lang-v5
asm-pred-lang-v5/locals
asm-pred-lang-v5/undead
asm-pred-lang-v5/conflicts
asm-pred-lang-v5/assignments
nested-asm-lang-v5
block-pred-lang-v5
]
