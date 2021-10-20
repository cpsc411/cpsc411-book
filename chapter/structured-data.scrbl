#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a8-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v7
  cpsc411/langs/v8
  (for-label cpsc411/langs/v8))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a8-solution]

@(define sb
   (make-cached-eval
    "ch8-eval"
    '(require racket/pretty cpsc411/reference/a8-solution cpsc411/compiler-lib)))

@define[v8-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

Lx [label="Exprs-lang v8"];
Ly [label="Exprs-unique-lang v8"];
Lz [label="Exprs-unsafe-data-lang v8"];
L0 [label="Exprs-bits-lang v8"];
L1 [label="Values-bits-lang v8"];
L2 [label="Proc-imp-mf-lang v8"];
L3 [label="Imp-mf-lang v8"];
L4 [label="Imp-cmf-lang v8"];
L5_1 [label="Asm-alloc-lang v8"];
L5 [label="Asm-pred-lang v8"];
L6 [label="Asm-pred-lang v8/locals"];
L7 [label="Asm-pred-lang v8/undead"];
L8 [label="Asm-pred-lang v8/conflicts"];
L81 [label="Asm-pred-lang v8/pre-framed"];
L82 [label="Asm-pred-lang v8/framed"];
L83 [label="Asm-pred-lang v8/spilled"];
L9 [label="Asm-pred-lang v8/assignments"];
L10 [label="Nested-asm-lang-fvars v8"];
L10_1 [label="Nested-asm-lang v8"];
L11 [label="Block-pred-lang v8"];
L12 [label="Block-asm-lang v8"];
L12_1 [label="Para-asm-lang v8"];
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
Ly -> Lz [label=" implement-safe-primops"];
Lz -> L0 [label=" specify-representation"];
L0 -> L1 [label=" remove-complex-opera*"];
L1 -> L2 [label=" sequentialize-let"];
L2 -> L3 [label=" impose-calling-conventions"]
L3 -> L4 [label=" canonicalize-bind"];
L4 -> L5_1 [label=" select-instructions"];
L5_1 -> L5 [label= " expose-allocation-pointer"];


L10 -> L10_1 [label=" implement-fvars"];
L10_1 -> L11 [label=" expose-basic-blocks"];
L11 -> L12 [label=" resolve-predicates"]
L12 -> L12_1 [label=" flatten-program"];
L12_1 -> L15_1 [label=" patch-instructions"];
L15_1 -> L16 [label=" implement-mops"];
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

  L15_1 [label="Paren-x64-mops v8"];
  L16 [label="Paren-x64 v8"];
  L17 [label="Paren-x64-rt v8"];
}

  L16 -> L17 [label=" link-paren-x64"];
  L17 -> L15 [label=" interp-loop"];
  L16 -> L15 [label=" interp-paren-x64"];
}
}
]

@title[#:tag "top" #:tag-prefix "chp-structured-data:"]{Data types: Structured Data and Heap Allocation}
@(define (v7-tech . rest)
  (apply tech #:tag-prefixes '("book:" "chp-immediates:") rest))


@section{Preface: What's wrong with Exprs-Lang v7}
@v7-tech{Exprs-lang v7} gained proper data types and algebraic expressions,
which is a huge step forward in expressivity and high-level reasoning.
However, it still does not allow us to express structured data.
Real languages require structured data---such as strings, vectors, and linked
lists---to express interesting programs over data larger than a single word.
Functional languages use procedures, a data structure, to provide functions as
first-class values.

To express data larger than a single word, we need support from the low-level
languages to get access to locations larger than a single word in size.
All our locations so far, registers and frame locations, are only a single word
in size.
We need access to @deftech{heap pointers}, memory locations whose size can be
arbitrary.

Our strategy is to add three forms that @racket[specify-representation] use
to create new data structures for its surface language.
These forms are:
@itemlist[
@item{@asm-alloc-lang-v8[(alloc e)] allocates a number of bytes specified by
@asm-alloc-lang-v8[e] and returns a pointer to the base address of those bytes.}
@item{@asm-alloc-lang-v8[(mref e_base e_index)] dereferences the pointer at
@asm-alloc-lang-v8[e_base] with the offset specified by @asm-alloc-lang-v8[e_index].
Thinking in terms of pointer arithmetic, this dereferences @asm-alloc-lang-v8[(+
e_base e_index)].
The value of @asm-alloc-lang-v8[(+ e_base e_index)] should always be word-aligned, @ie a multiple
of 8, and point to a an initialized heap allocated value.}
@item{@asm-alloc-lang-v8[(mset! e_base e_index e)] stores the value of @asm-alloc-lang-v8[e]
in the address @asm-alloc-lang-v8[(+ e_base e_index)], @ie in the address given by pointer at
@asm-alloc-lang-v8[e_base] with the offset specified by @asm-alloc-lang-v8[e_index].
The value of @asm-alloc-lang-v8[(+ e_base e_index)] should always be word-aligned, @ie a multiple
of 8.
}
]

To implement these new memory operations, or @deftech{mops} (pronounced
@emph{em ops}), we need to expose additional features from @ch1-tech{x64}.

@section{Exposing Heap Pointers in the Back-end}
@subsection{generate-x64}
We start by exposing a generalized @paren-x64-v8[addr] form in
@deftech{Paren-x64 v8} below, which will allow us to access arbitrary memory
locations.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpt)" "v8 Full")
(#:exclude (reg binop relop fbp int32 int64 label dispoffset) paren-x64-v7 paren-x64-v8)
(paren-x64-v8)
]

The language contains a new @paren-x64-v8[addr] representing the @ch1-tech{x64}
index-mode operand @paren-x64-v8[(reg + reg)].
This supports accessing a memory location by the index stored in another
register.
For example, in @ch1-tech{x64}, we represent loading the @emph{n}th element of an
array into @paren-x64-v8[r10] using @tt{mov r10 [r11 + r12]}, where the base of the
array is stored at @tt{r11} and the value of @emph{n} is stored in @tt{r12}.

The index-mode operand is not restricted to use a particular register, unlike
the displacement-mode operand from @v7-tech{Paren-x64 v7}.
We will use this feature to store pointers to structured data, and the register
allocator will move those pointers into whichever registers it chooses.

We also allow a generalized form of displacement-mode operand.
We can access the value pointed to by a base register @paren-x64-v8[reg] at
offset @paren-x64-v8[int32]. by @paren-x64-v8[(reg + int32)].
This allows optimizing heap accesses when a constant offset is known, which is
often the case for some data structures.
The index is not restricted to be a multiple of 8, but it should be the case in
our compiler that the value of the base plus the value of the offset is a
multiple of 8.

All languages with direct access to registers, including @tech{Paren-x64 v8},
are now parameterized by a new register,
@racket[current-heap-base-pointer-register] (abbreviated
@object-code{hbp}).
The run-time system initializes this register to point to the base of the heap.
Allocation is implemented by copying the current value of this pointer, and
incrementing it by the number of bytes we wish to allocate.
The pointer must only be incremented by word-size multiples of bytes.
Any other access to this register is now undefined behavior, similar to accesses
to @object-code{fbp} that do not obey the stack of frames discipline.

@digression{
A real language implementation might abstract access to the @tt{mmap}
system call for allocation, and implement a strategy (such as garbage
collection) to deallocate memory that is no longer used.
Garbage collection is tricky to implement and requires too much time for this
course, so we use a different strategy.
Our implementation of allocation is trivial, and does not support de-allocation.
We rely on the operating system to clean up memory after our process exits.

For a quick introduction to garbage collection, see this short video @url{https://twitter.com/TartanLlama/status/1296413612907663361?s=20}.
}

@defproc[(generate-x64 [s paren-x64-v8])
         string?]{
Compile the @tech{Paren-x64 v8} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}

@subsection{Em-Ops: Abstracting Memory Operations}
Like when we implemented @object-code{fvar}s to support working with frame
locations, we implement primitive memory operations, @tech{mops} , to simplify
working with heap addresses.
We should do this before @racket[patch-instructions] to avoid complicating the
already complex logic for rewriting @object-code{set!} instructions.

Below, we define @deftech{Paren-x64-mops v8}, with differences compared to @tech{Paren-x64-v8}.

@bettergrammar*-ndiff[
#:labels ("v8 Diff (excerpts)" "Full")
(#:include (p s addr index) paren-x64-v7 paren-x64-mops-v8)
(paren-x64-mops-v8)
]

We add two new instructions that directly map to operations on heap addresses,
either as index- or displacement-mode operands.
@margin-note{We could encode the restricted form of @paren-x64-mops-v8[addr],
used for frame variables, as an @paren-x64-mops-v8[mref], but the rest of our
compiler already knows about @paren-x64-mops-v8[addr]s, and it represents a
semantically different concept, so we leave it alone.}

@defproc[(implement-mops [p paren-x64-mops-v8?])
          paren-x64-v8?]{
Compiles @tech{mops} to instructions on pointers with index- and
displacement-mode operands.
}

Next we design @deftech{Para-asm-lang v8}.
Below, we give a definition.
@;However, your design may differ slightly since you've been responsible for the
@;design of @a6-tech{Paren-asm v6} and @a7-tech{Paren-asm v7}.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "source/target Diff" "Full")
(#:exclude (reg relop binop trg opand label int64) para-asm-lang-v7 para-asm-lang-v8)
(paren-x64-mops-v8 para-asm-lang-v8)
(para-asm-lang-v8)
]

By introducing @tech{mops}, we implicitly restrict how heap addresses appear in
the language and simplify the job of @racket[patch-instructions].
The @tech{mops} implicitly restrict heap addresses to being part of a move
instruction, so we do not have to patch binary operation instructions despite
apparently adding a new form of physical location.
By making them separate forms, we only need to patch the new instructions, and
leave old code untouched.

In @racket[patch-instructions], we also lift the restriction on
@para-asm-lang-v8[index], so @para-asm-lang-v8[int64]s can appear as an
@para-asm-lang-v8[index].
This makes @para-asm-lang-v8[index] and @para-asm-lang-v8[opand] coincide,
syntactically, but they are conceptually different so we maintain separate
non-terminal definitions.

@defproc[(patch-instructions [s para-asm-lang-v8])
         paren-x64-mops-v8]{
Patches instructions that have no @ch1-tech{x64} analogue into to a sequence of
instructions and an auxiliary register from
@racket[current-patch-instructions-registers].
}

@subsection{Exposing @tech{mops} up the pipeline}
The new @tech{mops} require minor changes to most of the pipeline up to
@tech{Asm-alloc-lang v8}, where we will use them to implement data structures.

@exercise{Redesign and extend the implementation of
@itemlist[
@item{@racket[flatten-program], should require no changes}
@item{@racket[resolve-predicates], should require no changes}
@item{@racket[expose-basic-blocks], should require no changes}
@item{@racket[implement-fvars], should require minor changes to support
@tech{mops}.
Note that we assume the @object-code{fbp} is not modified by @tech{mops}.}
@item{@racket[optimize-predicates], could require minor changes.}
@item{@racket[replace-locations], should require minor changes to support
@tech{mops}.}
@item{@racket[assign-frame-variables], should require no changes.}
@item{@racket[assign-registers], should require no changes.}
@item{@racket[allocate-frames], should require no changes.}
@item{@racket[assign-call-undead-variables], should require no changes.}
@item{@racket[conflict-analysis], should require minor changes.
Note that @tech{mops} do not @emph{assign} any registers or frame variables.}
@item{@racket[undead-analysis], should require minor changes.
Note that @tech{mops} do not @emph{assign} any registers or frame variables.}
@item{@racket[uncover-locals], should require minor changes.}
]}

@subsection{Implementing Allocation}
Before we introduce structured data, we implement the @asm-alloc-lang-v8[alloc]
instruction to allow programs to allocate a bunch of bytes and not worry about
the details of the allocation pointer.
We want to do this @emph{after} the passes that analyze physical locations,
since then we do not have to update those passes to know that
@asm-alloc-lang-v8[alloc] introduces a reference to a register.
However, we want to do this @emph{before} we abstract away from all machine
details so we do not need to expose registers beyond @tech{Asm-alloc-lang v8}.

We choose to insert this pass between @racket[uncover-locals] and
@racket[select-instructions].

Below, we design @deftech{Asm-alloc-lang v8}, the source language for this pass.
We typeset the differences compared to @v7-tech{Asm-pred-lang v7}.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "Full")
(#:exclude (binop relop int64 opand triv loc trg aloc label rloc) asm-pred-lang-v7 asm-alloc-lang-v8)
(asm-alloc-lang-v8)
]

@todo{Should not reuse index as the operand for alloc; it is semantically a
different concept, which happens to have the same representation.
}
@;Should probably abstract dispoffset to word-aligned bytes or something, then
@;reuse that.

We expose the earlier @tech{mops}, and add a new one, @asm-alloc-lang-v8[(set! loc
(alloc index))].
This is the low-level form of our allocation operation, which we will abstract
into an expression in @tech{Exprs-lang v8}.
In @asm-alloc-lang-v8[(set! loc (alloc index))], the @asm-alloc-lang-v8[index] is restricted
to be an @asm-alloc-lang-v8[int32] if it is an integer literal, for the same reasons
as the restriction on @asm-alloc-lang-v8[binop]s.
It must also be a multiple of a word size.
This requirement is partly from the operating system's @tt{mmap} (which will
usually ignore us if we violate the restriction and give us page-aligned memory
anyway), but mostly to ensure every pointer we get has @code{#b000} as its final
three bits so we can tag all pointers.

We design the target language, @deftech{Asm-pred-lang v8}, below.
This language removes the @asm-alloc-lang-v8[(alloc index)] form and is the
highest-level language parameterized by
@racket[current-heap-base-pointer-register].
This language, and all languages between it and @ch1-tech{x64}, assumes all
accesses to @paren-x64-v8[hbp] obey the restrictions described in
@tech{Paren-x64 v8}.

@bettergrammar*-ndiff[
#:labels ("Source/Target Diff (excerpts)" "v7 Diff (excerpts)" "Full")
(#:exclude (opand triv loc trg binop relop int64 int32 aloc label rloc) asm-alloc-lang-v8 asm-pred-lang-v8)
(#:exclude (opand triv loc trg binop relop int64 int32 aloc label rloc) asm-pred-lang-v7 asm-pred-lang-v8)
(asm-pred-lang-v8)
]

Intuitively, we will transform each @racket[`(set! ,loc (alloc ,index))] into
@racketblock[
`(begin
   (set! ,loc ,hbp)
   (set! ,hbp (+ ,hbp ,index)))
]

@defproc[(expose-allocation-pointer [p asm-alloc-lang-v8?])
         asm-pred-lang-v8?]{
Implements the allocation primitive in terms of pointer arithmetic on the
@racket[current-heap-base-pointer-register].
}

@subsection{Abstracting Mops}
Before we implement structured data, we expose our @tech{mops} through a few
layers of abstractions.
Below we design @deftech{Imp-cmf-lang v8} with support for
@tech{mops}.
We typeset the differences compared to @v7-tech{Imp-cmf-lang v7}.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "Source/Target Diff" "Full")
(#:exclude (opand triv loc trg info frame binop relop int64 aloc label rloc) imp-cmf-lang-v7 imp-cmf-lang-v8)
(#:exclude (opand triv loc trg info frame binop relop int64 aloc label rloc)
imp-cmf-lang-v8 asm-alloc-lang-v8)
(imp-cmf-lang-v8)
]

We add value forms of @imp-cmf-lang-v8[mref] and @imp-cmf-lang-v8[alloc].
We require these forms are used in a well-typed way.
This is a simple extension.

@defproc[(select-instructions [p imp-cmf-lang-v8?])
         asm-alloc-lang-v8?]{
Selects appropriate sequences of abstract assembly instructions to implement the
operations of the source language.
}

Next we design @deftech{Imp-mf-lang v8} with support for @tech{mops}.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "Full")
(#:exclude (opand triv loc trg binop relop int64 aloc label rloc) imp-mf-lang-v7 imp-mf-lang-v8)
(imp-mf-lang-v8)
]

We introduce the @imp-mf-lang-v8[value] context, but notice that the index
position for an @imp-mf-lang-v8[mset!] instruction is still restricted.

@question{Why can't (or shouldn't) we allow the index position to also be a
@imp-mf-lang-v8[value]?}

@defproc[(canonicalize-bind [p imp-mf-lang-v8?])
         imp-cmf-lang-v8?]{
Pushes @imp-mf-lang-v8[set!] and @imp-mf-lang-v8[mset!] under @imp-mf-lang-v8[begin] and
@imp-mf-lang-v8[if] so that the right-hand-side of each is simple
value-producing operand.

This canonicalizes @tech{Imp-mf-lang v8} with respect to the equations
@tabular[
(list
 (list
  @imp-mf-lang-v8[(set! loc (begin effect_1 ... value))]
  "="
  @imp-mf-lang-v8[(begin effect_1 ... (set! loc value))])
 (list
  @imp-mf-lang-v8[(set! loc (if pred value_1 value_2))]
  "="
  @imp-mf-lang-v8[(if pred (set! loc value_1) (set! loc value_2))])
 (list
  @imp-mf-lang-v8[(mset! loc opand (begin effect_1 ... value))]
  "="
  @imp-mf-lang-v8[(begin effect_1 ... (mset! loc opand value))])
 (list
  @imp-mf-lang-v8[(mset! loc opand (if pred value_1 value_2))]
  "="
  @imp-mf-lang-v8[(if pred (mset! loc opand value_1) (mset! loc opand value_2))])
 )
]
}

The @racket[impose-calling-conventions] pass requires only minor changes.

For @racket[sequentialize-let], we need to design an
@values-bits-lang-v8[effect] context in order to expose
@values-bits-lang-v8[mset!].
We design @deftech{Values-bits-lang v8} to include a few imperative features.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "Source/Target Diff (Excerpts)" "Full")
(#:exclude (opand triv binop relop int64 aloc label) values-bits-lang-v7 values-bits-lang-v8)
(#:exclude (opand triv binop relop int64 aloc label) values-bits-lang-v8 proc-imp-mf-lang-v8)
(values-bits-lang-v8)
]

We add an @values-bits-lang-v8[effect] context to support
@values-bits-lang-v8[mset!], and a @values-bits-lang-v8[begin] expression for
convenience.
Previously, all expressions in the Values-lang languages were @emph{pure}---they
evaluated and produced the same value regardless of in which order expressions
were evaluated.
We could freely reorder expressions, as long as we respected scope.
Now, however, @values-bits-lang-v8[mset!] modifies memory during its execution.
It not safe to reorder expressions after an @values-bits-lang-v8[mset!].
Furthermore, @values-bits-lang-v8[mset!] does not return a useful value.

To deal with this, we introduce a contextual distinction in the language.
We add the nonterminal @values-bits-lang-v8[effect] to represent an impure computation.
A @values-bits-lang-v8[effect] represents an expression that does not have a value, and is
executed only for its effect.
We can use @values-bits-lang-v8[effect] in certain expression contexts using
@values-bits-lang-v8[begin].
If we're already in an impure context, that is, in a
@values-bits-lang-v8[effect], then we can freely nest other
@values-bits-lang-v8[effect]s.

This contextual distinction is similar to the one we introduce to distinguish
tail calls from non-tail calls.

Despite the conceptually complex change to the language, the transformation is
still straightforward.

@digression{Now that effects can appear on the right-hand side of a
@values-bits-lang-v8[let] expression, it MAY not longer be safe to reorder them.
This is a design choice: we could make it clear to the programmer that
@values-bits-lang-v8[let] does not guarantee a particular order of evaluation
for its bindings, but then effects on the right-hand side lead to undefined
behaviour.
Or, we could impose a particular order, such as left-to-right, forbidding an
optimization.
A middle ground is to impose such an order only if any effects are detected in
the right-hand side of a @values-bits-lang-v8[let] (or rather, if we can
guarantee no effects are present, because Rice still does not let us know for
sure).
}

@defproc[(sequentialize-let [s values-bits-lang-v8?])
          proc-imp-mf-lang-v8?]{
Picks a particular order to implement @values-bits-lang-v8[let] expressions
using @imp-mf-lang-v8[set!].
}

Finally, we enable arbitrary nesting in value position.
We design @deftech{Exprs-bits-lang v8/contexts} below.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "Full")
(#:exclude (triv binop aloc label relop int64)
 exprs-bits-lang-v7/contexts exprs-bits-lang-v8/contexts)
(exprs-bits-lang-v8/contexts)
]

Supporting @exprs-bits-lang-v8/contexts[effect] context requires paying
attention to order when designing @racket[remove-complex-opera*], but does not
significantly complicate anything.

@defproc[(remove-complex-opera* [p exprs-bits-lang-v8/contexts?])
         values-bits-lang-v8?]{
Performs the monadic form transformation, unnesting all non-trivial operators
and operands, making data flow explicit and and simple to implement imperatively.
}

@subsection{Implementing Structured Data}
Now we have all the abstractions necessary to implement structured data.

We design a new @deftech{Exprs-unsafe-data-lang v8} below.
The language is large, as we include several new structured data types and their
primitives.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "Full")
(#:exclude (v aloc label fixnum uint8 ascii-char-literal)
  exprs-unsafe-data-lang-v7 exprs-unsafe-data-lang-v8)
(exprs-unsafe-data-lang-v8)
]

We add new primops for each of our new data types.
Since the number of primitive operations is growing, we simplify the syntax to
only give @exprs-unsafe-data-lang-v8[primops], rather than distinguishing
@exprs-unsafe-data-lang-v8[unops], @exprs-unsafe-data-lang-v8[binops], and so on, so we can easily group
like primops with like.

We also add impure computations, since vectors are a mutable data structure.
Only effectful @exprs-unsafe-data-lang-v8[primops], those ending with @tt{!}, are allowed in
effect context.

We add two heap-allocated data types, described below:
@itemlist[
@item{@deftech{Pairs} are constructed using @exprs-unsafe-data-lang-v8[(cons e_1 e_2)].
The predicate @exprs-unsafe-data-lang-v8[pair?] should return @exprs-unsafe-data-lang-v8[#t] when passed any
value constructed this way, and #f for any other value---@exprs-unsafe-data-lang-v8[(eq?
(pair? (cons e_1 e_2)) #t)].
@exprs-unsafe-data-lang-v8[(unsafe-car e)] returns the value of the first element of the pair,
and @exprs-unsafe-data-lang-v8[(unsafe-cdr e)] returns the value of the second element.
That is, @exprs-unsafe-data-lang-v8[(eq? (unsafe-car (cons e_1 e_2)) e_1)] and
@exprs-unsafe-data-lang-v8[(eq? (unsafe-cdr (cons e_1 e_2)) e_2)].
}
@item{@deftech{Vectors} are essentially arrays that know their length.
They are constructed using @exprs-unsafe-data-lang-v8[(unsafe-make-vector e)]; the constructor
takes the length of the vector as the argument.
The predicate @exprs-unsafe-data-lang-v8[vector?] should return @exprs-unsafe-data-lang-v8[#t] for any value
constructed this way, and #f for any other value---@exprs-unsafe-data-lang-v8[(eq? (vector?
(unsafe-make-vector e)) #t)].
@exprs-unsafe-data-lang-v8[(unsafe-vector-ref e_1 e_2)] returns the value at index
@exprs-unsafe-data-lang-v8[e_2] in the vector @exprs-unsafe-data-lang-v8[e_1].
@exprs-unsafe-data-lang-v8[(unsafe-vector-set! e_1 e_2 e_3)] mutates the index
@exprs-unsafe-data-lang-v8[e_2] in the vector @exprs-unsafe-data-lang-v8[e_1], setting its value to the
value of @exprs-unsafe-data-lang-v8[e_3].

@exprs-unsafe-data-lang-v8[(unsafe-vector-set! e_1 e_2 e_3)] is only allowed in impure
computation context.
}
]

As we're adding new data types, we need new tags.
Here is our updated list of tags:
Here is the set of tags we will use in this assignment, given in base 2.
@itemlist[
@item{@code{#b000}, @deftech{fixnums}, fixed-sized integers}
@item{@code{#b001}, @tech{pairs}}
@item{@code{#b010}, @emph{unused}}
@item{@code{#b011}, @tech{vectors}}
@item{@code{#b100}, @emph{unused}}
@item{@code{#b101}, @emph{unused}}
@item{@code{#b110}, non-fixnum immediates (booleans, etc)}
@item{@code{#b111}, @emph{unused}}
]

We follow the same pattern as @secref[#:tag-prefixes '("book:"
"chp-immediates:")]{top} to implement the new predicates.

To implement constructors, we need to compile to @exprs-bits-lang-v8/contexts[alloc].
@exprs-unsafe-data-lang-v8[cons] allocates two words of space, storing its first argument in
the first word and the second element in the second word, producing
@racket[`(alloc ,(current-pair-size))].
The @a7-tech{ptr} we get back needs to be tagged, so we produce
@racket[`(bitwise-ior (alloc ,(current-pair-size)) ,(current-pair-tag))].
@;@exprs-unsafe-data-lang-v8[make-procedure] also allocates two words, one for the label and one
@;for the argument arity.
@exprs-unsafe-data-lang-v8[unsafe-make-vector] allocates one word for the length, and then one
word for every element of the vector.
That is, it should allocate @tt{n+1} words for a vector of length @tt{n}.

After allocating, we initialize the data structures using
@exprs-bits-lang-v8/contexts[mset!].
For example, we would transform @racket[`(cons ,e_1 ,e_2)].
@racketblock[
`(let ([x.1 (bitwise-ior (alloc 16) ,(current-pair-tag))])
   (begin
     (mset! (bitwise-xor x.1 ,(current-pair-tag)) 0 ,e_1)
     (mset! (bitwise-xor x.1 ,(current-pair-tag)) 8 ,e_2)
     x.1))
]

Since the length of the vector is dynamically determined, we do not initialize
each field when implementing its constructor.
Instead, we expose an unsafe constructor for vectors, and leave it to a safer
language to initialize the vector.

We can optimize memory operations to avoid masking the pointer by taking
advantage of pointer arithmetic.
For example, @exprs-bits-lang-v8/contexts[(bitwise-ior (alloc 16) #b001)] is the same as
@exprs-bits-lang-v8/contexts[(+ (alloc 16) 1)].
We can therefore adjust the index by -1 to access the base of the pointer,
instead of masking the pointer.
Performing this optimization for pairs, we would instead transform
@racket[`(cons ,e_1 ,e_2)] into
@racketblock[
`(let ([x.1 (+ (alloc 16) 1)])
   (begin
     (mset! x.1 -1 ,e_1)
     (mset! x.1 7 ,e_2)
     x.1))
]
The same optimization holds for vectors with different constants.

@defproc[(specify-representation [p exprs-unsafe-data-lang-v8?])
         exprs-bits-lang-v8/contexts?]{
Compiles data types and primitive operations into their implementations as
@v7-tech{ptrs} and primitive bitwise operations on @v7-tech{ptrs}.
}

@subsection{New Safe Primops}
@; Don't know how to do this without closures.
@;Last week, the last piece of undefined behavior in our language was in procedure
@;calls.
@;We were not checking that procedures were correctly applied to the number of
@;expected arguments.
@;Lower down, when procedure calls are implemented in terms of register and frame
@;variables, this could result in dereferencing uninitialized locations.
@;We can use the procedure data type to eliminate this undefined behavior.
@;
@;This week, this pass is optional, as current events mean I haven't been able to
@;write up the design in time.
@;Lucky you.
@;
@;@challenge{Design and implement @racket[implement-safe-apply].
@;You should design the source language, and the target language is
@;@tech{Impure-Exprs-data-lang v8}.
@;}
@;
@;@subsection{implement-safe-primops}
All the accessors for the new data types can result in undefined behaviour if
used on the wrong @v7-tech{ptr}.
Similarly, vector reference can access undefined values if the vector is
constructed but never initialized.

Below we define @deftech{Exprs-unique-lang v8}.

@bettergrammar*-ndiff[
#:labels ("v7 Diff (excerpts)" "Full")
(#:exclude (aloc label fixnum uint8 ascii-char-literal)
 exprs-unique-lang-v7 exprs-unique-lang-v8)
(exprs-unique-lang-v8)
]

Note that in this language, we remove @exprs-unsafe-data-lang-v8[begin].
The user must manually call impure functions and bind the result.
The result of an effectful function could be @exprs-unique-lang-v8[void], or an
@exprs-unique-lang-v8[error].
It would be unwise, although technically safe, to simple discard errors.
@;However, it would complicate compilation---notice that none of the features in
@;the language correspond to an @exprs-unsafe-data-lang-v8[effect] in the target
@;language, so how would we compile @exprs-unsafe-data-lang-v8[begin]?

To implement this safe language, we wrap all accessors to perform dynamic tag checking
before using the unsafe operations.
We also wrap @exprs-unsafe-data-lang-v8[unsafe-make-vector] to initialize all elements to
@racket[0].

Writing a compiler for the following specification language may simplify this
task:
@racketblock[
(code:comment "Symbol x Symbol x (List-of Parameter-Types)")
(code:comment "The first symbol is the name of a function in the source language.")
(code:comment "The second is either the name of a primop or a label in the target language implementing the")
(code:comment "behaviour safely, assuming well-typed parameters.")
(code:comment "The third is list of predicates, one for each argument to the source")
(code:comment "function, to check the parameters with. `any?` is specially recognized to")
(code:comment "not be checked.")
(define prim-f-specs
  `((* unsafe-fx* (fixnum? fixnum?))
    (+ unsafe-fx+ (fixnum? fixnum?))
    (- unsafe-fx- (fixnum? fixnum?))
    (< unsafe-fx< (fixnum? fixnum?))
    (<= unsafe-fx<= (fixnum? fixnum?))
    (> unsafe-fx> (fixnum? fixnum?))
    (>= unsafe-fx>= (fixnum? fixnum?))

    (make-vector ,make-init-vector-label (fixnum?))
    (vector-length unsafe-vector-length (vector?))
    (vector-set! ,unsafe-vector-set!-label (vector? fixnum? any?))
    (vector-ref ,unsafe-vector-ref-label (vector? fixnum?))

    (car unsafe-car (pair?))
    (cdr unsafe-cdr (pair?))

    ,@(map (lambda (x) `(,x ,x (any?)))
           '(fixnum? boolean? empty? void? ascii-char? error? pair?
                     vector? not))
    ,@(map (lambda (x) `(,x ,x (any? any?)))
           '(cons eq?))))
]
@;todo{Handling vector-set! required inlining a bit of context normalization}

All impure computations, those that end in @tt{!}, should only return
@exprs-unique-lang-v8[(void)] or an @exprs-unique-lang-v8[error].

@defproc[(implement-safe-primops [p exprs-unique-lang-v8])
         exprs-unsafe-data-lang-v8]{
Implement safe primitive operations by inserting procedure definitions for each
primitive operation which perform dynamic tag checking, to ensure type and memory safety.
}

@subsection{uniquify}
Finally, we define the source language.
Below we define @deftech{Exprs-lang v8}.

@bettergrammar*-diff[exprs-lang-v7 exprs-lang-v8]

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v8-graph" "Overview of Compiler Version 8" v8-graph]

@section{Appendix: Languages}

@declare-exporting[cpsc411/langs/v8]

@deflangs[
exprs-lang-v8
exprs-unique-lang-v8
exprs-unsafe-data-lang-v8
@;exprs-bits-lang-v8
exprs-bits-lang-v8/contexts
values-bits-lang-v8
proc-imp-mf-lang-v8
imp-mf-lang-v8
imp-cmf-lang-v8
asm-alloc-lang-v8
asm-pred-lang-v8
asm-pred-lang-v8/locals
asm-pred-lang-v8/undead
asm-pred-lang-v8/conflicts
asm-pred-lang-v8/pre-framed
asm-pred-lang-v8/framed
asm-pred-lang-v8/spilled
asm-pred-lang-v8/assignments
nested-asm-lang-fvars-v8
nested-asm-lang-v8
block-pred-lang-v8
block-asm-lang-v8
para-asm-lang-v8
paren-x64-mops-v8
paren-x64-v8
@;paren-x64-rt-v8
]
