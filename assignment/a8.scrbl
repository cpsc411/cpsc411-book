#lang reader "assignment-lang.rkt"
@(require cpsc411/deprecated/a8-compiler-lib)

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@;TODO abstractions for each of these; no more copy/paste/modify
@(define eg
   (make-cached-eval
    "a8-eval"
    '(require
      cpsc411/v1-reference/a8-solution
      cpsc411/deprecated/a8-compiler-lib
      racket/pretty)
    '(current-stack-size 512)))

@title[#:tag "top" #:tag-prefix "a8:"]{Compiler 8: Structured Data Types}

@section{Assignment Summary}

The goal of this assignment is to introduce memory allocation and structured
data.
We will add cons pairs, vectors, and procedures.
We will extend out tagged object representation to distinguished these data
types dynamically.

This assignment is due Friday, March 20, 2020 at 11:59pm.

You can use the reference solution here, but if you hammer it it might go down
and not come back up.
@url{https://www.students.cs.ubc.ca/~cs-411/2019w2/a8-interrogator.cgi}

A file with tests is available here: @share{a8-tests.rkt}.

@subsubsub*section{Assignment Checklist}
You should find a new repository in your
@url{https://github.students.cs.ubc.ca} account named @tt{a8_<team ids>} with a
code skeleton and a support library.
You should complete the assignment in that git repository and push it to the
GitHub Students instance.

You should first merge your solution to @secref[#:tag-prefixes '("a7:")]{top}
with the starter code provided.
The new starter code has the correct provides and includes a submodule to help
you run your compiler on the command line if you want.
The name of the skeleton is @share{a8-skeleton.rkt} to avoid accidentally
overwriting your files, but your file in the Git repository should be named
@tt{a8.rkt}.

@section{Language Diagram}

@dot->svg{
digraph {

node [ shape="box" ]

/* The Languages */

LX0 [label="Exprs-lang v8"]
LX1 [label="Impure-Exprs-safe-data-lang v8"]
LX2 [label="Impure-Exprs-data-lang v8"]
LX3 [label="Impure-Exprs-bits-lang v8"]
LX4 [label="Impure-Values-bits-lang v8"]
LX5 [label="Block-lang v8"];

LX5b [label="Block-hbp-lang v8"];

L3 [label="Block-locals-lang v8"];
L4 [label="Undead-block-lang v8"];
L5 [label="Conflict-lang v8"];
L5a1 [label="Pre-framed-lang v8"];
L5a2 [label="Framed-block-lang v8"];
L5b [label="Spilled-lang v8"];
L6 [label="Block-jump-live-lang v8"];
L7 [label="Block-assigned-lang v8"];
L7b [label="Block-fvar-lang v8"];
L8 [label="Block-nested-lang v8"];
L9 [label="Block-asm-lang v8"];
L10 [label="Paren-asm v8"];

L11a [label="Paren-x64-mops v8"];
L11b [label="Paren-x64 v8"];
L13 [label="x64"];


/* The Passes */

edge [fontname="Courier"]

LX0 -> LX1 [label=" uniquify"];
LX1 -> LX2 [label=" implement-safe-primops"];
/* CHALLENGE:
   LX1 -> XXX [label=" implement-safe-primops"]; 
   XXX -> LX2 [label=" implement-safe-apply"];
*/
LX2 -> LX3 [label=" specify-representation"];
LX3 -> LX4 [label=" a-normalize"];
LX4 -> LX5 [label=" select-instructions"];

LX5 -> LX5b [label=" expose-allocation-pointer"];

LX5b -> L3 [label=" uncover-locals"];

L3 -> L4 [label=" undead-analysis"];
L4 -> L5 [label=" conflict-analysis"];
L5 -> L5a1 [label=" pre-assign-frame-variables"];
L5a1 -> L5a2 [label=" assign-frames"];
L5a2 -> L5b [label=" assign-registers"];
L5b -> L6 [label=" assign-frame-variables"];
L6 -> L7 [label=" discard-call-live"];
L7 -> L7b [label=" replace-locations"];
L7b -> L8 [label=" implement-fvars"];
L8 -> L9 [label=" expose-basic-blocks"];
L9 -> L10 [label=" flatten-program"];

L10 -> L11a [label=" patch-instructions"];

L11a -> L11b [label="implement-mops"];
L11b -> L13 [label=" generate-x64"];
}
}

@section{Preface: What's wrong with Exprs-Lang v7}
@a7-tech{Exprs-lang v7} gained proper data types and algebraic expressions,
which is a huge step forward in expressivity and high-level reasoning.
However, it still does not allow us to express structured data.
This is greatly limits our expressivity.
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

Our strategy is to add three forms that @racket[specify-representation] will use
to create new data structures for its surface language.
These forms are:
@itemlist[
@item{@object-code{(alloc e)} allocates a number of bytes specified by
@object-code{e} and returns a pointer to the base address of those bytes.}
@item{@object-code{(mref e_base e_index)} dereferences the pointer at
@object-code{e_base} with the offset specified by @object-code{e_index}.
Thinking in terms of pointer arithmetic, this dereferences @tt{e_base + e_index}.
The value of @tt{e_base + e_index} should always be word-aligned, @ie a multiple
of 8, and point to a an initialized heap allocated value.}
@item{@object-code{(mset! e_base e_index e)} stores the value of @object-code{e}
in the address @tt{e_base + e_index}, @ie in the address given by pointer at
@object-code{e_base} with the offset specified by @object-code{e_index}.
The value of @tt{e_base + e_index} should always be word-aligned, @ie a multiple
of 8.
}
]

To implement these new memory operations, or @deftech{mops}, we need to expose
additional features from @a0-tech{x64}.

@section{Exposing Heap Pointers in the Back-end}
@subsection{generate-x64}
We start by exposing a generalized @object-code{addr} form in @deftech{Paren-x64
v8} below, which will allow us to access arbitrary memory locations.

@racketgrammar*[
[p     (begin s ...)]
[s     (set! loc triv)
       (set! reg loc)
       (set! reg_1 (binop reg_1 int32))
       (set! reg_1 (binop reg_1 loc))
       (define label s)
       (jump trg)
       (compare reg opand)
       (jump-if cmp label)]
[trg   reg label]
[triv  trg int64]
[opand int64 reg]
[loc   reg addr]
[reg   _...]
[addr  ((unsyntax @bnf:add{fbp}) + dispoffset) (unsyntax @bnf:add{(reg + int32)})
       (unsyntax @bnf:add{(reg + reg)})]
[binop * + - bitwise-and
       bitwise-ior
       bitwise-xor
       arithmetic-shift-right]
[cmp   neq? eq? < <= > >=]
]

The language contains a new @object-code{addr} representing the @a0-tech{x64}
index-mode operand @object-code{(reg + reg)}.
This supports accessing a memory location by the index stored in another
register.
For example, in @a0-tech{x64}, we represent loading the @emph{n}th element of an
array into @tt{r10} using @tt{mov r10 [r11 + r12]}, where the base of the
array is stored at @tt{r11} and the value of @emph{n} is stored in @tt{r12}.

The index-mode operand is not restricted to use a particular register, unlike
the displacement-mode operand from @a7-tech{Paren-x64 v7}.
We will use this feature to store pointers to structured data, and the register
allocator will move those pointers into whichever registers it chooses.

We also allow a generalized form of displacement-mode operand.
We can access the value of pointed to by base register @object-code{reg} at
offset @object-code{int32}. by @object-code{(reg + int32)}.
This allows optimizing heap accesses when a constant offset is known, which is
often the case for some data structures.
The index is not restricted to be a multiple of 8, but it should be the case in
our compiler that the value of the base plus the value of the offset is a
multiple of 8.

All languages with direct access to registers, including @tech{Paren-x64 v8},
are now parameterized by a new register,
@racket[current-heap-allocation-pointer-register] (abbreviated
@object-code{hbp}).
The run-time system initializes this register to point to the base of the heap.
Allocation is implemented by copying the current value of this pointer, and
incrementing it by the number of bytes we wish to allocate.
The pointer must only be incremented by word-size multiples of bytes.
Any other access to this register is now undefined behavior, similar to accesses
to @object-code{fbp} that do not obey the stack of frames discipline.

@digression{
A real language implementation would expose access to the @tt{mmap}
system call for allocation, and implement a strategy (such as garbage
collection) to deallocate memory that is no longer used.
Garbage collection is tricky to implement and requires too much time for this
course, so we use a different strategy.
Our implementation of allocation is trivial, and does not support de-allocation.
We rely on the operating system to clean up memory after our process exits.
}

@exercise{Redesign and extend the implementation of @racket[generate-x64] to
support index-mode operands.
The source language is @tech{Paren-x64 v8}, and output is a sequence of
@a0-tech{x64} instructions represented as a string.

This should be a one-line change if your compiler is well designed.
}

@subsection{implement-mops}
Like when we implemented @object-code{fvar}s to support working with frame
locations, we implement primitive memory operations, @tech{mops} (pronounced
@emph{em ops}), to simplify working with heap addresses.
We should do this before @racket[patch-instructions] to avoid complicating the
already complex logic for rewriting @object-code{set!} instructions.

Below, we define @deftech{Paren-x64-mops v8}.
@racketgrammar*[
[p     (begin s ...)]
[s     (set! loc triv)
       (set! reg loc)
       (set! reg_1 (binop reg_1 int32))
       (set! reg_1 (binop reg_1 loc))
       (unsyntax @bnf:add{(set! reg (mref reg index))})
       (unsyntax @bnf:add{(mset! reg index triv)})
       (define label s)
       (jump trg)
       (compare reg opand)
       (jump-if cmp label)]
[trg   reg label]
[triv  trg int64]
[opand int64 reg]
[loc   reg addr]
[(unsyntax @bnf:add{index}) int32 reg]
[addr  (fbp + dispoffset) (unsyntax @bnf:sub{(reg + reg)})]
[reg   _...]
[binop _...]
[cmp   _...]
]

We add two new instructions that directly map to operations on heap addresses,
either as index- or displacement-mode operands.
@margin-note{We could encode an @object-code{addr} using an @object-code{mref},
but the rest of our compiler already knows about @object-code{addr}s, and it
represents a semantically different concept, so we leave it alone..}

@exercise{Design and implement @racket[implement-mops].
The source language is @tech{Paren-x64-mops v8} and the target is
@tech{Paren-x64 v8}.
This should be a very simple compiler.
}

@subsection{patch-instructions}
@;@deftech{Block-mops-lang v8}
@;@racketgrammar*[
@;[p     (module b ... b)]
@;[b     (define label info tail)]
@;[info  (any ...)]
@;[tail  (begin s ... tail)
@;       (jump trg)
@;       (if (cmp rloc opand) tail tail)]
@;[s     (return-point label tail)
@;       (set! rloc triv)
@;       (set! rloc (binop rloc opand))
@;       (unsyntax @bnf:add{(set! rloc (mref rloc index))})
@;       (unsyntax @bnf:add{(mset! rloc index triv)})]
@;[triv  opand label]
@;[opand int64 rloc]
@;[trg   label rloc]
@;[rloc  reg addr]
@;[addr (fbp + dispoffset)]
@;[(unsyntax @bnf:add{index}) int32 rloc]
@;[reg   _...]
@;[binop _...]
@;[cmp   _...]
@;]

Next we design @deftech{Paren-asm v8}.
Below, we give a definition.
However, your design may differ slightly since you've been responsible for the
design of @a6-tech{Paren-asm v6} and @a7-tech{Paren-asm v7}.

@racketgrammar*[
[p     (begin s ...)]
[s     (set! loc triv)
       (set! loc (binop loc opand))
       (unsyntax @bnf:add{(set! loc (mref loc index))})
       (unsyntax @bnf:add{(mset! loc index triv)})
       (define label s)
       (jump trg)
       (compare loc opand)
       (jump-if cmp label)]
[triv  opand label]
[opand int64 loc]
[trg   label loc]
[loc   reg addr]
[addr (fbp + dispoffset)]
[(unsyntax @bnf:add{index}) int32 loc]
[binop _...]
[reg   _...]
[cmp   _...]
]
@;todo{triv is definitely not triv anymore}

By introducing @tech{mops}, we implicitly restrict how heap addresses appear in
the language and simplify the job of @racket[patch-instructions].
The @tech{mops} implicitly restrict heap addresses to being part of a move
instruction, so we do not have to patch binary operation instructions despite
apparently adding a new form of physical location.
By making them separate forms, we only need to patch the new instructions, and
leave old code untouched.

@exercise{Redesign and extend the implementation of @racket[patch-instructions].
The source language is @tech{Paren-asm v8} and the target language is
@tech{Paren-x64-mops v8}.

You only need to add two cases, and you can write them very systematically, but
they will take care to get right and cover all combinations.
}

@subsection{Exposing @tech{mops} up the pipeline}
The new @tech{mops} require minor changes to most of the pipeline up to
@tech{Block-lang v8}, where we will use them to implement data structures.

@exercise{Redesign and extend the implementation of
@itemlist[
@item{@racket[flatten-program], should require no changes}
@item{@racket[expose-basic-blocks], should require no changes}
@item{@racket[implement-fvars], should require minor changes to support
@tech{mops}.
Note that we assume the @object-code{fbp} is not modified by @tech{mops}.}
@item{@racket[replace-locations], should require minor changes to support
@tech{mops}.}
@item{@racket[discard-call-live], should require no changes.}
@item{@racket[assign-frame-variables], should require no changes.}
@item{@racket[assign-registers], should require no changes.}
@item{@racket[assign-frames], should require no changes.}
@item{@racket[pre-assign-frame-variables], should require no changes.}
@item{@racket[conflict-analysis], should require minor changes.
Note that @tech{mops} do not @emph{assign} any registers or frame variables.}
@item{@racket[undead-analysis], should require minor changes.
Note that @tech{mops} do not @emph{assign} any registers or frame variables.}
@item{@racket[uncover-locals], should require minor changes.}
]}

@subsection{expose-allocation-pointer}
Before we introduce structured data, we implement the @object-code{alloc}
instruction to allow programs to allocate a bunch of bytes and not worry about
the details of the allocation pointer.
We want to do this @emph{after} the passes that analyze physical locations,
since then we do not have to update those passes to know that
@object-code{alloc} introduces a reference to a register.
However, we want to do this @emph{before} we abstract away from all machine
details so we do not need to expose registers beyond @tech{Block-lang v8}.

We choose to insert this pass between @racket[uncover-locals] and
@racket[select-instructions].

Below, we design @deftech{Block-lang v8}, the source language for this pass.

@racketgrammar*[
[p     (module b ... b)]
[b     (define label info tail)]
[info  ((new-frames ((aloc ...) ...)))]
[tail  (begin s ... tail)
       (jump trg loc ...)
       (if (cmp loc opand) tail tail)]
[s     (return-point label tail)
       (set! loc triv)
       (set! loc (binop loc opand))
       (unsyntax @bnf:add{(mset! loc index triv)})
       (unsyntax @bnf:add{(set! loc (mref loc index))})
       (unsyntax @bnf:add{(set! loc (alloc index))})]
[triv  opand label]
[opand int64 loc]
[trg   label loc]
[loc   rloc aloc]
[rloc  reg fvar]
[(unsyntax @bnf:add{index}) int32 loc]
[reg   _...]
[binop _...]
[cmp   _...]
]

@todo{Should not reuse index as the operand for alloc; it is semantically a
different concept, which happens to have the same representation.
}
@;Should probably abstract dispoffset to word-aligned bytes or something, then
@;reuse that.

We expose the earlier @tech{mops}, and add a new one, @object-code{(set! loc
(alloc index))}.
This is the low-level form of our allocation operation, which we will abstract
into an expression in @tech{Exprs-lang v8}.
In @object-code{(set! loc (alloc index))}, the @object-code{index} is restricted
to be an @object-code{int32} if it is an integer literal, for the same reasons
as the restriction on @object-code{binop}s.
It must also be a multiple of a word size.
This requirement is partly from the operating system's @tt{mmap} (which will
usually ignore us if we violate the restriction and give us page-aligned memory
anyway), but mostly to ensure every pointer we get has @code{#b000} as its final
three bits so we can tag all pointers.

We design the target language, @deftech{Block-hbp-lang v8}, below.
This language removes the @object-code{alloc index} form and is the
highest-level language parameterized by
@racket[current-heap-base-pointer-register].
This language, and all languages between it and @a0-tech{x64}, assumes all
accesses to @object-code{hbp} obey the restrictions described in @tech{Paren-x64
v8}.

@racketgrammar*[
[p     (module b ... b)]
[b     (define label info tail)]
[info  ((new-frames ((aloc ...) ...)))]
[tail  (begin s ... tail)
       (jump trg loc ...)
       (if (cmp loc opand) tail tail)]
[s     (return-point label tail)
       (set! loc triv)
       (set! loc (binop loc opand))
       (unsyntax @bnf:add{(mset! loc index triv)})
       (unsyntax @bnf:add{(set! loc (mref loc index))})
       (unsyntax @bnf:sub{(set! loc (alloc index))})]
[triv  opand label]
[opand int64 loc]
[trg   label loc]
[loc   rloc aloc]
[rloc  reg fvar]
[index int32 loc]
[reg   _...]
[binop _...]
[cmp   _...]
]

Intuitively, we will transform each @racket[`(set! ,loc (alloc ,index))] into
@racketblock[
`(begin
   (set! ,loc ,hbp)
   (set! ,hbp (+ ,hbp ,index)))
]

@exercise{Design and implement the function @racket[expose-allocation-pointer].
The source language is @tech{Block-lang v8} and the target language is
@tech{Block-hbp-lang v8}.}

@subsection{select-instructions}

Below we design @deftech{Impure-Values-bits-lang v8} with support for
@tech{mops}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[(unsyntax @bnf:add{c}) (let ([aloc n]) c) (begin c ...) (mset! v v v)]
[e     n
       (let ([aloc n]) e)
       (if (cmp v v) e e)
       (unsyntax @bnf:add{(begin c ... e)})]
[n     v (binop v v) (apply v v ...) (unsyntax @bnf:add{(alloc v)}) (unsyntax @bnf:add{(mref v v)})]
[v     int64 label aloc]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[cmp   neq? eq? < <= > >=]
]

The language is an extension of @a7-tech{Values-bits-lang v7}.
We add value forms of @object-code{mref} and @object-code{alloc} to the
@object-code{n} nonterminal.
We require these forms are used in a well-typed way.
This is a simple extension.

However, supporting @object-code{mset!} requires that we introduce a notion of
effectful, or impure, computation into the language.
Previously, all expressions in the Values-lang languages were @emph{pure}---they
evaluated and produced the same value regardless of in which order expressions
were evaluated.
We could freely reorder expressions, as long as we respected scope.
Now, however, @object-code{mset!} modifies memory during its execution.
It not safe to reorder expressions after an @object-code{mset!}.
Furthermore, @object-code{mset!} does not return a useful value.

To deal with this, we introduce a contextual distinction in the language.
We add the nonterminal @object-code{c} to represent an impure computation.
A @object-code{c} represents an expression that does not have a value, and is
executed only for its effect.
We can use @object-code{c} in certain expression contexts using
@object-code{begin}.
If we're already in an impure context, that is, in a @object-code{c}, then we
can freely nest other @object-code{c}s.

This contextual distinction is similar to the one we introduce to distinguish
tail calls from non-tail calls.

Despite the conceptually complex change to the language, the transformation is
straightforward.
Previously, @racket[select-instructions] essentially implemented the pure
@a7-tech{Values-bits-lang v7} using the impure @a7-tech{Block-lang v7}.
To transform the effectful computations, we essentially splice them into the
effectful computations @racket[select-instructions] was already producing.
The new @tech{mops} @object-code{n} are already valid operands in the target
language, so nothing special is needed to deal with them.

@exercise{Redesign and extend the implementation of the function
@racket[select-instructions].
The source language is @tech{Impure-Values-bits-lang v8} and the target language
is @tech{Block-lang v8}.}

@margin-note{@racket[select-instructions] is getting to be a big pass.
It now does several things---flattens @object-code{let} into @object-code{set!},
implements calling conventions, and selects instructions to implement binary
operations on values.
If you haven't already, you might want to redesign it as several smaller
passes.}

@subsection{a-normalize}
Next we design @deftech{Impure-Exprs-bits-lang v8} with support for
@tech{mops}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[(unsyntax @bnf:add{c}) (begin c ...) (mset! e e e)]
[e     v
       (let ([aloc e]) e)
       (if (cmp e e) e e)
       (unsyntax @bnf:add{(begin c ... e)})
       (binop e e) (apply e e ...)
       (unsyntax @bnf:add{(alloc e)})
       (unsyntax @bnf:add{(mref e e)})]
[v     int64 label aloc]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[cmp   neq? eq? < <= > >=]
]

While we remove the distinction between expressions and values, we maintain the
contextual distinction between impure computations and pure expressions.

@exercise{Redesign and extend the implementation of the function
@racket[a-normalize].
The source language is @tech{Impure-Exprs-bits-lang v8} and the target language
is @tech{Impure-Values-bits-lang v8}.

Be careful not to reorder the effectful operations.}

@subsection{specify-representation}
Now we have all the abstractions necessary to implement structured data.

We design a new @deftech{Impure-Exprs-data-lang v8}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[(unsyntax @bnf:add{c})     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       (apply e e ...)
       (let ([aloc e]) e)
       (if e e e)
       (begin c ... e)]
[v     fixnum aloc label #t #f () (void) (error uint8) ascii-char-literal]
[primop unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
        unsafe-fx>=
        fixnum? boolean? empty? void? ascii-char? error? not
        (unsyntax @bnf:add{pair?})
        (unsyntax @bnf:add{procedure?})
        (unsyntax @bnf:add{vector?})

        (unsyntax @bnf:add{cons})
        (unsyntax @bnf:add{unsafe-car})
        (unsyntax @bnf:add{unsafe-cdr})

        (unsyntax @bnf:add{unsafe-make-vector})
        (unsyntax @bnf:add{unsafe-vector-length})
        (unsyntax @bnf:add{unsafe-vector-set!})
        (unsyntax @bnf:add{unsafe-vector-ref})

        (unsyntax @bnf:add{make-procedure})
        (unsyntax @bnf:add{unsafe-procedure-arity})
        (unsyntax @bnf:add{unsafe-procedure-label})]
]

We add new primops for each of our new data types.
Since the number of primitive operations is growing, we simplify the syntax to
only give @object-code{primops}, rather than distinguishing
@object-code{unops}, @object-code{binops}, and so on, so we can easily group
like primops with like.

We also add impure computations, since vectors are a mutable data structure.
Only effectful @object-code{primops}, those ending with @tt{!}, are allowed in
impure computations.

We add three new heap allocated data types, described below:
@itemlist[
@item{@deftech{Pairs} are constructed using @object-code{(cons e_1 e_2)}.
The predicate @object-code{pair?} should return @object-code{#t} when passed any
value constructed this way, and #f for any other value---@object-code{(eq?
(pair? (cons e_1 e_2)) #t)}.
@object-code{(unsafe-car e)} returns the value of the first element of the pair,
and @object-code{(unsafe-cdr e)} returns the value of the second element.
That is, @object-code{(eq? (unsafe-car (cons e_1 e_2)) e_1)} and
@object-code{(eq? (unsafe-cdr (cons e_1 e_2)) e_2)}.
}
@item{@deftech{Vectors} are essentially arrays that know their length.
They are constructed using @object-code{(unsafe-make-vector e)}; the constructor
takes the length of the vector as the argument.
The predicate @object-code{vector?} should return @object-code{#t} for any value
constructed this way, and #f for any other value---@object-code{(eq? (vector?
(unsafe-make-vector e)) #t)}.
@object-code{(unsafe-vector-ref e_1 e_2)} returns the value at index
@object-code{e_2} in the vector @object-code{e_1}.
@object-code{(unsafe-vector-set! e_1 e_2 e_3)} mutates the index
@object-code{e_2} in the vector @object-code{e_1}, setting its value to the
value of @object-code{e_3}.

@object-code{(unsafe-vector-set! e_1 e_2 e_3)} is only allowed in impure
computation context.
}
@item{A @deftech{procedure} is a data structure representing a value that can be
called as a function.
Essentially, it is a wrapper around labels so we can check applications.
Starting in this language, application must use a procedure instead of
referencing a label directly.
We construct a procedure using @object-code{(make-procedure e_1 e_2)}, where
@object-code{e_1} must evaluate to a label and @object-code{e_2} is the number
of expected arguments.
The predicate @object-code{procedure?} should return @object-code{#t} for any
value constructed this way, and #f for any other value---@object-code{(eq?
(procedure? (make-procedure e_1 e_2)) #t)}.
We extract the label of a procedure with @object-code{(unsafe-procedure-label
e_1)}, where @object-code{e_1} is a procedure.
We get the arity of a procedure with @object-code{(unsafe-procedure-arity e_1)},
where @object-code{e_1} is a procedure.
}
]

As we're adding new data types, we need new tags.
Here is our updated list of tags:
Here is the set of tags we will use in this assignment, given in base 2.
@itemlist[
@item{@code{#b000}, @deftech{fixnums}, fixed-sized integers}
@item{@code{#b001}, @tech{pairs}}
@item{@code{#b010}, @tech{procedures}}
@item{@code{#b011}, @tech{vectors}}
@item{@code{#b100}, @emph{unused}}
@item{@code{#b101}, @emph{unused}}
@item{@code{#b110}, non-fixnum immediates (booleans, etc)}
@item{@code{#b111}, @emph{unused}}
]

We follow the same pattern as @secref[#:tag-prefixes '("a7:")]{top} to implement
the new predicates.

To implement constructors, we need to compile to @object-code{alloc}.
@object-code{cons} allocates two words of space, storing its first argument in
the first word and the second element in the second word, producing
@racket[`(alloc ,(current-pair-size))].
The @a7-tech{ptr} we get back needs to be tagged, so we produce
@racket[`(bitwise-ior (alloc ,(current-pair-size)) ,(current-pair-tag))].
@object-code{make-procedure} also allocates two words, one for the label and one
for the argument arity.
@object-code{unsafe-make-vector} allocates one word for the length, and then one
word for every element of the vector.
That is, it should allocate @tt{n+1} words for a vector of length @tt{n}.

After allocating, we initialize the data structures using @object-code{mset!}.
For example, we would transform @racket[`(cons ,e_1 ,e_2)].
@racketblock[
`(let ([x.1 (bitwise-xor (alloc 16) 1)])
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
For example, @object-code{(bitwise-xor (alloc 16) #b001)} is the same as
@object-code{(+ (alloc 16) 1)}.
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
The same optimization holds for vectors and procedures, with different
constants.

@exercise{Redesign and extend the implementation of the function
@racket[specify-representation].
The source language is @tech{Impure-Exprs-data-lang v8} and the target language
is @tech{Impure-Exprs-bits-lang v8}.

Remember to not use constant values and instead use parameters.
Some values may change.

Remember that the input language uses fixnum @a7-tech{ptrs} for all inputs, but the
output uses bytes for @tech{mops}.
}

@subsection{implement-safe-apply}
Last week, the last piece of undefined behavior in our language was in procedure
calls.
We were not checking that procedures were correctly applied to the number of
expected arguments.
Lower down, when procedure calls are implemented in terms of register and frame
variables, this could result in dereferencing uninitialized locations.
We can use the procedure data type to eliminate this undefined behavior.

This week, this pass is optional, as current events mean I haven't been able to
write up the design in time.
Lucky you.

@challenge{Design and implement @racket[implement-safe-apply].
You should design the source language, and the target language is
@tech{Impure-Exprs-data-lang v8}.
}

@subsection{implement-safe-primops}
All the accessors for the new data types can result in undefined behavior if
used on the wrong @a7-tech{ptr}.
Similarly, vector reference can access undefined values if the vector is
constructed but never initialized.

Below we define @deftech{Impure-Exprs-safe-lang v8}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[e     v
       (apply e e ...)
       (let ([aloc e]) e)
       (if e e e)]
[v     fixnum label prim-f aloc #t #f () (void) (error uint8) ascii-char-literal]
[prim-f * + - eq? < <= > >=
        fixnum? boolean? empty? void? ascii-char? error? not
        (unsyntax @bnf:add{pair?})
        (unsyntax @bnf:add{procedure?})
        (unsyntax @bnf:add{vector?})

        (unsyntax @bnf:add{cons})
        (unsyntax @bnf:add{car})
        (unsyntax @bnf:add{cdr})

        (unsyntax @bnf:add{make-vector})
        (unsyntax @bnf:add{vector-length})
        (unsyntax @bnf:add{vector-set!})
        (unsyntax @bnf:add{vector-ref})

        (unsyntax @bnf:add{procedure-arity})]
]

As in the last assignment, we wrap all accessors to perform dynamic tag checking
before using the unsafe operations.
We also wrap @object-code{unsafe-make-vector} to initialize all elements to
@racket[0].

We remove @object-code{make-procedure} and @object-code{procedure-label}, which
are used internally.
The surface programmer will only be able to define safe procedures using
@object-code{lambda}.
However, we do allow the user to dynamically test whether a value is a procedure
and how many arguments it takes.

Note that in this language, we've removed @object-code{begin}.
The user must manually call impure functions and bind the result.
The result of an effectful function could be @object-code{void}, or an
@object-code{error}.
It would be unwise, although technically safe, to simple discard errors.

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
    (vector-ref unsafe-vector-ref (vector? fixnum?))

    (car unsafe-car (pair?))
    (cdr unsafe-cdr (pair?))

    (procedure-arity unsafe-procedure-arity (procedure?))

    ,@(map (lambda (x) `(,x ,x (any?)))
           '(fixnum? boolean? empty? void? ascii-char? error? pair? procedure?
                     vector? not))
    ,@(map (lambda (x) `(,x ,x (any? any?)))
           '(cons eq?))))
]
@;todo{Handling vector-set! required inlining a bit of context normalization}

All impure computations, those that end in @tt{!}, should only return
@object-code{(void)} or an @object-code{error}.

@exercise{Redesign and extend the implementation of @racket[implement-safe-primops].
The source language is @tech{Impure-Exprs-safe-lang v8} and the target language
is @tech{Impure-Exprs-data-lang v8}, or your designed target language if you
completed the challenge problem.

Now that there are now more combinations of @object-code{primop}s and arguments
than are possible to encode in a single number.
You'll need to give up some precision in error messages.
}

@subsection{uniquify}
Finally, we define the source language.
Below we define @deftech{Exprs-lang v8}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define x (lambda (x ...) e))]
[e     v
       (apply e e ...)
       (let ([x e]) e)
       (if e e e)]
[v     fixnum x prim-f x #t #f () (void) (error uint8) ascii-char-literal]
[prim-f * + - eq? < <= > >=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        car
        cdr

        make-vector
        vector-length
        vector-set!
        vector-ref

        (unsyntax @bnf:add{procedure-arity})]
]

@exercise{Redesign and extend the implementation of @racket[uniquify].
The source language is @tech{Exprs-lang v8} and the target language
@tech{Impure-Exprs-safe v8}.}

@;  LocalWords:  lang rkt TODO eg eval behaviour pm url subsubsub tt ids Exprs
@;  LocalWords:  GitHub secref expressivity deftech itemlist alloc mref mset
@;  LocalWords:  ie addr Paren racketgrammar loc triv reg binop int trg opand
@;  LocalWords:  cmp unsyntax bnf fbp dispoffset ior xor neq eq emph mov hbp
@;  LocalWords:  mmap de fvar em ops reimpose rloc asm todo fvars pre undead
@;  LocalWords:  aloc racketblock effectful primop fixnum uint ascii fx cdr
@;  LocalWords:  ref arity primops fixnums immediates ptr ptrs init inlining
@;  LocalWords:  uniquify
