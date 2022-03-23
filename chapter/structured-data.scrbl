#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a8-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v7
  cpsc411/langs/v8
  (for-label cpsc411/langs/v8)
  (for-label cpsc411/langs/v7))

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
L3 [label="Imp-mf-lang v8"];
L2 [label="Proc-imp-cmf-lang v8"];
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
L1 -> L3 [label=" sequentialize-let"];
L3 -> L2 [label=" normalize-bind"];
L2 -> L4 [label=" impose-calling-conventions"]
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
@v7-tech{Exprs-lang v7} gained proper data types, which is a huge step forward
in expressivity and high-level reasoning.
However, it still does not allow us to express structured data.
Real languages require structured heap-allocated data---such as strings,
vectors, and linked lists---to express interesting programs over data larger
than a single word.
Functional languages use procedures, a data structure, to provide functions as
first-class values.

To express data larger than a single word, we need support from the low-level
languages to get access to locations larger than a single word in size.
All our locations so far, registers and frame locations, are only a single word
in size.
We need access to the heap---arbitrary unstructured and unrestricted memory locations.

Once we have the ability to allocate arbitrary space in heap memory, and
pointers to that memory, we can add structured data types.
We'll use the same tagging approach introduced in the last chapter to tag
pointers, so we can distinguish pointers to vectors vs pointers to lists,
perform dynamic checking, etc.

Since our low-level languages don't have any operations on memory address except
frame variables, we'll need to introduce new low-level abstraction.
Our run-time system will also need to provide access to the heap memory in some
way.

@section{Implementing Structured Data}
@;Now we have all the abstractions necessary to implement structured data.
@;
@;We design a new @deftech{Exprs-unsafe-data-lang v8} below.
@;The language is large, as we include several new structured data types and their
@;primitives.
@;
@;@bettergrammar*-ndiff[
@;#:labels ("v7 Diff (excerpts)" "Full")
@;(#:exclude (v aloc label fixnum uint8 ascii-char-literal)
@;  exprs-unsafe-data-lang-v7 exprs-unsafe-data-lang-v8)
@;(exprs-unsafe-data-lang-v8)
@;]

In this chapter, we'll design and implement two heap-allocated data types,
described below.
@itemlist[
@item{@deftech{Pairs} are constructed using @exprs-lang-v8[(cons e_1 e_2)].
The predicate @exprs-lang-v8[pair?] should return @exprs-lang-v8[#t] when passed any
value constructed this way, and @exprs-lang-v8[#f] for any other
value---@exprs-lang-v8[(eq? (pair? (cons e_1 e_2)) #t)].
@exprs-lang-v8[(car e)] returns the value of the first element of the pair,
and @exprs-lang-v8[(cdr e)] returns the value of the second element.

As usual, want to ensure safety in our source language.
That is, @exprs-lang-v8[(eq? (car (cons e_1 e_2)) e_1)] and
@exprs-lang-v8[(eq? (cdr (cons e_1 e_2)) e_2)], while
@exprs-lang-v8[(if (not (pair? e)) (eq? (cdr e) (error uint8)) #t)].
We do not need to worry about uninitialized values, since the only constructors
ensures all parts of the pair are initialized.
}
@item{@deftech{Vectors} are arrays that know their length.
They are constructed using @exprs-lang-v8[(make-vector e)]; the constructor
takes the length of the vector as the argument.
The predicate @exprs-lang-v8[vector?] should return @exprs-lang-v8[#t] for any value
constructed this way, and @exprs-lang-v8[#f] for any other
value---@exprs-lang-v8[(eq? (vector? (make-vector e)) #t)].
@exprs-lang-v8[(vector-ref e_1 e_2)] returns the value at index
@exprs-lang-v8[e_2] in the vector @exprs-lang-v8[e_1].
@exprs-lang-v8[(vector-set! e_1 e_2 e_3)] mutates the index
@exprs-lang-v8[e_2] in the vector @exprs-lang-v8[e_1], setting its value to the
value of @exprs-lang-v8[e_3], and returning @exprs-lang-v8[(void)]

To ensure safety, the appropriate arguments must be ensured to be vectors, the
index arguments must be positive integers, and @exprs-lang-v8[make-vector] or
@exprs-lang-v8[vector-ref] must work to ensure that we never get an
uninitialized value.
We'll implement these with dynamic checks and by making
@exprs-lang-v8[make-vector] initialize all values in the vector to 0.
}
]

These two are interesting, since one has a statically known size, while one has
a dynamically determined size.
Each of these properties requires attention in our compiler.
We could add more, but these two are enough to demonstrate the general approach
to compiling structured data.

As we're adding new data types, we need new tags.
These are two very commonly uses data types, so we assign them @ch7-tech{primary
tags}.
Here is our updated list of @ch7-tech{primary tags}:
@itemlist[
@item{@code{#b000}, @tech{fixnums}, fixed-sized integers}
@item{@code{#b001}, @tech{pairs}}
@item{@code{#b010}, @emph{unused}}
@item{@code{#b011}, @tech{vectors}}
@item{@code{#b100}, @emph{unused}}
@item{@code{#b101}, @emph{unused}}
@item{@code{#b110}, non-fixnum immediates (booleans, etc)}
@item{@code{#b111}, @emph{unused}}
]

To add immediate data, we needed three operations: tagging, untagging, and tag
checking.
We need all of these to implement structured data types and follow the same
pattern as @secref[#:tag-prefixes '("book:" "chp-immediates:")]{top} for them,
but we also need three additional operations: memory allocation, dynamically
computed memory assignment, and dynamically computed memory dereference.

First, to implement constructors, we need the ability to allocate memory.
For now, we'll assume the existance of some abstraction
@exprs-bits-lang-v8/contexts[alloc] that can do this (which we become
responsible for implementing later).
@exprs-lang-v8[cons] create a pair, so it should allocate two words of space by
producing @racket[`(alloc ,(current-pair-size))].
@exprs-lang-v8[make-vector] allocates one word for the length, and then one
word for every element of the vector.
That is, it should allocate @tt{n+1} words for a vector of length @tt{n}.

Second, to initialize and mutate structured data, we need the ability to assign
to a memory location that is dynamically computed.
With @ch2-tech{frame variables}, we added @ch2-tech{displacement mode operands},
which could statically offset from the frame base pointer (whose location is
statically known), and we could use this as an operand to a @paren-x64-v8[set!]
instruction.
However, for structured data, we need something more general.
We want the ability to (1) use an arbitrary location storing the base address,
since we are dynamically allocating structured data, and passing that pointer
around as a value, so the address itself could end up in any register or frame
variable and (2) a dynamically determined offset from that base address, since
we may want pass as an argument the index into vector.
This is quite different from the abstractions we used to add @ch2-tech{frame
variables}.

We'll assume a new abstraction, @exprs-bits-lang-v8[(mset! value value value)],
where the first operand is an arbitrary expression that evaluates to a base
pointer (and is assumed to be produced from @exprs-bits-lang-v8[alloc]), the
second operand is an expression that evaluates to an offset in bytes from that
base pointer, and the third operand evaluates to the value to be stored in the
memory location computed by adding the offset to the base.
As usual, we become responsible for implementing this abstraction at some
pointer further down the compiler pipeline.

Using this abstraction, we can implement our constructors as specified.
@exprs-lang-v8[cons] is meant to not only allocate, but also initialize the two
words it allocated.
For example, intuitively, we would transform @exprs-lang-v8[(cons value_1 value_2)].
@exprs-bits-lang-v8-block[
#:datum-literals (x.1)
(let ([x.1 (alloc 16)])
   (begin
     (mset! x.1 0 value_1)
     (mset! x.1 8 value_2)
     x.1))
]
First we allocate 2 words, then set the first word to contain the first value,
and the second word to contain the second value.

However, we must also tag the pointer as a pair, to create a @ch7-tech{ptr}.
Previously, we used a combination of shifting and bitwise-and to tag data.
This limited the range of our immediate data.
To tag pointers, we need to similar limit the range of addresses that we get
pointers to, to ensure some bits are available as tags.
This is relatively easy to achieve---most operating systems enable requesting
aligned pointers, ensuring the 3 low-order bits are 0.
We could also over-allocate, then find an address in the allocated range that is
aligned properly to consider our base.
We'll assume the run-time system is capable of giving us aligned pointers, so we
avoid wasting memory through over allocation.
However, this method means we get pointers whose high-order bits contain part of
the address, and low-order bits are already zero---we do not need to shift the
pointer.
Instead, to tag, we combine the pointer with the tag bits using
@exprs-bits-lang-v8[bitwise-ior] and untag by using
the @exprs-bits-lang-v8[bitwise-xor] on the tagged pointer and the tag, which
resets only the tag bits to 0.

For example, we generate an aligned pointer for pairs using
@exprs-bits-lang-v8[(alloc 16)].
The @ch7-tech{primary tag} for pairs is @racket[(unsyntax (current-pair-tag))].
We immediately tag the pointer, and anytime we access the pointer (for example,
to initialize the pair), we must untag it first.
For example, we would essentially implement @exprs-lang-v8[(cons value_1
value_2)] as:
@exprs-bits-lang-v8-block[
#:datum-literals (x.1)
(let ([x.1 (bitwise-ior (alloc 16) (unsyntax (current-pair-tag)))])
  (begin
    (mset! (bitwise-xor x.1 (unsyntax (current-pair-tag))) 0 value_1)
    (mset! (bitwise-xor x.1 (unsyntax (current-pair-tag))) 8 value_2)
    x.1))
]

We can optimize some memory operations to avoid masking the pointer by taking
advantage of pointer arithmetic.
For example, @exprs-bits-lang-v8/contexts[(bitwise-ior (alloc 16) #b001)] is the same as
@exprs-bits-lang-v8/contexts[(+ (alloc 16) 1)].
We can therefore adjust the index by -1 to access the base of the pointer,
instead of untagging the pointer.
Performing this optimization for pairs, we would instead implement
@exprs-lang-v8[(cons value_1 value_2)] into
@exprs-bits-lang-v8-block[
#:datum-literals (x.1)
(let ([x.1 (+ (alloc 16) 1)])
   (begin
     (mset! x.1 -1 value_1)
     (mset! x.1 7 value_2)
     x.1))
]
The same optimization holds for vectors with different constants.

Finally, we need some way to actually acess memory locations to implement the
destructors for our data types.
This requires similar functionality to @exprs-bits-lang-v8[mset!]---it needs to
take dynamically computed pointers and offsets in order to access a dynamically
computed memory address.
We'll assume some abstraction @exprs-bits-lang-v8[(mref value value)], where the
first operand is the pointer and the second operand is the offset, and which
returns the value stored at the pointer plus the offset.
Using this we could implement @exprs-lang-v8[(car value_1)] as
@exprs-bits-lang-v8[(mref value_1 -1)], using the previous optimization to avoid
the explicit untagging.

In summary, our strategy is to add three intermediate abstractions that can be
used to easily create new data structures for its surface language.

These forms are:
@itemlist[
@item{@exprs-bits-lang-v8[(alloc value)] allocates a number of bytes specified by
@exprs-bits-lang-v8[value] and returns a pointer to the base address of those bytes.}
@item{@exprs-bits-lang-v8[(mref value_base value_index)] dereferences the pointer at
@exprs-bits-lang-v8[value_base] with the offset specified by
@exprs-bits-lang-v8[value_index].
Thinking in terms of pointer arithmetic, this dereferences @exprs-bits-lang-v8[(+
value_base value_index)].
The value of @exprs-bits-lang-v8[(+ value_base value_index)] should always be
word-aligned, @ie a multiple of 8, and point to a uninitialized heap allocated
value.}
@item{@exprs-bits-lang-v8[(mset! value_base value_index value)] stores the value
of @exprs-bits-lang-v8[value] in the address @exprs-bits-lang-v8[(+ value_base
value_index)], @ie in the address given by pointer at @exprs-bits-lang-v8[value_base]
with the offset specified by @exprs-bits-lang-v8[value_index]. The value of
@exprs-bits-lang-v8[(+ value_base value_index)] should always be word-aligned, @ie a
multiple of 8.
}
]

To implement these new memory operations, or @deftech{mops} (pronounced
@emph{em ops}), we need to expose additional features from @ch1-tech{x64} and
from the run-time system.
Namely, we need low-level pointer operations that are different from those for
@ch2-tech{frame variables}, and we need run-time support for getting aligned
addresses from the operating system to enable tagging pointers efficiently.

We then reuse our object tagging approach from the previous chapter to tag,
untag, and tag check pointers to our new structured data types.

@section{Memory Allocation and Access in x64}
To implement the memory allocation and access abstractions we've just
introduced, we need additional support from @ch1-tech{x64} and from the run-time
system.

To access memory, we expose the @deftech{index-mode operand} from @ch1-tech{x64}.
It is written @tt{QWORD [reg_base + reg_offset]}, and refers to the memory
location whose base pointer is stored in the register @tt{reg_base} and whose
index is stored in the register @tt{reg_offset}.
Unlike @ch2-tech{displacement-mode operands}, both components of this operand
are registers, so both the base and offset can be computed dynamically.
We also enable a generalization of the @ch2-tech{displacement-mode operand}, so
we can access statically known offsets from base pointers other than the frame
base pointer.
This generalized operand, @tt{QWORD [reg_base + int32]}, adds some static offset
to a base pointer to compute an address.
Note that unlike the version used to implement @ch2-tech{frame variables}, we
@emph{add} to (rather than subtract from) the base pointer, and the index is not
restricted to a multiple of 8.

Each of these operands can be as part of move and arithmetic instructions.
For example:
@verbatim{
  mov r10, 0
  mov QWORD [r12 + r10], 0
  add QWORD [r12 + r10], 1
  mov rax, QWORD [r12 + r10]
}
moves 0 into the address @tt{r12 + r10}, adds 1 to it, then moves the result
into the return value register.
It assumes @tt{r12} contains some valid pointer.
@tech{Index-mode operands} are subject to similar restrictions as with the
@ch2-tech{displacement-mode operand}.
We formalize these restrictions in our definition of @tech{Paren-x64 v8}.

To implement allocation, we need some strategy for managing memory.
Our run-time system or compiler needs to know what memory is in use, how to
allocate (mark free memory as in use), and how to deallocate memory (return
in-use memory to the pool of free memory and ultialtey to the system).
There are many strategies for this, such as "let the user deal with it",
"add a process to the run-time system that dynamically manages memory", or "make
the type system so complex that the compiler can statically manage memory".
Each of these has merits.

We choose an all together different approach: memory is infinite so just keep
allocating.
Of course, memory isn't @emph{quite} infinite, but if our programs are
relatively short lived, and the operating system cleans up a process's memory
when it finishes running, then this approach will work and save us a lot of
effort.
This isn't as unrealistic a strategy as it might as first appear; it's used in
practice by short-lived programs, such as some Unix command line tools and the
control systems for some missles.
@digression{
In general, a language implementation might abstract access to the @tt{mmap}
system call for allocation, and implement a strategy (such as garbage
collection) to deallocate memory that is no longer used.
Garbage collection is tricky to implement so we avoid it for now to focus on
implement structured data.

For a quick introduction to garbage collection, see this short video
@url{https://twitter.com/TartanLlama/status/1296413612907663361?s=20}.
}

To implement our allocation-only strategy, we need the run-time system to
provide an initial base pointer from which we can start allocating.
We reserve another register, the @racket[current-heap-base-pointer-register]
(abbreviated @paren-x64-v8[hbp]).
The run-time system initializes this register to point to the base of the heap,
with all positive-integer indexes after this base as free memory.
Allocation is implemented by copying the current value of this pointer, and
incrementing it by the number of bytes we wish to allocate.
The pointer must only be incremented by word-size multiples of bytes, to ensure
the 3 low-order bits are 0 and the pointer can be tagged.
Any other access to this register is now undefined behvaiour, similar to
accesses to @paren-x64-v8[fbp] that do not obey the stack of frames discipline.


@section{Implementing Structured Data}
We design @deftech{Exprs-lang v8} below.
The language is large, as we include several new structured data types and their
primitives.

@bettergrammar*-ndiff[
#:labels ("Diff vs v7" "Exprs-lang v8")
(exprs-lang-v7 exprs-lang-v8)
(exprs-lang-v8)
]

Since the number of primitive operations is growing, we simplify the syntax to
only give @exprs-lang-v8[primops], rather than distinguishing
@exprs-lang-v8[unops], @exprs-lang-v8[binops], and so on, so we can easily group
like primops with like.

As usual, we first task is to @racket[uniquify].
Below we define the target language, @deftech{Exprs-unique-lang v8}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v7 (excerpts)" "Diff vs Source" "Exprs-unique-lang v8")
(#:exclude (aloc label fixnum uint8 ascii-char-literal)
 exprs-unique-lang-v7 exprs-unique-lang-v8)
(exprs-lang-v8 exprs-unique-lang-v8)
(exprs-unique-lang-v8)
]

@nested[#:style 'inset
@defproc[(uniquify [p exprs-lang-v8])
          exprs-unique-lang-v8]{
Resolves top-level @ch3-tech{lexical identifiers} into unique labels, and all
other @ch3-tech{lexical identifiers} into unique @ch2-tech{abstract locations}.
}]

@; Don't know how to do this without closures.
@;Last week, the last piece of undefined behvaiour in our language was in procedure
@;calls.
@;We were not checking that procedures were correctly applied to the number of
@;expected arguments.
@;Lower down, when procedure calls are implemented in terms of register and frame
@;variables, this could result in dereferencing uninitialized locations.
@;We can use the procedure data type to eliminate this undefined behvaiour.
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
Next, we implement new safe primitive operations.
All the accessors for the new data types can result in undefined behaviour if
used on the wrong @v7-tech{ptr}.
Similarly, vector reference can access undefined values if the vector is
constructed but never initialized.
We fix this by wrapping each primitive to perform dynamic checks and remove
undefined behaviour.

We design the target language, @deftech{Exprs-unsafe-data-lang v8}, below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v7" "Exprs-unsafe-data-lang v8")
(exprs-unsafe-data-lang-v7 exprs-unsafe-data-lang-v8)
(exprs-unsafe-data-lang-v8)
]

Note that in this language, we add expose effect context and
@exprs-unsafe-data-lang-v8[begin].
In the source, we have an effect procedure, namely @racket[vector-set!].
The user must manually call impure functions and bind the result.
The result of an effectful function is either @exprs-unique-lang-v8[void], or an
@exprs-unique-lang-v8[error].
The unsafe variant, @racket[unsafe-vector-set!], does not need to produce a
value, so we expose effect context.

To implement the safe language, we wrap all accessors to perform dynamic tag checking
before using the unsafe operations.
We also wrap @exprs-unsafe-data-lang-v8[unsafe-make-vector] to initialize all elements to
@racket[0].

To implement the safe primops, it may be useful to abstract out the
safety specification for each primitive operation.
For example, we could write our compiler as a a function over the program and
the following specification language.
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
@todo{Handling vector-set! required inlining a bit of context normalization}

@defproc[(implement-safe-primops [p exprs-unique-lang-v8?])
         exprs-unsafe-data-lang-v8?]{
Implement safe primitive operations by inserting procedure definitions for each
primitive operation which perform dynamic tag checking, to ensure type and
memory safety.
}

Finally, we must extend @racket[specify-representation] to implement the new
data structures and primitives.
We design the target @deftech{Exprs-bits-lang v8} below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v7 (excerpts)" "Diff vs Source" "Exprs-bits-lang v8")
(#:exclude (triv binop aloc label relop int64)
 exprs-bits-lang-v7 exprs-bits-lang-v8)
(#:exclude (triv binop aloc label relop int64)
 exprs-unsafe-data-lang-v8 exprs-bits-lang-v8)
(exprs-bits-lang-v8)
]

We implement the new data structures using the approach described earlier,
combining tagging with the new allocation and memory abstractions.
We compile each constructor, namely @exprs-unsafe-data-lang-v8[cons] and
@exprs-unsafe-data-lang-v8[unsafe-make-vector], to @exprs-bits-lang-v8[alloc]
plus tagging, producing the tagged pointer as the value.
We compile each destructor to @exprs-bits-lang-v8[mref], detagging or statically
adjusting the index to the pointer first.
Initliazation, done by @exprs-unsafe-data-lang-v8[cons], and mutation, done by
@exprs-unsafe-data-lang-v8[unsafe-vector-set!], are both compiled to
@exprs-bits-lang-v8[mset!].

@defproc[(specify-representation [p exprs-unsafe-data-lang-v8?])
          exprs-bits-lang-v8?]{
Compiles data types and primitive operations into their implementations as
@v7-tech{ptrs} and primitive bitwise operations on @v7-tech{ptrs}.

@examples[#:eval sb
(specify-representation '(module (cons 5 6)))
(specify-representation '(module (unsafe-car (cons 5 6))))
(specify-representation '(module (unsafe-vector-ref (unsafe-make-vector 3) 6)))
]
}

@section{Front-end Extensions}
We've added effect context and a new effect higher in the compiler pipeline than
previously.
This requires some attention to a few of our front-end passes.

Our next pass, @racket[remove-complex-opera*], is responsible for imposing
left-to right evaluation for operands and arguments, and explicitly binding all
algebraic expressions.
We design @deftech{Values-bits-lang v8} below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v7 (excerpts)" "Diff vs Source" "Values-bits-lang v8")
(#:exclude (triv binop aloc label relop int64)
 values-bits-lang-v7 values-bits-lang-v8)
(#:exclude (triv binop aloc label relop int64)
 values-bits-lang-v7 exprs-bits-lang-v8)
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
We add the non-terminal @values-bits-lang-v8[effect] to represent an impure computation.
A @values-bits-lang-v8[effect] represents an expression that does not have a value, and is
executed only for its effect.
We can use @values-bits-lang-v8[effect] in certain expression contexts using
@values-bits-lang-v8[begin].
If we're already in an impure context, that is, in a
@values-bits-lang-v8[effect], then we can freely nest other
@values-bits-lang-v8[effect]s.

This contextual distinction is similar to the one we introduce to distinguish
tail calls from non-tail calls.

Supporting @exprs-bits-lang-v8[effect] context requires paying attention to
order when designing @racket[remove-complex-opera*], but does not significantly
complicate anything.

@nested[#:style 'inset
@defproc[(remove-complex-opera* [p exprs-bits-lang-v8?])
          values-bits-lang-v8?]{
Performs the monadic form transformation, unnesting all non-trivial operators
and operands, making data flow explicit and and simple to implement imperatively.
}]

Next, we transform into imperative instructions.
Now that effects can appear on the right-hand side of a
@values-bits-lang-v8[let] expression, it MAY not longer be safe to reorder them.
This is a design choice: we could make it clear to the programmer that
@values-bits-lang-v8[let] does not guarantee a particular order of evaluation
for its bindings, but then effects on the right-hand side lead to undefined
behaviour.
Or, we could impose a particular order, such as left-to-right, forbidding
possible optimizations.
A middle ground is to impose such an order only if any effects are detected in
the right-hand side of a @values-bits-lang-v8[let] (or rather, if we can
guarantee no effects are present, because Rice still does not let us know for
sure).

As usual, we choose to forbid exposing undefined behaviour to the source
language, so design @racket[sequentialize-let] to impose left-to-right (unless
we know its safe to reorder).

We design the target language, @deftech{Imp-mf-lang v8}, below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v7 (excerpts)" "Diff vs Source (Excerpts)" "Imp-mf-lang v8")
(#:exclude (opand triv binop relop int64 aloc label)
 imp-mf-lang-v7 imp-mf-lang-v8)
(#:exclude (opand triv binop relop int64 aloc label)
 values-bits-lang-v8 imp-mf-lang-v8)
(imp-mf-lang-v8)
]

@nested[#:style 'inset
@defproc[(sequentialize-let [p values-bits-lang-v8?])
          imp-mf-lang-v8?]{
Picks a particular order to implement @values-bits-lang-v8[let] expressions
using @imp-mf-lang-v8[set!].
}]


With the addition of an @imp-mf-lang-v8[mset!], @racket[normalize-bind] must be
updated.
This operator is enables the same kind of nesting as @imp-mf-lang-v8[set!], and
like @imp-mf-lang-v8[set!], its operand needs to be normalized.

Next we design @deftech{Proc-imp-cmf-lang v8}

@bettergrammar*-ndiff[
#:labels ("Diff vs v7" "Diff vs Source" "Proc-imp-cmf-lang v8")
(proc-imp-cmf-lang-v7 proc-imp-cmf-lang-v8)
(imp-mf-lang-v8 proc-imp-cmf-lang-v8)
(proc-imp-cmf-lang-v8)
]

@question{Why can't (or shouldn't) we allow the index position to also be a
@imp-mf-lang-v8[value]?}

@defproc[(normalize-bind [p imp-mf-lang-v8?])
         proc-imp-cmf-lang-v8?]{
Pushes @imp-mf-lang-v8[set!] and @imp-mf-lang-v8[mset!] under @imp-mf-lang-v8[begin] and
@imp-mf-lang-v8[if] so that the right-hand-side of each is simple
value-producing operand.

This normalizes @tech{Imp-mf-lang v8} with respect to the equations
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

@racket[impose-calling-conventions] and @racket[select-instructions] require
only minor changes, so we leave those changes as an exercise for the reader.

@nested[#:style 'inset
@defproc[(impose-calling-conventions [p proc-imp-cmf-lang-v8?])
          imp-cmf-lang-v8?]{
Compiles @tech{Proc-imp-cmf-lang v8} to @tech{Imp-cmf-lang v8} by imposing calling
conventions on all calls (both tail and non-tail calls), and @tech{entry
points}.
}]

@defproc[(select-instructions [p imp-cmf-lang-v8?])
         asm-alloc-lang-v8?]{
Selects appropriate sequences of abstract assembly instructions to implement the
operations of the source language.
}

@section{Implementing Allocation}
As previously discussed, our allocation strategy is to simply grab the current
heap base pointer, and increment beyond the bytes we want to use for our data
structure.
The question is where to implement this operation.
We want to do this @emph{after} we have access to physical locations, so so
after @racket[impose-calling-conventions], but @emph{before} the register
allocator passes, so we do not have to update those passes to know that
@asm-alloc-lang-v8[alloc] introduces a reference to a register.
This puts the pass right between @racket[select-instructions] and
@racket[uncover-locals].

Below, we design @deftech{Asm-alloc-lang v8}, the source language for this pass.
We typeset the differences compared to @racket[asm-pred-lang-v7].

@bettergrammar*-ndiff[
#:labels ("Diff vs v7 (excerpts)" "Full")
(#:exclude (binop relop int64 opand triv loc trg aloc label rloc)
 asm-pred-lang-v7 asm-alloc-lang-v8)
(asm-alloc-lang-v8)
]

@todo{Should not reuse index as the operand for alloc; it is semantically a
different concept, which happens to have the same representation.
}
@;Should probably abstract dispoffset to word-aligned bytes or something, then
@;reuse that.

This language contains our intermediate @tech{mops}, including
@asm-alloc-lang-v8[(set! loc (alloc index))].
This is the low-level form of our allocation operation.

We design the target language, @deftech{Asm-pred-lang v8}, below.
This language removes the @asm-alloc-lang-v8[(alloc index)] form and is the
highest-level language parameterized by
@racket[current-heap-base-pointer-register].
This language, and all languages between it and @ch1-tech{x64}, assumes all
accesses to @paren-x64-v8[hbp] obey the restrictions described in
@tech{Paren-x64 v8}.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Diff vs v7 (excerpts)" "Asm-pred-lang v8")
(#:exclude (opand triv loc trg binop relop int64 int32 aloc label rloc)
 asm-alloc-lang-v8 asm-pred-lang-v8 )
(#:exclude (opand triv loc trg binop relop int64 int32 aloc label rloc)
 asm-pred-lang-v7 asm-pred-lang-v8)
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

@section{Implementing @tech{mops}}
The new @tech{mops} require minor changes to most of the pipeline between
@tech{Imp-cmf-lang-v8}to @tech{Para-asm-lang v8}, where we will start
implementing these abstractions in the low-level languages.

Like when we implemented @para-asm-lang-v8[fvar]s to support working with frame
locations, we added @tech{mops} to simplify working with heap addresses.
We want to keep that simplification around as long as possible, to avoid
complicating the already complex logic for rewriting @paren-x64-v8[set!]
instructions.

The last pass that needs to do non-trivial rewriting of @paren-x64-v8[set!] is
@racket[patch-instructions], and it would benefit from that simplification.
So we decide to leave @tech{mops} in place and implement them @emph{after}
@racket[patch-instruction].

Below, we design @deftech{Para-asm-lang v8}, the new source language for
@racket[patch-instructions].
@;However, your design may differ slightly since you've been responsible for the
@;design of @a6-tech{Paren-asm v6} and @a7-tech{Paren-asm v7}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v7 (excerpts)" "Para-asm-lang v8")
(#:exclude (reg relop binop trg opand label int64)
 para-asm-lang-v7 para-asm-lang-v8)
(para-asm-lang-v8)
]

The target language, @deftech{Paren-x64-mops v8}, is given below.
@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Diff vs Paren-x64 v7" "Paren-x64-mops v8")
(#:exclude (reg relop binop trg opand label int64)
 para-asm-lang-v7 paren-x64-mops-v8)
(#:exclude (reg relop binop trg opand label int64)
 paren-x64-v7 paren-x64-mops-v8)
(paren-x64-mops-v8)
]

As usual, @racket[patch-instructions] makes a lot of changes to the operands of
each instructions.

By leaving @tech{mops} separate forms, we only need to patch the new
instructions, and leave patching of old instructions untouched.
We do not need to pay attention to and patch a new kind of operand.

The main restrictions on @tech{mops} are that the operands must be registers,
and that the index must be a 32-bit integer.
The operands for @tech{mops} have to be registers since they are compiled to
low-level instructions where one operand is implicitly an index-mode operand.
Similar to patching @ch2-tech{displacement-mode operands}, both operands cannot
be reference to memory.
The @para-asm-lang-v8[index] must be patches to 32-bit literals only for the
same reason that binary operations must only contain 32-bit literals.
This makes @para-asm-lang-v8[index] and @para-asm-lang-v8[opand] coincide,
syntactically, but they are conceptually different so we maintain separate
non-terminal definitions.

@defproc[(patch-instructions [p para-asm-lang-v8])
         paren-x64-mops-v8]{
Patches instructions that have no @ch1-tech{x64} analogue into to a sequence of
instructions and an auxiliary register from
@racket[current-patch-instructions-registers].
}

Finally, we can translate @tech{mops} into @tech{index-mode operands} and
@ch2-tech{displacement-mode operands}.
Note that the compiler pass doesn't need to know which it is generating; their
syntax is the same except for type of the second operand.
The target language is @deftech{Paren-x64 v8}, which we define below.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Diff vs v7" "Paren-x64 v8")
(#:include (p s addr index)
 paren-x64-mops-v8 paren-x64-v8)
(#:include (p s addr index)
 paren-x64-v7 paren-x64-v8)
(paren-x64-v8)
]

The language contains a new @paren-x64-v8[addr] representing the @ch1-tech{x64}
@tech{index-mode operand} @paren-x64-v8[(reg + reg)].
This supports accessing a memory location by the index stored in another
register.
For example, in @ch1-tech{x64}, we represent loading the @emph{n}th element of an
array into @paren-x64-v8[r10] using @tt{mov r10 [r11 + r12]}, where the base of the
array is stored at @tt{r11} and the value of @emph{n} is stored in @tt{r12}.

The index-mode operand is not restricted to use a particular register, unlike
the displacement-mode operand from @ch7-tech{Paren-x64 v7}.

@;@margin-note{We could encode the restricted form of @paren-x64-mops-v8[addr],
@;used for frame variables, as an @paren-x64-mops-v8[mref], but the rest of our
@;compiler already knows about @paren-x64-mops-v8[addr]s, and it represents a
@;semantically different concept, so we leave it alone.}

@nested[#:style 'inset
@defproc[(implement-mops [p paren-x64-mops-v8?])
          paren-x64-v8?]{
Compiles @tech{mops} to instructions on pointers with index- and
displacement-mode operands.
}]

Finally, we update @racket[generate-x64] to emit the string representation of
the @tech{index-mode operand}.

@nested[#:style 'inset
@defproc[(generate-x64 [p paren-x64-v8?])
         string?]{
Compile the @tech{Paren-x64 v8} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}]


@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v8-graph" "Overview of Compiler Version 8" v8-graph]
