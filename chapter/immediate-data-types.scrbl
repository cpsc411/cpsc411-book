#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a7-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v6
  cpsc411/langs/v7
  (for-label cpsc411/langs/v7))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a7-solution]

@(define sb
   (make-cached-eval
    "ch7-eval"
    '(require racket/pretty cpsc411/reference/a7-solution cpsc411/compiler-lib)))

@define[v7-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

Lx [label="Exprs-lang v7"];
Ly [label="Exprs-unique-lang v7"];
Lz [label="Exprs-unsafe-data-lang v7"];
L0 [label="Exprs-bits-lang v7"];
L1 [label="Values-bits-lang v7"];
L2 [label="Proc-imp-mf-lang v7"];
L3 [label="Imp-mf-lang v7"];
L4 [label="Imp-cmf-lang v7"];
L5 [label="Asm-pred-lang v7"];
L6 [label="Asm-pred-lang v7/locals"];
L7 [label="Asm-pred-lang v7/undead"];
L8 [label="Asm-pred-lang v7/conflicts"];
L81 [label="Asm-pred-lang v7/pre-framed"];
L82 [label="Asm-pred-lang v7/framed"];
L83 [label="Asm-pred-lang v7/spilled"];
L9 [label="Asm-pred-lang v7/assignments"];
L10 [label="Nested-asm-lang-fvars v7"];
L10_1 [label="Nested-asm-lang v7"];
L11 [label="Block-pred-lang v7"];
L12 [label="Block-asm-lang v7"];
L12_1 [label="Para-asm-lang v7"];
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

  L16 [label="Paren-x64 v7"];
  L17 [label="Paren-x64-rt v7"];
}

  L16 -> L17 [label=" link-paren-x64"];
  L17 -> L15 [label=" interp-loop"];
  L16 -> L15 [label=" interp-paren-x64"];
}
}
]

@title[#:tag "top" #:tag-prefix "chp-immediates:"]{Data types: Immediates}

@(define (ch-v6-tech . rest)
  (apply tech #:tag-prefixes '("book:" "chp-return:") rest))

@section{Preface: What's wrong with @ch-v6-tech{Values-lang v6}}
@ch-v6-tech{Values-lang v6} gained the ability to express non-tail calls, an
important step forward in writing real programs.
However, our functions are still limited to work on machine integers.
A realistic language would allow us to express programs over more interesting
data types than mere machine integers.

Unfortunately, once we add data types, we have a problem distinguishing between
any two data types.
Everything is eventually represented as 64-bit integers in @ch1-tech{x64}.
We need some way to distinguish a collection of 64-bit as an integer, from a
boolean, from the empty list, from a pointer to a procedure.
Otherwise, we will have serious problems: undefined behaviours, unsafe casts,
or memory safety errors.

@;One way to add data types is to add a static type system.
@;This will certainly restrict undefined behaviour, but may impose a higher cost
@;on programming.
@;Due to Rice's Theorem@margin-note*{Still a huge buzz kill.}, we will never be
@;able to decide whether an arbitrary program is well typed.
@;Instead, we will be required to approximate and/or require user annotations,
@;ruling out some well-behaved programs to ensure no undefined behaviour.

This is not only a problem for ruling out unsafe behaviour in the source
language.
A static type system could take care to prevent the user from calling an
integer as a procedure, for example.
However, the language itself may need to distinguish different kinds of data at
run-time.
For example, a garbage collector may need to traverse data structures, but not
immediate data like integers; a pretty printer may want to print different data
differently.

To enable the language to distinguish different kinds of data, we can steal a
few of our 64-bits to represent a data type tag.
This limits the range of machine integers, but allows us to us to distinguish
data dynamically, enabling safety and abstractions that must differentiate data
dynamically.

There's a second limitation in @ch-v6-tech{Values-lang v6} that we lift this
week.
So far, we do not have algebraic expressions---we have limited ability to nest
expressions, and must manually bind many intermediate computations to names.
This is particularly tedious since we know how to write a compiler, which ought
to do the tedious work of binding expressions to temporary names.
After we have proper data types with dynamic checking, we can more easily nest
expressions, particularly in the predicate position of an @exprs-lang-v7[if].
@digression{
Normally, we take care to design each new abstraction independently from each
other, when we can.
However, data types and algebraic expressions are difficult to separate without
sacrificing some design goal.
With only algebraic expressions and no data types, it becomes difficult to
enforce safety without adding a more sophisticated type system with type
annotations.
With only data types and no algebraic expressions, implementing the compiler
passes is more tedious because implementing pointer manipulation operations to
implement data type tags benefits from algebraic expressions.
}

Our goal in this assignment is to implement the following language,
@tech{Exprs-lang v7}.

@bettergrammar*[exprs-lang-v7]

We add a bunch of new values in @tech{Exprs-lang v7}, including booleans, the
empty list, the void object, (printable) ASCII character literals, and an error
value.
@tech{Exprs-lang v7} programs are allowed to return any of these values.
@margin-note{We restrict ASCII characters to the printable ones, so we don't
have to figure out how to print non-printable characters.
The run-time system will work with some non-printable characters, but the
results will not be converted to Racket properly.
}

Data type require new support from the run-time system.
The new run-time system, @racketmodname[cpsc411/ptr-run-time], supports
printing the new values.
@racket[execute] takes a new optional second parameter, which can be used to
change your view of the result.
By default, you get back a Racket value.
You can pass @racket[nasm-run/print-string] to get back the string
representation of the result, which will be handy to view the result of
@exprs-lang-v7[(void)] correctly.
You can pass @racket[nasm-run/exit-code] to get the exit code, which is helpful
for viewing the result of @exprs-lang-v7[(error uint8)], which sets the exit code
to @exprs-lang-v7[uint8].

We also add new primitive operations, primarily predicates on our new data
types.
The interpretation of several operations will also change to add dynamic
type checking.
This will prevent those operations from being a source of undefined behaviour.

With proper booleans, we can finally allow an arbitrary value in the predicate
position of an @exprs-lang-v7[if] expression in the surface language.

It is not clear how to support new data types going top-down, so we begin this
assignment at the bottom.

@section[#:tag "intro-to-tags"]{Introduction to Tagged Object Representation}
A common approach to efficiently represent word-sized data types is called
@deftech{object tagging}.
@digression{Some of our clever tag choices, and some terminology, is borrowed
from R. Kent Dybvig.
You can learn more about it's use in Chez Scheme from this talk by his former
PhD student Andy Keep: @url{https://www.youtube.com/watch?v=BcC3KScZ-yA}.
}
Each data type in our language will now be represented by a @deftech{ptr}
(pronounced like @emph{footer}).
A @tech{ptr} is a machine word whose @racket[n] least-significant bits represent
the @deftech{primary tag}, and whose upper @racket[(- (* 8
(current-word-size-bytes)) n)] bits represent the data.

In our system, we have 64-bit words and will use the 3 least-significant bits
for @tech{primary tags}.
With 3 bits, we can represent 8 primary data types in a @tech{ptr}.
We want to reserve these for the most common and most performance critical data
that can fit in a @tech{ptr}, but we have additional constraints.
This will limit the size of data we can represent, but gives us additional data
types---probably a good trade off.

If we want more than 8 data types, which most languages support, we must reserve
one tag for "everything else" (a pointer to memory).
The structure in memory has infinite space in which to store additional tags.
This seems to leave us only 7 possible data types.

Some data types require fewer than 64-bits, and we can exploit this fact to
gain a few extra data types.
For example, booleans really only require one bit.
The empty list and the void object only require a tag, since they do not contain
any data.
ASCII characters only require 8 bits.
We can make all of these share a single @tech{primary tag}, and steal some of
the unnecessary high-order bits for a @deftech{secondary tag}.
This gives us 4 data types for the price of only 1 @tech{primary tag}---what a
bargain!

This leaves us 6 @tech{primary tags}.
One obvious choice is to use one for fixed sized integers; these will now be
61-bit integers.
Integer operations are fast, and we don't want to slow them down by making them
go through heap allocation and pointer indirection.
If we wanted to support graphics and scientific operations, we would reserve one
for floating-point numbers too.

We reserve the remaining @tech{primary tags} for @deftech{tagged pointers},
pointers to common heap-allocated structures.
Storing the tag on the pointer instead of in memory avoids the expense of an
additional memory access to check a tag and the additional memory overhead of
storing the tag in memory.
We'll address the implementation of heap-allocated structured data in a future
chapter.

Here is the default set of tags we will use in this assignment, given in base 2.
@itemlist[
@item{@code{#b000}, @deftech{fixnums}, fixed-sized integers}
@item{@code{#b001}, @emph{unused}}
@item{@code{#b010}, @emph{unused}}
@item{@code{#b011}, @emph{unused}}
@item{@code{#b100}, @emph{unused}}
@item{@code{#b101}, @emph{unused}}
@item{@code{#b110}, non-fixnum immediates (booleans, etc)}
@item{@code{#b111}, @emph{unused}}
]
@margin-note{Racket supports binary literals, and automatically interprets
them as two's complement integers.
If you type @code{#b111}, you get back @racket[#b111], and the two values are
@racket[equal?].
This will be helpful when writing this assignment and writing tests.}

For the non-fixnum immediates, we use the following @tech{secondary tags}.
Note that the 3 least-significant bits of @tech{secondary tags} are the shared
@tech{primary tag}.
@itemlist[
@item{@code{#b00000110}, for @exprs-lang-v7[#f]}
@item{@code{#b00001110}, for @exprs-lang-v7[#t]}
@item{@code{#b00010110}, for @exprs-lang-v7[empty]}
@item{@code{#b00011110}, for @exprs-lang-v7[(void)]}
@item{@code{#b00101110}, for an ASCII character}
@item{@code{#b00111110}, for the error value}
]
@margin-note{For annoying technical reasons, our representation of the empty
list differs slightly from Racket.
We use @exprs-lang-v7[empty], while Racket uses @racket['()].}


The integer 7 is @code{#b111} in base 2, and would be represented, in base 2, as
the @tech{ptr} @code{#b111000}.
The three low-order bits are the tag @code{#b000}, and the high-order bits are
the data @code{#b111} (implicitly padded with 0 to 64-bits).
To perform arithmetic on this representation, we can use simply right-shift the
@tech{ptr} by the size of a @tech{primary tag} to get a two's complement
integer:
@examples[
(arithmetic-shift #b111000 -3)
]
@margin-note{All these notes are typeset using base-2 literals, but Racket
cannot distinguish them from integers, so they may get accidentally rendered in
base 10.}

A handy fact about the choice of tag for fixnums is that any number
@racket[n] is represented as @racket[(* 8 n)].
@;@examples[
@;#b111000
@;#b111
@;(* 7 8)
@;]
This fact allows for some optimizations on fixnum operations, and will make
porting examples and tests easier.

Similarly, the ASCII character 7 would be as a @tech{ptr}, in base 2,
@code{#b011011100101110}.
@margin-note{See
@url{https://en.wikipedia.org/wiki/ASCII#Printable_characters} for the
representation of ASCII characters.
}
For character operations, we must right-shift by the size of a @tech{secondary
tag} (8).
The character @code{#b0110111} is the ASCII representation of 7.
Combined with its tag, this is @code{#b011011100101110}, which reads in Racket
as @racket[#b011011100101110].
@examples[
(integer->char (arithmetic-shift #b011011100101110 -8))
]

We can implement tag checks using bitwise masking.
For example, to check whether a @tech{ptr} is a fixnum, we mask the @tech{ptr}
with tag @code{#b111} (@racket[#b111]) and compare the result to the
@tech{primary tag} for fixnums, @code{#b000} (@racket[#b000]).
For example, we can ask whether the above two examples are fixnums
(and whether the latter is a character) as follows:
@examples[
(eq? (bitwise-and #b111000 #b111) #b000)
(eq? (bitwise-and #b011011100101110 #b111) #b000)
(eq? (bitwise-and #b011011100101110 #b11111111) #b00101110)
]
Our representation of the number 7 is a fixnum, while the representation of the
character 7 is not.

The representation of booleans is a bit tricky.
We don't quite introduce a new tag for booleans, and then add a single bit in
the payload.
Instead, we essentially represent @exprs-lang-v7[#t] and @exprs-lang-v7[#f]
as separate data types, whose tags are @code{#b110} (@racket[#b110]) and
@code{#b1110} (@racket[#b1110]).
It makes masking a boolean slightly more complex.
We use the mask @code{#b11110111} (@racket[#b11110111]), to succeed regardless
of whether the fourth bit is set.
@examples[
(eq? (bitwise-and #b00001110 #b11110111) #b110)
(eq? (bitwise-and #b00000110 #b11110111) #b110)
]
The benefit is that any immediate that is not @exprs-lang-v7[#f]
can be interpreted as @exprs-lang-v7[#t] using @exprs-lang-v7[bitwise-xor],
instead of converting to a boolean and using an arithmetic shift to compute the
value of the boolean.
Only @exprs-lang-v7[#f] is 0 when "xor"'d with the non-fixnum immediate tag,
@code{#b110}.
@examples[
(eq? (arithmetic-shift #b00000110 -3) 0)
(eq? (arithmetic-shift #b00001110 -3) 0)
(eq? (bitwise-xor #b00000110 #b110) 0)
(eq? (bitwise-xor #b00001110 #b110) 0)
(eq? (bitwise-xor #b00111000 #b110) 0)
(eq? (bitwise-and #b00001110 #b111) 0)
(eq? (bitwise-xor #b011011100101110 #b110) 0)
]

@todo{Why not just... (!= #b110)?}

The representation of @exprs-lang-v7[error] is inefficient.
We only require 8 bits of data for the error code, so 24 bits are wasted.
But we're not adding more data types with @tech{secondary tags}, so it is
not worth over-engineering.
Besides, a better error data type would use at least a string payload, which
requires allocating space on the heap anyway.

The particular choice of tags is not important for correctness, although clever
choices like above can help us implement common operations more efficiently.
We therefore should introduce abstractions to keep the compiler abstract with
respect to particular tag choices, so we can change our minds later if a better
representation occurs to us.

To implement @tech{ptrs}, we need bitwise operations so that we can mask off the
tags bits and operate on the data.
This means we need to expose bitwise operations from @ch1-tech{x64}.

@section{Exposing Bitwise Operations in Paren-x64}

We expose the following @ch1-tech{x64} instructions this week.
@itemlist[
@item{@tt{sar @paren-x64-v7[loc], @paren-x64-v7[int]}

Perform arithmetic right-shift by @paren-x64-v7[int] on @paren-x64-v7[loc].

This instruction requires that its second operand be an integer literal between
0 and 63, inclusive, @ie @racket[0 <= int <= 63].
We will assume this constraint in the intermediate languages, and
never expose this operation in @tech{Exprs-lang v7}.}

@item{@tt{and @paren-x64-v7[loc], @paren-x64-v7[op]}

Compute the bitwise "and" of @paren-x64-v7[loc] and @paren-x64-v7[op], storing
the result in @paren-x64-v7[loc].
Like with other binary operations, when @paren-x64-v7[op] is an integer, it
must be an @paren-x64-v7[int32], and when @paren-x64-v7[op] is an
@paren-x64-v7[addr], @paren-x64-v7[loc] cannot also be an @paren-x64-v7[addr].
}

@item{@tt{or @paren-x64-v7[loc], @paren-x64-v7[op]}

Compute the bitwise "inclusive or" of @paren-x64-v7[loc] and @paren-x64-v7[op],
storing the result in @paren-x64-v7[loc].
Like with other binary operations, when @paren-x64-v7[op] is an integer, it
must be an @paren-x64-v7[int32], and when @paren-x64-v7[op] is an
@paren-x64-v7[addr], @paren-x64-v7[loc] cannot also be an @paren-x64-v7[addr].
}

@item{@tt{xor @paren-x64-v7[loc], @paren-x64-v7[op]}

Compute the bitwise "exclusive or" of @paren-x64-v7[loc] and @paren-x64-v7[op],
storing the result in @paren-x64-v7[loc].
Like with other binary operations, when @paren-x64-v7[op] is an integer, it
must be an @paren-x64-v7[int32], and when @paren-x64-v7[op] is an
@paren-x64-v7[addr], @paren-x64-v7[loc] cannot also be an @paren-x64-v7[addr].
}
]

First, we add each of these operations as a @paren-x64-v7[binop] to
@deftech{Paren-x64 v7} below.
The differences are typeset with respect to @ch-v6-tech{Paren-x64 v6}.

@bettergrammar*-diff[paren-x64-v6 paren-x64-v7]

@defproc[(generate-x64 [p paren-x64-v7?])
         (and/c string? x64-instructions?)]{
Compile the @tech{Paren-x64 v7} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}

@section{Exposing Bitwise Operations}
Specifying the tagged representation happens very close to our source language.
We need to expose these operations all the way up to our prior source language,
@ch-v6-tech{Values-unique-lang-v6}.
Below, we design the new @deftech{Values-bits-lang v7}, typeset with differences
compared to @ch-v6-tech{Values-unique-lang-v6}.

@bettergrammar*-diff[values-unique-lang-v6 values-bits-lang-v7]

The only data type in @tech{Values-bits-lang v7} is BITS, 64 for them,
interpreted as an integer sometimes.

We assume that each @values-bits-lang-v7[binop] is well-typed; they shouldn't be
used with labels as arguments, the and calls to
@values-bits-lang-v7[arithmetic-shift-right] follow the restrictions required by
@ch1-tech{x64}.

The new operations do not have large effects on the language designs or compiler
passes between @racket[sequentialize-let] and @racket[generate-x64], so these
details are left unspecified in this chapter.
You will need to redesign the intermediate languages with the new operations.

@section{Algebraic Expressions}
As we saw in @secref{intro-to-tags}, many of the operations we want to perform
on @tech{ptrs} are easily expressed as algebraic expressions.
The expression @object-code{(fixnum? 7)} is expressed as
@code{(eq? (bitwise-and 7 #b111) #b000)}.
Representing this in @tech{Values-bits-lang v7}, which does not have algebraic
expressions, is unnecessarily complicated.
We would have to introduce auxiliary variables, manually pass them around the
compiler, and generate code with additional let-bindings.
This is particularly silly; we know how to write compilers, so the compiler
should do this for us, and allow us to write the code we want to write.

We therefore design @deftech{Exprs-bits-lang v7}, a language that has only bits
and bitwise operations, but that allows algebraic expressions in most
positions.
The predicate position of @exprs-bits-lang-v7[if] expressions is still
restricted, since we cannot introduce algebraic @exprs-bits-lang-v7[if]
expressions without booleans.

@bettergrammar*[exprs-bits-lang-v7]

Looking at this grammar, it may not be obvious how to transform this into
@tech{Values-bits-lang v7}.
We can see the structure of the pass more clearly if we expand the grammar
slightly.
Below, we define the expanded grammar @deftech{Exprs-bit-lang v7/context}.
We typeset the differences compared to @tech{Values-bits-lang v7}

@bettergrammar*-diff[values-bits-lang-v7 exprs-bits-lang-v7/contexts]

Now we can see the the main difference, semantically, is additional nesting in
the value context.
Calls and binary operations can now take nested @values-bits-lang-v7[value]
expressions rather than @values-bits-lang-v7[triv]s.
While this means we can collapse the syntax, separating the syntax semantically
helps us see the true difference and see that the essence of the transformation
is let-binding intermediate results to make all operands trivial.

To transform this into @tech{Values-bits-lang v7}, we need to perform the
moandic form translation.
In essence, we recursively translate any nested expression that is not a
@values-bits-lang-v7[value] into one by let-binding all intermediate
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
We can do a better job if we following the template for the output language
while processing the input language.
In this way, we design functions for each non-terminal in the output language.
These functions describe in what context the current term is being processed.
For example, when processing @exprs-bits-lang-v7[(relop value_1 value_2)], we
want to process @exprs-bits-lang-v7[value_1] in
@values-bits-lang-v7[aloc] context, since that's the restriction in the target
language, but can process @exprs-bits-lang-v7[value_2] in
@values-bits-lang-v7[triv] context to avoid creating an unnecessary auxiliary
binding.
}

For historical reasons, we call this pass @racket[remove-complex-opera*].

@defproc[(remove-complex-opera* [p exprs-bits-lang-v7?])
         values-bits-lang-v7?]{
Performs the monadic form transformation, unnesting all non-trivial operators
and operands to @exprs-bits-lang-v7[binop]s, @exprs-bits-lang-v7[call]s, and
@exprs-bits-lang-v7[relops]s, making data flow explicit and and simple to
implement imperatively.
}

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


@section{Specifying Data Type Representation}
@subsection{specify-representation}
Next we design @deftech{Exprs-unsafe-data-lang v7}.
We replace bits with proper data types, and lift the restriction on
@exprs-unsafe-data-lang-v7[if] expressions, which are now properly algebraic.

@bettergrammar*-diff[exprs-bits-lang-v7 exprs-unsafe-data-lang-v7]

We assume all operations are well-typed in this language, and implement dynamic
checks later.
To make this clear, we prefix all the operators that require a dynamic check
with @exprs-unsafe-data-lang-v7[unsafe-].
@exprs-unsafe-data-lang-v7[call] is still unsafe, since we do not know how to
tag functions yet.

First, we translate each value literal to @tech{ptrs}.

For booleans, empty, and void, this is trivial.
We simply emit their @tech{ptr} representation; you can find some parameters for
this defined in @racketmodname[cpsc411/compiler-lib].

For data types with a payload (fixnum and ASCII characters) we need to do some
work to merge the payload data with the tag.
The general strategy is to first left shift the data by the number of bits in
the tag, then perform an inclusive or with the tag.
@codeblock{
(bitwise-ior (arithmetic-shift 7 3) #b000)
(bitwise-ior (arithmetic-shift (char->integer #\x) 8) #b00101110)
}
Note that because the fixnum tag is all 0s, we can omit the bitwise or.
@codeblock{
(arithmetic-shift 7 3)
}

Remember not to use magic numbers in your compiler, and instead use appropriate
parameters so we can change tags and masks easily later.

The complicated cases are for operations on numbers, but even these are mostly
unchanged due to some handy algebraic facts.
Recall that every fixnum @racket[n] is represented by a @tech{ptr} whose
value is @racket[(* 8 n)].
For @exprs-bits-lang-v7[+] and @exprs-bits-lang-v7[-], this means we don't need to do anything
at all, since @tt{8x + 8y = 8(x + y)}, and similarly @tt{8x - 8y =
8(x - y)}.
Similarly, @exprs-bits-lang-v7[<], @exprs-bits-lang-v7[<=], @exprs-bits-lang-v7[>],
@exprs-bits-lang-v7[>=], and @exprs-bits-lang-v7[eq?] all work unchanged on
@tech{ptrs}.
However, these are boolean operations in @tech{Exprs-data-lang v7}, so
their implementation must return a boolean @tech{ptr}.

Only @exprs-bits-lang-v7[*] poses a problem, since @tt{8x * 8y = 64(x * y)}.
However, we do not need to adjust both arguments: we observe that @tt{8x *
y = 8(x * y)}, and similarly @tt{x * 8y = 8(x * y)}.
We only need to shift one operand before performing @exprs-bits-lang-v7[*] to get the
correct result as a @tech{ptr}.
If either argument is constant, we can perform the shift at compile time,
completely eliminating the additional overhead.
Otherwise, we translate @exprs-bits-lang-v7[(* e_1 e_2)] to (roughly) @exprs-bits-lang-v7[(*
e_1 (arithmetic-shift-right e_2 3))].

Next, we translate @exprs-bits-lang-v7[if], which should be translated from an
operation on booleans to an operation on @exprs-bits-lang-v7[pred]s.
We'll do some work later transforming booleans into predicates, for
optimization, but for now we just consider how to implement booleans correctly.
Racket and Scheme are falsey languages---any thing that is not @racket[#f] is
considered true.
We can implement this naively: simply compare to the @tech{ptr} for @racket[#f].
Recall from earlier that our representation allows us to treat anything that is
not false as true by a simple @exprs-bits-lang-v7[bitwise-xor] and comparison to
0, but we might want to leave that for a more general optimization.

When translating the booleans @exprs-bits-lang-v7[unops] and @exprs-bits-lang-v7[binops]
@exprs-bits-lang-v7[binops]
on @tech{ptrs}, we need to produce something that the translation of
@exprs-bits-lang-v7[if] can consume.
@exprs-bits-lang-v7[if] is expecting a boolean value, so each
@exprs-bits-lang-v7[unop] should be translated to an expression that returns a
boolean.
As we saw earlier, type predicates are implemented by masking the @tech{ptr}
using @exprs-bits-lang-v7[bitwise-and], and comparing the result to the tag using
@exprs-bits-lang-v7[=].
But the target language @exprs-bits-lang-v7[=] is a relop, not a boolean operation,
so we translate @exprs-bits-lang-v7[(fixnum? e)] to
@;@exprs-bits-lang{(if (eq? (bitwise-and e #b111) #b000) ##b00001110 #b00000110)}.
@exprs-bits-lang-v7[(if (= (bitwise-and e #b111) #b000) #t #f)].
Our representation of booleans supports optimizing this, as described earlier,
but we should leave that optimization for a separate pass.

@defproc[(specify-representation [p exprs-unsafe-data-lang-v7?])
         exprs-bits-lang-v7?]{
Compiles immediate data and primitive operations into their implementations as
@tech{ptrs} and primitive bitwise operations on @tech{ptrs}.
}

Next we design @deftech{Exprs-unique-lang v7}, which exposes a uniform safe
interface to our language with immediate data.
It exposes dynamically checked versions of each unsafe operation,
hides the predicate sub-language from the user, and exposes all primitive
operations as functions which should be used with @object-code{call} in the
source language.

To implement this language, we essentially "link" the definitions of each
procedure wrapper for each primitive operation and replace the
reserved @exprs-unsafe-data-lang-v7[prim-f] names for the functions with the
appropriate fresh labels.
Since our compiler as not provided any means of linking separately compiled
modules, we implement this by adding new definitions to the module.
Each safe function should raise a different error code depending on which
operation was attempted, and which argument was not well-typed.
Be sure to document your error codes.

@bettergrammar*-diff[exprs-unsafe-data-lang-v7 exprs-unique-lang-v7]

In @tech{Exprs-unique-lang v7}, most ill-typed expressions are valid
programs.
For example, @exprs-unique-lang-v7[(+ #t (eq? 5 (void)))] is a valid program.
The only invalid programs are those that attempt to @exprs-unique-lang-v7[apply] a
non-function, or use a label in any position except the first operand of
@exprs-unique-lang-v7[apply]; a limitation we will solve in the coming chapters.

@defproc[(implement-safe-primops [p exprs-unique-lang-v7?])
          exprs-unsafe-data-lang-v7?]{
Implement safe primitive operations by inserting procedure definitions for each
primitive operation which perform dynamic tag checking, to ensure type safety.
}

@subsection{uniquify}
Last but not least, we update uniquify.
The source language, @deftech{Exprs-lang v7}, is defined below.

@bettergrammar*-diff[exprs-unique-lang-v7 exprs-lang-v7]

You are allowed to shadow @exprs-unique-lang-v7{prim-f}s.

@defproc[(uniquify [p exprs-lang-v7])
         exprs-unique-lang-v7]{
Resolves all @ch2-tech{lexical identifiers} to @ch2-tech{abstract locations}.
}

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v7-graph" "Overview of Compiler Version 7" v7-graph]

@section{Appendix: Languages}

@declare-exporting[cpsc411/langs/v7]

@deflangs[
exprs-lang-v7
exprs-unique-lang-v7
exprs-unsafe-data-lang-v7
exprs-bits-lang-v7
exprs-bits-lang-v7/contexts
values-bits-lang-v7
proc-imp-mf-lang-v7
imp-mf-lang-v7
imp-cmf-lang-v7
asm-pred-lang-v7
asm-pred-lang-v7/locals
asm-pred-lang-v7/undead
asm-pred-lang-v7/conflicts
asm-pred-lang-v7/pre-framed
asm-pred-lang-v7/framed
asm-pred-lang-v7/spilled
asm-pred-lang-v7/assignments
nested-asm-lang-fvars-v7
nested-asm-lang-v7
block-pred-lang-v7
block-asm-lang-v7
para-asm-lang-v7
paren-x64-v7
paren-x64-rt-v7
]
