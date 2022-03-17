#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a7-solution)
  cpsc411/reference/a7-solution
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v6
  cpsc411/langs/v6-5
  cpsc411/langs/v7
  (for-label cpsc411/langs/v7))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a7-solution]

@(define sb
   (make-cached-eval
    "ch7-eval"
    '(require racket/pretty cpsc411/reference/a7-solution cpsc411/compiler-lib cpsc411/langs/v7)))

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
L3 [label="Imp-mf-lang v7"];
L2 [label="Proc-imp-cmf-lang v7"];
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

@(define (ch-ae-tech . rest)
  (apply tech #:tag-prefixes '("book:" "chp-ae:") rest))

@section{Preface: What's wrong with @ch-ae-tech{Exprs-lang v6.5}}
@ch-ae-tech{Exprs-lang v6.5} gained the ability to nest expressions pretty much
arbitrarily.
However, our functions are still limited to work on machine integers.
A realistic language would allow us to express programs over more interesting
data types than mere machine integers.

Unfortunately, once we add data types, we have a problem distinguishing between
any two data types.
Everything is eventually represented as 64-bit integers in @ch1-tech{x64}.
We need some way to distinguish a collection of 64 bits as an integer, from a
boolean, from the empty list, from a pointer to a procedure.
Otherwise, we will have serious problems: undefined behaviours, unsafe casts,
and/or memory safety errors.

This is not only a problem for ruling out unsafe behaviour in the source
language.
A static type system could take care to prevent the user from calling an
integer as a procedure, for example.
However, the language itself may need to distinguish different kinds of data at
run time.
For example, a garbage collector may need to traverse data structures, but not
immediate data like integers; a pretty printer may want to print different data
differently.

To enable the language to distinguish different kinds of data, we can steal a
few of our 64 bits to represent a data type tag.
This limits the range of machine integers, but allows us to us to distinguish
data dynamically, enabling safety and abstractions that must differentiate data
dynamically.

Our goal in this assignment is to implement the following language,
@tech{Exprs-lang v7}.

@bettergrammar*-ndiff[
#:labels ("Exprs-lang v7" "Diff vs v6.5")
(exprs-lang-v7)
(exprs-lang-v6.5 exprs-lang-v7)
]

We add a bunch of new values in @tech{Exprs-lang v7}, including booleans, the
empty list, the void object, (printable) ASCII character literals, and an error
value indicating an exit code.
@margin-note{We restrict ASCII characters to the printable ones, so we don't
have to figure out how to print non-printable characters.
The run-time system will work with some non-printable characters, but the
results will not be converted to Racket properly.
}

We want to allow @tech{Exprs-lang v7} programs to return any of these values,
which requires additional support from the run-time system.
The run-time system must be able to distinguish these different values in order
to pretty print or otherwise return them to the user (such as via an exit code
in the case of the error value).
This means our run-time representation of values cannot just be 64-bit machine
integers; we need some way for the run-time system to differentiate values
dynamically.

@margin-note{
The support libraries include an implementation of this new run-time system in
@racketmodname[cpsc411/ptr-run-time], which supports pretty printing all the new
values.
@racketmodname[cpsc411/compiler-lib] also contains parsers for the values.
@racket[execute] takes a new optional second parameter, which can be used to
change your view of the result.
By default, it parsers the printed value as a Racket value.
You can pass @racket[nasm-run/print-string] to get back the string
representation of the result, which will be handy to view the result of
@exprs-lang-v7[(void)] correctly.
You can pass @racket[nasm-run/exit-code] to get the exit code, which is helpful
for viewing the result of @exprs-lang-v7[(error uint8)], which sets the exit code
to @exprs-lang-v7[uint8].
}

We also add new primitive operations, primarily predicates on our new data
types.
The interpretation of several operations will also change to add dynamic
type checking.
This will prevent those operations from being a source of undefined behaviour.

With proper booleans, we can finally allow an arbitrary value in the predicate
position of an @exprs-lang-v7[if] expression in the surface language.

@section[#:tag "intro-to-tags"]{Introduction to Tagged Object Representation}
The key challenge we need to solve in adding data types to our language is how
to implement data-type checking.
That is, given some value @exprs-lang-v7[triv], branch on whether it is a
particular type data type such as a boolean or a number: @exprs-lang-v7[(if
(fixnum? triv) value_1 value_2)] and @exprs-lang-v7[(if (boolean? triv) value_1
value_2)] should be supported.
Ideally, each data type is unique: @exprs-lang-v7[(boolean? triv)] and
@exprs-lang-v7[(fixnum? triv)] should never both be true.

These don't necessarily need to be dynamic checks, as long as they are somehow
expressible by the compiler, whether as expressions that dynamically check or
meta-data that is statically checked.
However, even in a statically typed language, we may need some ability for the
run-time system to distinguish different kinds of data dynamically.

Depending on how these type checks are implement, this alone is enough to let us
implement different data types safely.
We could use procedural abstraction to wrap existing binary operations to
ensure, @eg that @exprs-lang-v7[+] is only operating on numbers, and not booleans.
We could expand @exprs-lang-v7[if] to check that the value in predicate position
is both a boolean (that is, that @exprs-lang-v7[(boolean? triv)] is equal to
some value using a @exprs-lang-v7[relop]), and that the value is equal to some
representation of true.

A common approach to efficiently represent word-sized data types is called
@deftech{object tagging}.
This lets us implement data-type checking by providing the following
primitive operations (which we implement with further, low-level primitives in
x64):
@itemlist[
@item{Tagging, @ie given some machine integer, tag it to indicate what data type
it represents, producing a tagged representation of the underlying data.
This tagged representation will happen to correspond to some machine integer,
since all sequences of bits do, but maybe not in any meaningful way.
}
@item{Untagging, @ie given some tagged representation, remove the tag returning
the underlying data that can be used with primitive x64 instructions.
}
@item{Tag checking, @ie given some tagged representation, get the tag or compare
the tag to something.
}
]
@todo{Make sure to introduce each of these aspects in this section.}

So far, we've treated all of our data as 64-bit integers.
Now, we treat all our data as 64 bits, which we manipulate to implement various
data types (including some fixed-sized integers).
We choose a particular representation of object tagging below that has some nice
properties, and implement each of these operation by expanding them into
bitwise operations.
@margin-note{Some of our clever tag choices, and some terminology, is borrowed
from R. Kent Dybvig.
You can learn more about it's use in Chez Scheme from this talk by his former
PhD student Andy Keep: @url{https://www.youtube.com/watch?v=BcC3KScZ-yA}.
}

Each data type in our language will now be represented by a @deftech{ptr}
(pronounced like @emph{footer}).
A @tech{ptr} is a machine word whose @racket[n] least-significant bits represent
the @deftech{primary tag}, and whose upper @racket[(- (* 8
(current-word-size-bytes)) n)] bits represent the data.

In our setting, we have 64-bit words and will use the 3 least-significant bits
for @tech{primary tags}.
With 3 bits, we can represent 8 primary data types in a @tech{ptr}.
We want to reserve these for the most common and most performance critical data
that can fit in a @tech{ptr}, but we have additional constraints.
This will limit the size of data we can represent, but gives us additional data
types---probably a good trade off.

If we want more than 8 data types, which most languages support, we must reserve
one tag for "everything else" (a pointer to memory).
The structure in memory has infinite space in which to store additional tags.
This seems to leave us only 7 possible data types without going to memory.

Some data types require fewer than 64 bits, and we can exploit this fact to
gain a few extra data types.
For example, booleans really only require 1 bit.
The empty list and the void object only require a tag, since they do not contain
any data.
ASCII characters only require 8 bits.
We can make all of these share a single @tech{primary tag}, and steal some of
the unnecessary high-order bits for a @deftech{secondary tag}.
This gives us 4 data types for the price of only 1 @tech{primary tag}---what a
bargain!

This leaves us 6 @tech{primary tags}.
One obvious choice is to use one for fixed sized integers; these will now be
61-bit integers, which we call @exprs-lang-v7[fixnum]s.
Integer operations are fast, and we don't want to slow them down by making them
go through heap allocation and pointer indirection.
If we wanted to support graphics and scientific operations, we would reserve one
for floating-point numbers too.

We reserve the remaining @tech{primary tags} for @deftech{tagged pointers},
pointers to common heap-allocated structures.
Storing the tag on the pointer instead of in memory avoids the expense of an
additional memory access to check a tag and the additional memory overhead of
storing the tag in memory.
We'll address the implementation of tagged pointers and heap-allocated
structured data in the next chapter.

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
@item{@code{#b00111110}, for the error exit-code value}
]
@margin-note{For annoying technical reasons, our representation of the empty
list differs slightly from Racket.
We use @exprs-lang-v7[empty], while Racket uses @racket['()].}


The integer 7 is @code{#b111} in base 2, and would be represented, in base 2, as
the @tech{ptr} @code{#b111000}.
The three low-order bits are the tag @code{#b000}, and the high-order bits are
the data @code{#b111} (implicitly padded with 0 to 64-bits).

To implement untagging, we can simply right-shift the @tech{ptr} by the size of
a @tech{primary tag} to get a two's complement integer:
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
This fact allows for implementing fast fixnum operations on the @tech{ptr}
representation directly.
That is, we can implement binary operations on fixnums without detagging
followed by tagging, by relying on some algebraic facts about operations on
integers.

Similarly, the ASCII character 7 would be as a @tech{ptr}, in base 2,
@code{#b011011100101110}.
@margin-note{See
@url{https://en.wikipedia.org/wiki/ASCII#Printable_characters} for the
representation of ASCII characters.
}
For character operations, to detag we right-shift by the size of a
@tech{secondary tag} (8).
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
We can think of booleans either as the 61-bit values 0 and 1, with
tag @code{#b110}, or as two separate @tech{secondary tags} (with no payload)
@code{#b00001110} and @code{#b00000110}.
I typically think in the the latter interpretation, where we represent
@exprs-lang-v7[#t] and @exprs-lang-v7[#f] as separate data types, whose tags are
@code{#b110} (@racket[#b110]) and @code{#b1110} (@racket[#b1110]).

To implement tag checking for booleans, we use the mask @code{#b11110111}
(@racket[#b11110111]), and compare to the tag @code{#b110}. This succeed
regardless of whether the fourth bit is set, as long as all the
other @tech{secondary tag} bits are 0.
@examples[
(eq? (bitwise-and #b00001110 #b11110111) #b110)
(eq? (bitwise-and #b00000110 #b11110111) #b110)
]

To extract the payload for booleans, all we really need to know is whether the
boolean is false.
We can perform this test without untagged, merely by comparing to @code{#b110}
(with all other bits 0, implicitly).
This gives us the same result as explicitly untagging, and compares to 0.
@examples[
(eq? (arithmetic-shift #b00000110 -3) 0)
(eq? (arithmetic-shift #b00001110 -3) 0)
(eq? #b00000110 #b110)
(eq? #b00001110 #b110)
]
To implement branching on booleans in terms of predicates, we can compile a
boolean @exprs-lang-v7[value] to the predicate @exprs-lang-v7[(not (!= value
#b110))].

The representation of @exprs-lang-v7[error], which represents an exit code, is
inefficient.
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

To implement @tech{ptrs}, we need bitwise operations such as
@racket[arithmetic-shift] so that we can implement tag checking, tagging, and
untagging.
This means we need to expose bitwise operations from @ch1-tech{x64}, all the way
through the compiler pipeline.
This is not very interesting, so we begin by implementing tagged objects in
terms of these, and then extending the rest of the compiler to expost bitwise
operations.

@section{Extending the source and front-end passes}
We start by deiscussing the design of @deftech{Exprs-lang v7}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v6.5" "Exprs-lang v7")
(exprs-lang-v6.5 exprs-lang-v7)
(exprs-lang-v7)
]

As usual, we allow arbitrary shadowing.
This means users can shadow @exprs-unique-lang-v7[prim-f]s.
@examples[#:eval sb
(interp-exprs-lang-v7
  '(module
     (let ([* 1] [- 2])
       (call + * -))))
]

Now, all operations on data types in the source are procedures.
This is because we'll need to implement binary operations by detagging,
operating on the underlying data, and retagging.
For safety, we also should perform dynamic tag checking, to ensure we never add
booleans or characters, or other odd and undefined behaviour.
@examples[#:eval sb
(interp-exprs-lang-v7
 '(module
    (call * #f #\y)))
]

We could distinguish procedure calls from primitive operations, and implement
primitive operations by expanding to expressions, but this risks duplicating
code (although, not much code).
But, at some point, primitive operations will need to be wrapped as procedures
if we want to use them with functional abstractions, so we might as well do it
now, and leave some other pass to implement procedure inlining.

We also expose the tag checking operations to the source, again as procedures.
Exposing these is a design choice, and this choice moves us toward a dynamically
typed source language.
@examples[#:eval sb
(interp-exprs-lang-v7
 '(module
    (call fixnum? #t)))
(interp-exprs-lang-v7
 '(module
    (call fixnum? 5)))
]

These choices mean we can change our validator, @racket[check-exprs-lang].
We no longer need to type check the arguments (previously, operands) to
primitive operations, except to @exprs-lang-v7[call], which still must call a
statically known @ch5-tech{procedure} (since we don't have a tagged
representation of @ch5-tech{procedures}, yet).
Instead, we statically allow expressions like @exprs-lang-v7[(call * #f #\y)],
but expect this to raise a dynamic error.

@nested[#:style 'inset
@defproc[(check-exprs-lang [p any/c])
         exprs-lang-v7]{
Checks that a @tech{Exprs-lang v7} program is well typed (only that procedures
are called with the right number of arguments), and well scoped.
}
]

We extend @racket[uniquify] as usual.
Below, we design @deftech{Exprs-unique-lang v7}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v6.5" "Diff vs Source" "Exprs-unique-lang v7")
(exprs-unique-lang-v6.5 exprs-unique-lang-v7)
(exprs-lang-v7 exprs-unique-lang-v7)
(exprs-unique-lang-v7)
]

As usual, we transform names into either @ch2-tech{abstract locations} or labels.
Data types do not complicate this.

@nested[#:style 'inset
@defproc[(uniquify [p exprs-lang-v7])
         exprs-unique-lang-v7]{
Resolves top-level @ch3-tech{lexical identifiers} into unique labels, and all
other @ch3-tech{lexical identifiers} into unique @ch2-tech{abstract locations}.
}
]

And now, we figure out where to place our new passes.

As we saw in @secref{intro-to-tags}, many of the operations we want to perform
on @tech{ptrs} are easily expressed as algebraic expressions.
For example, the expression @exprs-lang-v7[(call fixnum? 7)] is compiled to as
@exprs-bits-lang-v7[(= (bitwise-and 7 #b111) #b000)].

Thankfully, we just added @tech{algebraic expressions}.
If we position our new passes @emph{above} @racket[remove-complex-opera*], we
don't need to manually introduce auxiliary variables, or generate code with
additional let-bindings.
The compiler will do this for us, and allow us to write the code we want to
write.

@section{Specifying Data Type Representation}
First we design we design @deftech{Exprs-unsafe-data-lang v7}.
In this language, we compile our dynamically typed safe primitive procedures
into lower level primitive operations on data types.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Exprs-unsafe-data-lang v7")
(exprs-unique-lang-v7 exprs-unsafe-data-lang-v7)
(exprs-unsafe-data-lang-v7)
]

To make clear the distinction between the source and target implementations of
these operations, we prefix the unsafe operators that require a dynamic check
with @exprs-unsafe-data-lang-v7[unsafe-].
@exprs-unsafe-data-lang-v7[call] is still unsafe, since we do not know how to
tag procedures yet.

To implement this language, we essentially "link" the definitions of each
procedure wrapper for each primitive operation and replace the reserved
@exprs-unsafe-data-lang-v7[prim-f] names for the functions with the appropriate
fresh labels.
Since our compiler has not provided any means of linking separately compiled
modules, we implement this linking by directly adding new definitions to the
module.
For example, the program:
@exprs-unique-lang-v7-block[
(module (call + 1 2))
]
should compile to the something equivalent to the following (although perhaps
with different error codes).
@exprs-unsafe-data-lang-v7-block[
#:datum-literals (L.+.1 tmp.3 tmp.4)
(module
  (define L.+.1
    (lambda (tmp.3 tmp.4)
      (if (fixnum? tmp.4)
          (if (fixnum? tmp.3)
              (unsafe-fx+ tmp.3 tmp.4)
              (error 2))
          (error 2))))
  (call L.+.1 1 2))
]

Each safe procedure should produce some error code indicating that kind of error
that ocurred.
Be sure to document your error codes.
For now, we have enough error codes that we can encode which operation and which
argument caused the error.

@defproc[(implement-safe-primops [p exprs-unique-lang-v7?])
          exprs-unsafe-data-lang-v7?]{
Implement safe primitive operations by inserting procedure definitions for each
primitive operation which perform dynamic tag checking, to ensure type safety.
}

@digression{
To avoid producing unnecessary definitions, you may consider how to design this
pass so that a definition for a primitive procedure is only added to the module
if that procedure is actually used.

You may also consider an alternative implementation of this pass that avoids
inserting dynamic checks at all.
Note that the the language definitions allow this behaviour for all well-typed
programs.
}

Next we design @deftech{Exprs-bits-lang v7}, a language with all values
represented as 64 bits and only primitive bitwise operations, but that allows
algebraic expressions in most positions.
The predicate position of @exprs-bits-lang-v7[if] is again restricted to the
predicate sublanguage.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Exprs-bits-lang v7")
(exprs-unsafe-data-lang-v7 exprs-bits-lang-v7)
(exprs-bits-lang-v7)
]

This is the hard part of adding data types---translating our data types and
their operations into bits and primitive opertions on bits.

First, we translate each value literal to a @tech{ptr}.

For booleans, empty, and void, this is trivial.
Since these contain no payload data, their @tech{ptr} representation is just a
statically known tag.
We simply emit their @tech{ptr} representation.
@margin-note{You can find some parameters for @tech{ptrs} defined in
@racketmodname[cpsc411/compiler-lib].}

For data types with a payload (fixnum and ASCII characters) we need to do some
work to merge the payload data with the tag.
The general strategy is to first left shift the data by the number of bits in
the tag, then perform a bitwise inclusive or with the tag.
After the left shift, the least significant bits will be all 0, and a bitwise
inclusive or will simply set the tags bits.
For example, to tag the character "x" as an ASCII character, we would shift left
by 8 (the number of bits in the @tech{secondary tag}), and bitwise or with the tag:
@codeblock{
(bitwise-ior (arithmetic-shift (char->integer #\x) 8) #b00101110)
}
For a fixnum, which uses a 3 bit @tech{primary tag}, we could shift left by 3
and bitwise or with the fixnum tag:
@codeblock{
(bitwise-ior (arithmetic-shift 7 3) #b000)
}

Similarly, to implement operations on our @tech{ptrs}, in general we need to
untag them.
We can untag easily be performing an arithmetic right shift by the size of the
tag.
For example, below we tag the character @racket[#\x], producing a tagged
@tech{ptr} representation.
To untag it, we simply shift right by 8 (the length of the @tech{secondary
tag}), which is implement by shift by -8.
@examples[#:eval sb
(bitwise-ior (arithmetic-shift (char->integer #\x) 8) #b00101110)
(integer->char
 (arithmetic-shift
  (bitwise-ior (arithmetic-shift (char->integer #\x) 8) #b00101110)
  -8))
]

For fixnum operations, we can avoid some of these steps, due to our choice of
tag.
Because the fixnum tag is all 0s, we can omit the bitwise or. @examples[#:eval
sb
(= (bitwise-ior (arithmetic-shift 7 3) #b000) (arithmetic-shift 7 3))
]

For operations on fixnums, we can use some handy algebraic facts to avoid some
tagging and untagging.
Recall that every fixnum @racket[n] is represented by a @tech{ptr} whose
value is @racket[(* 8 n)].
For @exprs-bits-lang-v7[+] and @exprs-bits-lang-v7[-], this means we don't need to do anything
at all, since @tt{8x + 8y = 8(x + y)}, and similarly @tt{8x - 8y =
8(x - y)}.
Similarly, @exprs-bits-lang-v7[<], @exprs-bits-lang-v7[<=], @exprs-bits-lang-v7[>],
@exprs-bits-lang-v7[>=], and @exprs-bits-lang-v7[=] can all work on @tech{ptrs}
directly without untagging.
However, these are boolean operations in @tech{Exprs-unsafe-data-lang v7}, so
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
We can implement this easily: simply compare to the @tech{ptr} for @racket[#f].
@codeblock{
(= value #,(current-false-ptr))
}
@;Recall from earlier that our representation allows us to treat anything that is
@;not false as true by a simple @exprs-bits-lang-v7[bitwise-xor] and comparison to
@;0, but we might want to leave that for a more general optimization.

When translating the booleans @exprs-bits-lang-v7[unops] and @exprs-bits-lang-v7[binops]
on @tech{ptrs}, we need to produce something that the translation of
@exprs-bits-lang-v7[if] can consume.
@exprs-bits-lang-v7[if] is expecting a boolean value, so each
@exprs-bits-lang-v7[unop] should be translated to an expression that returns a
boolean.
As we saw earlier, type predicates are implemented by masking the @tech{ptr}
using @exprs-bits-lang-v7[bitwise-and], and comparing the result to the tag using
@exprs-bits-lang-v7[=].
But the target language @exprs-bits-lang-v7[=] is a relop, not a boolean operation,
so we translate @exprs-unsafe-data-lang-v7[(fixnum? value)] to
@;@exprs-bits-lang{(if (eq? (bitwise-and e #b111) #b000) ##b00001110 #b00000110)}.
@exprs-bits-lang-v7[(if (= (bitwise-and value #b111) #b000) #b00001110 #t #f)]
(although, of course, @exprs-bits-lang-v7[#t] and @exprs-bits-lang-v7[#f] must
be compiled to @tech{ptrs} too).
@;Our representation of booleans supports optimizing this, as described earlier,
@;but we should leave that optimization for a separate pass.

@defproc[(specify-representation [p exprs-unsafe-data-lang-v7?])
         exprs-bits-lang-v7?]{
Compiles immediate data and primitive operations into their implementations as
@tech{ptrs} and primitive bitwise operations on @tech{ptrs}.
}

@section{Exposing Bitwise Operations}
Specifying the tagged representation happens very close to our source language.
We need to expose these bitewise operations all the way up to what was
previously @ch6-tech{Values-unique-lang-v6}.
Below, we design the new @deftech{Values-bits-lang v7}, typeset with differences
compared to @ch6-tech{Values-unique-lang-v6}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v6" "Diff vs Source" "Values-bits-lang-v7")
(values-unique-lang-v6 values-bits-lang-v7)
(exprs-bits-lang-v7 values-bits-lang-v7)
(values-bits-lang-v7)
]

The new operations do not have large effects on the language designs or compiler
passes between @racket[remove-complex-opera*] and @racket[generate-x64], so
these details are left unspecified in this chapter.
Redesigning the intermediate languages with the new operations is left as an
exercise for the reader.

@margin-note{
A specification of these languages is given in the support library, if you wish
to use it.
}

@nested[#:style 'inset
@defproc[(remove-complex-opera* [p exprs-bits-lang-v7?])
         values-bits-lang-v7?]{
Performs the monadic form transformation, unnesting all non-trivial operators
and operands to @exprs-bits-lang-v7[binop]s, @exprs-bits-lang-v7[call]s, and
@exprs-bits-lang-v7[relops]s, making data flow explicit and and simple to
implement imperatively.
}]

@section{Exposing Bitwise Operations in Paren-x64}

Finally, we need to expose the following @ch1-tech{x64} instructions.
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

We add each of these operations as a @paren-x64-v7[binop] to @deftech{Paren-x64
v7} below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v6 (excerpts)" "Paren-x64 v7")
(#:exclude (trg reg addr fbp relop int64 int32 dispoffset label) paren-x64-v6 paren-x64-v7)
(paren-x64-v7)
]

@nested[#:style 'inset
@defproc[(generate-x64 [p paren-x64-v7?])
         (and/c string? x64-instructions?)]{
Compile the @tech{Paren-x64 v7} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}]

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v7-graph" "Overview of Compiler Version 7" v7-graph]
