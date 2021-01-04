#lang reader "assignment-lang.rkt"
@(require cpsc411/deprecated/a7-compiler-lib)

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@;TODO abstractions for each of these; no more copy/paste/modify
@(define eg
   (make-cached-eval
     "a7-eval"
     '(require
     cpsc411/v1-reference/a7-solution
     cpsc411/deprecated/a7-compiler-lib
     racket/pretty)
     '(current-stack-size 512)))

@title[#:tag "top" #:tag-prefix "a7:"]{Compiler 7: Immediate Data Types}

@section{Assignment Summary}

The goal of this assignment is to introduce data type representation.
We will finally add booleans, as well as the empty list, the void object,
characters, and an error value.
We will introduce a tagged object representation to distinguished different data
types dynamically.
This will let us implement dynamic tag checking, reducing the amount of
undefined behaviour in our surface language.

This assignment is due Friday, March 13, 2020 at 11:59pm.

You can use the reference solution here, but if you hammer it it might go down
and not come back up.
@url{https://www.students.cs.ubc.ca/~cs-411/2019w2/a7-interrogator.cgi}

@todo{Explicit learning objectives}

@subsubsub*section{Assignment Checklist}
You should find a new repository in your
@url{https://github.students.cs.ubc.ca} account named @tt{a7_<team ids>} with a
code skeleton and a support library.
You should complete the assignment in that git repository and push it to the
GitHub Students instance.

You should first merge your solution to @secref[#:tag-prefixes '("a6:")]{top}
with the starter code provided.
The new starter code has the correct provides and includes a submodule to help
you run your compiler on the command line if you want.
The name of the skeleton is @share{a7-skeleton.rkt} to avoid accidentally
overwriting your files, but your file in the Git repository should be named
@tt{a7.rkt}.

@emph{Completely new passes}
@itemlist[
@item{@racket[a-normalize]}
@item{@racket[specify-representation]}
@item{@racket[implement-safe-primops]}
]

@emph{Minor modifications to passes}
@itemlist[
@item{@racket[uniquify]}
@item{@racket[select-instructions]}
]

@emph{No modifications to other passes} (if your compiler has been designed with
good abstractions).

@section{Language Diagram}

@dot->svg{
digraph {

node [ shape="box" ]

/* The Languages */

LX0 [label="Exprs-lang v7"]
LX1 [label="Exprs-safe-data-lang v7"]
LX2 [label="Exprs-data-lang v7"]
LX3 [label="Exprs-bits-lang v7"]
LX4 [label="Values-bits-lang v7"]
LX5 [label="Block-lang v7"];

L3 [label="Block-locals-lang v7"];
L4 [label="Undead-block-lang v7"];
L5 [label="Conflict-lang v7"];
L5a1 [label="Pre-framed-lang v7"];
L5a2 [label="Framed-block-lang v7"];
L5b [label="Spilled-lang v7"];
L6 [label="Block-jump-live-lang v7"];
L7 [label="Block-assigned-lang v7"];
L7b [label="Block-fvar-lang v7"];
L8 [label="Block-nested-lang v7"];
L9 [label="Block-asm-lang v7"];
L10 [label="Paren-asm v7"];

L11 [label="Paren-x64 v7"];
L13 [label="x64"];


/* The Passes */

edge [fontname="Courier"]

LX0 -> LX1 [label=" uniquify"];
LX1 -> LX2 [label=" implement-safe-primops"];
LX2 -> LX3 [label=" specify-representation"];
LX3 -> LX4 [label=" a-normalize"];
LX4 -> LX5 [label=" select-instructions"];

LX5 -> L3 [label=" uncover-locals"];
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

L10 -> L11 [label=" patch-instructions"];

L11 -> L13 [label=" generate-x64"];
}

}


@section{Preface: What's Wrong with Values-lang v6}
@a6-tech{Values-lang v6} gained the ability to express non-tail calls, an
important step forward in writing real programs.
However, our functions are still limited to work on machine integers.
We cannot write many interesting programs without more data types.

Unfortunately, once we add data types, we have a problem distinguishing between
any two data types.
Everything is eventually represented as 64-bit integers in @a0-tech{x64}.
We need some way to distinguish @object-code{0} as an integer, from a boolean,
from the empty list, from a pointer to a function.
Otherwise, we will have serious problems with undefined behaviour (like C).

One way to do this is to add a static type system.
This will certainly restrict undefined behaviour, but may impose a higher cost
on programming.
Due to Rice's Theorem@margin-note*{Still a huge buzz kill.}, we will never be
able to decide whether an arbitrary program is well typed.
Instead, we will be required to approximate and/or require user annotations,
ruling out some well-behaved programs to ensure no undefined behaviour.

Another way is to add dynamic checking.
This imposes a run-time cost, but frees the programmer from proving absence of
undefined behaviour, and allows the language implementation to catch only those
errors that would actually have resulted in undefined behaviour.

We will follow Scheme and Racket's (and JavaScript and Python and ...'s)
approach and use dynamic checking.

There's a second limitation in @a6-tech{Values-lang v6} that we will lift this
week.
So far, we do not have algebraic expressions---we must manually bind all
intermediate computations to names.
This is particularly tedious since we know how to write a compiler, which ought
to do the tedious work of binding expressions to temporary names.
After we have proper data types with dynamic checking, we can more easily nest
expressions, particularly in the predicate position of an @object-code{if}.

Our goal in this assignment is to implement the following language,
@tech{Exprs-lang v7}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define x (lambda (x ...) e))]
[e     v
       (apply e e ...)
       (let ([x e]) e)
       (if e e e)]
[x     name prim-f]
[v     fixnum x #t #f () (void) (error uint8) ascii-char-literal]
[prim-f binop unop]
[binop  * + - eq? < <= > >=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
]

@;Our goal will in this assignment is to support following language,
@;@deftech{Values-lang v7}
@;
@;@racketgrammar*[
@;  [p     (module b ... e)]
@;  [b     (define x (lambda (x ...) e))]
@;  [e     n
@;         (let ([x n]) e)
@;         (if v e e)]
@;  [n     v (primop v ...) (apply v v ...)]
@;  [x     name]
@;  [v     fixnum x #t #f () (void) (error uint8) ascii-char-literal]
@;  [primop * + - fixnum? boolean? empty? void? ascii-char? eq? not < <= >
@;          >=]
@;]

@racketblock[
(define (ascii-char-literal? x)
  (and (char? x) (<= 40 (char->integer x) 176)))

(define (fixnum? x)
  (int-size? 61 x))

(define (uint8? x)
  (<= 0 x 255))
]

We add a bunch of new values in @tech{Exprs-lang v7}, including booleans, the
empty list, the void object, (printable) ASCII character literals, and an error
value.
@tech{Exprs-lang v7} programs are allowed to return any of these values.
@margin-note{We restrict ASCII characters to the printable ones, so we don't
have to figure out how to print non-printable characters.
The run-time system will work with some non-printable characters, but the
results will not be converted to Racket properly.
}

The new @share{a7-compiler-lib.rkt} features a run-time system that supports
printing the new values.
@racket[execute] takes a new optional second parameter, which can be used to
change your view of the result.
By default, you get back a Racket value.
You can pass @racket[nasm-run/print-string] to get back the string
representation of the result, which will be handy to view the result of
@object-code{(void)} correctly.
You can pass @racket[nasm-run/exit-code] to get the exit code, which is helpful
for viewing the result of @object-code{(error uint8)}, which sets the exit code
to @object-code{uint8}.

We also add new primitive operations, primarily predicates on our new data
types.
The interpretation of several operations will also change to add dynamic
type checking.
This will prevent those operations from being a source of undefined behaviour.

With proper booleans, we can finally allow an arbitrary value in the predicate
position of an @object-code{if} expression in the surface language.

It is not clear how to support new data types going top-down, so we begin this
assignment at the bottom.

@section[#:tag "intro-to-tags"]{Introduction to Tagged Object Representation}
A common approach to efficiently represent word-sized data types is called
@deftech{object tagging}.
@margin-note*{Some of our clever tag choices, and some terminology, is borrowed
from R. Kent Dybvig.
You can learn more about it's use in Chez Scheme from this talk by his former
PhD student Andy Keep: @url{https://www.youtube.com/watch?v=BcC3KScZ-yA}.
}
Each data type in our language will now be represented by a @deftech{ptr}
(pronounced like @emph{footer}).
A @tech{ptr} is a machine word whose @racket[n] least-significant bits represent
the @deftech{primary tag}, and whose upper @racket[(- word-size n)] bits
represent the data.

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

For the non-fixnum immediates, we use the following secondary tags.
Note that the 3 least-significant bits of secondary tags are the shared primary
tag.
@itemlist[
@item{@code{#b00000110}, for @object-code{#f}}
@item{@code{#b00001110}, for @object-code{#t}}
@item{@code{#b00010110}, for @object-code{()}}
@item{@code{#b00011110}, for @object-code{(void)}}
@item{@code{#b00101110}, for an ASCII character}
@item{@code{#b00111110}, for the error value}
]
@margin-note{For annoying technical reasons, our representation of the empty
list differs slightly from Racket.
We use @object-code{()}, while Racket uses @racket['()].}


The integer 7 is @code{#b111} in base 2, and would be represented, in base 2, as
the @tech{ptr} @code{#b111000}.
The three low-order bits are the tag @code{#b000}, and the high-order bits are
the data @code{#b111} (implicitly padded with 0 to 64-bit total).
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
porting your tests easier.

Similarly, the ASCII character 7 would be as a @tech{ptr}, in base 2,
@code{#b011011100101110}.
@margin-note{See
@url{https://en.wikipedia.org/wiki/ASCII#Printable_characters} for the
representation of ASCII characters.
}
For character operations, we must right-shift by the size of a secondary tag
(8).
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
Instead, we essentially represent @object-code{(true)} and @object-code{(false)}
as separate data types, whose tags are @code{#b110} (@racket[#b110]) and
@code{#b1110} (@racket[#b1110]).
It makes masking a boolean slightly more complex.
We use the mask @code{#b11110111} (@racket[#b11110111]), to succeed regardless
of whether the fourth bit is set.
@examples[
(eq? (bitwise-and #b00001110 #b11110111) #b110)
(eq? (bitwise-and #b00000110 #b11110111) #b110)
]
The benefit is that any immediate that is not @object-code{(false)}
can be interpreted as @object-code{(true)} using @object-code{bitwise-xor},
instead of converting to a boolean and using an arithmetic shift to compute the
value of the boolean.
Only @object-code{(false)} is 0 when "xor"'d with the non-fixnum immediate tag,
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

The representation of @object-code{error} is inefficient.
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
This means we need to expose bitwise operations from @a0-tech{x64}.

@section{Exposing Bitwise Operations in Paren-x64}

We expose the following @a0-tech{x64} instructions this week.
@itemlist[
@item{@verbatim{sar @object-code{loc}, @object-code{int}}

Perform arithmetic right-shift by @object-code{int} on @object-code{loc}.

This instruction requires that its second operand be an integer literal between
0 and 63, inclusive, @ie @racket[0 <= int <= 63].
We will assume this constraint in the intermediate languages, and
never expose this operation in @tech{Exprs-lang v7}.}

@item{@verbatim{and loc, triv}

Compute the bitwise "and" of @object-code{loc} and @object-code{triv}, storing
the result in @object-code{loc}.
Like with other binary operations, when @object-code{triv} is an integer, it
must be an @object-code{int32}.
}

@item{@verbatim{or loc, triv}

Compute the bitwise "inclusive or" of @object-code{loc} and @object-code{triv},
storing the result in @object-code{loc}.
Like with other binary operations, when @object-code{triv} is an integer, it
must be an @object-code{int32}.
}

@item{@verbatim{xor loc, triv}

Compute the bitwise "exclusive or" of @object-code{loc} and @object-code{triv},
storing the result in @object-code{loc}.
Like with other binary operations, when @object-code{triv} is an integer, it
must be an @object-code{int32}.
}
]

First, we add each of these operations as a @object-code{binop} to
@deftech{Paren-x64 v7} below.

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
[addr  (fbp + dispoffset)]
[binop * + - (unsyntax @bnf:add{bitwise-and})
       (unsyntax @bnf:add{bitwise-ior})
       (unsyntax @bnf:add{bitwise-xor})
       (unsyntax @bnf:add{arithmetic-shift-right})]
[cmp   neq? eq? < <= > >=]
]

@exercise{
Redesign and extend the implementation of the function @racket[generate-x64].
The source is @tech{Paren-x64 v7} and the target is sequence of @a0-tech{x64}
instructions, as a string.

This should be a small change if you've abstracted over @object-code{binops}.
}

@section{Exposing Bitwise Operations in Block-lang}
We need to expose these operations all the way up to @tech{Block-lang v7}.
We define the new @deftech{Block-lang v7} below.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  ((new-frames ((aloc ...) ...)))]
  [tail  (begin s ... tail)
         (jump trg loc ...)
         (if (cmp loc opand) tail tail)]
  [s     (return-point label tail)
         (set! loc triv)
         (set! loc (binop loc opand))]
  [binop * + - (unsyntax @bnf:add{bitwise-and})
         (unsyntax @bnf:add{bitwise-ior})
         (unsyntax @bnf:add{bitwise-xor})
         (unsyntax @bnf:add{arithmetic-shift-right})]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   rloc aloc]
  [rloc  reg fvar]
  [reg   _...]
  [cmp   neq? eq? < <= > >=]
]

The new operations do not have large effects on the language designs or compiler
passes between @racket[select-instructions] and @racket[generate-x64], so we
leave you to figure out most of the details.
You will need to redesign the intermediate languages with the new operations.

@exercise{
Redesign and extend the implementation of @racket[patch-instructions].
The source is @deftech{Paren-asm v7} and the target is @tech{Paren-x64 v7}.


You need not support a more general form of
@object-code{arithmetic-shift-right}: it cannot be implemented by simply
patching this instruction with a few moves to auxiliary locations as
before; the second operand must be an integer literal in the range 0 to 63
(inclusive).  @a0-tech{x64} does not provide a way to make that second operand
a location.}

@exercise{
Redesign and extend the implementations of all the following passes:
@itemlist[
@item{@racket[flatten-program]}
@item{@racket[expose-basic-blocks]}
@item{@racket[implement-fvars]}
@item{@racket[replace-locations]}
@item{@racket[discard-call-live]}
@item{@racket[assign-frame-variables]}
@item{@racket[assign-registers]}
@item{@racket[assign-frames]}
@item{@racket[pre-assign-frame-variables]}
@item{@racket[conflict-analysis]}
@item{@racket[undead-analysis]}
@item{@racket[uncover-locals]}
]

This should involve a change to a single function if your compiler is
well-designed.
}

@section{Specifying Data Type Representation}
@subsection{select-instructions}
Now that we have exposed the necessary primitives, we can specify the
representation of our new data types in bits.
For this, we design @deftech{Values-bits-lang v7}, which essentially corresponds
to @a6-tech{Values-unique-lang v6} extended with the new primitives.
This will be the target language of our new compiler pass.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[e     n
       (let ([aloc n]) e)
       (if (cmp v v) e e)]
[n     v (binop v v) (apply v v ...)]
[v     int64 label aloc]
[binop * + - (unsyntax @bnf:add{bitwise-and})
       (unsyntax @bnf:add{bitwise-ior})
       (unsyntax @bnf:add{bitwise-xor})
       (unsyntax @bnf:add{arithmetic-shift-right})]

[cmp   neq? eq? < <= > >=]
]

The only data type in @tech{Values-bits-lang v7} is BITS, 64 for them,
interpreted as an integer sometimes.

We assume that each @object-code{binop} is well-typed; they shouldn't be used
with labels as arguments.

@exercise{Redesign and extend the implementation of
@racket[select-instructions].
The source language is @tech{Values-bits-lang v7} and the target language is
@tech{Block-lang v7}.

This should not change much, but be sure you do not put the second operand to an
@object-code{arithmetic-shift-right} into an aloc.
}

@subsection{a-normalize}
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
They are still disallowed for the predicate position of an @object-code{if}
expression, since we cannot introduce algebraic @object-code{if} expressions
without booleans.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[e     v
       (binop e e)
       (apply e e ...)
       (let ([aloc e]) e)
       (if (cmp e e) e e)]
[v     int64 label aloc]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[cmp   neq? eq? < <= > >=]
]

You've already seen part of this transformation in class: the A-normal form
translation.
The idea is essentially to transform expressions like
@racket[`(apply ,e_1 ,e_2)] into
@racketblock[
`(let ([,x_10 ,v_10])
   ...
    (let ([,x_1 ,v_1])
      (let ([,x_20 ,v_20])
        ...
        (let ([,x_2 ,v_2])
          (apply ,x_1 ,x_2)))))
]
where the translation of @racket[_e_1] is
@racketblock[
`(let ([,x_10 ,v_10])
   ...
   v_1)
]
and the translation of @racket[_e_2] is
@racketblock[
`(let ([,x_20 ,v_20])
   ...
   v_2)
]

I recommended that you use a procedural accumulator, @ie write the
transformation in CPS.
This will turn your mind inside out, but after that, the code writes itself.

The translation of @object-code{if} is tricky.  There are two ways to
accomplish it, but one of them requires first-class functions, so we must use
the naive form for now.
You must essentially push the surrounding context into the branches of the
@object-code{if}:
@racketblock[
(equal?
  (a-normalize `(module (+ (if (eq? 1 1) 5 6) 5)))
  `(module
     (if (eq? 1 1)
         (+ 5 5)
         (+ 6 5))))
]
This is straightforward, particularly in CPS, and correct, but can cause
considerable code duplication.
We'll address that later.
@;Or, you can introduce a @deftech{join-point}, a function that abstracts over the
@;context and is called in both branches:
@;@racketblock[
@;(equal?
@;  (a-normalize `(module (+ (if (eq? 1 1) 5 6) 5)))
@;  `(module
@;     (define L.jp.1 (lambda (x) (+ 5 x)))
@;     (if (eq? 1 1)
@;         (apply L.jp.1 5)
@;         (apply L.jp.1 6))))
@;]
@;This introduces a function call, but it's a known function in tail position, so
@;it can be heavily optimized.

A-normal form was introduced in
@hyperlink["https://slang.soe.ucsc.edu/cormac/papers/pldi93.pdf"]{this paper},
which you might be interested in reading, and it is still of active interest in
the compiler construction and research community.

@exercise{Design and implement the function @racket[a-normalize].
The source language is @tech{Exprs-bits-lang v7} and the target language is
@tech{Values-bits-lang v7}.

If you want to handle join-points, it's more efficient and simpler if you use a
single mutable variable.
But @emph{be careful}; you will have to manually sequentialize part of your
code.

When transforming function calls, @object-code{(apply e e ...)}, be sure to
enforce left-to-right evaluation order.
}

@subsection{specify-representation}
Next we design @deftech{Exprs-data-lang v7}.
We replace bits with proper data types, and lift the restriction on
@object-code{if} expressions, which are now properly algebraic.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[e     v
       (primop e ...)
       (apply e e ...)
       (let ([aloc e]) e)
       (if e e e)]
[v     fixnum aloc label #t #f () (void) (error uint8) ascii-char-literal]
[primop binop unop]
[binop  unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
        unsafe-fx>=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
]

We assume all operations are well-typed in this language, and implement dynamic
checks later.
To make this clear, we prefix all the operators that require a dynamic check
with @object-code{unsafe-}.
@object-code{apply} is still unsafe, since we do not know how to tag functions
yet.

First, we translate each value literal to @tech{ptrs}.

For booleans, empty, and void, this is trivial.
We simply emit their @tech{ptr} representation; you can find some parameters for
this defined in @share{a7-compiler-lib.rkt}.

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
For @object-code{+} and @object-code{-}, this means we don't need to do anything
at all, since @tt{8x + 8y = 8(x + y)}, and similarly @tt{8x - 8y =
8(x - y)}.
Similarly, @object-code{<}, @object-code{<=}, @object-code{>}, @object-code{>=},
and @object-code{eq?} all work unchanged on @tech{ptrs}.
However, these are boolean operations in @object-code{Exprs-data-lang v7}, so
their implementation must return a boolean @tech{ptr}.

Only @object-code{*} poses a problem, since @tt{8x * 8y = 64(x * y)}.
However, we do not need to adjust both arguments: we observe that @tt{8x *
y = 8(x * y)}, and similarly @tt{x * 8y = 8(x * y)}.
We only need to shift one operand before performing @object-code{*} to get the
correct result as a @object-code{ptr}.
If either argument is constant, we can perform the shift at compile time,
completely eliminating the additional overhead.
Otherwise, we translate @object-code{(* e_1 e_2)} to (roughly) @object-code{(*
e_1 (arithmetic-shift-right e_2 3))}.

Next, we translate @object-code{if}, which should be translated from an
operation on booleans to the primitive compare-and-branch expression.
Racket and Scheme are falsey languages---any thing that is not @racket[#f] is
considered true.
We can implement this naively: simply compare to the @tech{ptr} for @racket[#f].
Recall from earlier that our representation allows us to treat anything that is
not false as true by a simple @object-code{bitwise-xor} and comparison to 0, but
we might want to leave that for a more general optimization.

When translating the booleans @object-code{unops} and @object-code{binops}
@object-code{binops}
on @tech{ptrs}, we need to produce something that the translation of
@object-code{if} can consume.
@object-code{if} is expecting a boolean value, so each @object-code{unop} should
be translated to an expression that returns a boolean.
As we saw earlier, type predicates are implemented by masking the @tech{ptr}
using @object-code{bitwise-and}, and comparing the result to the tag using
@object-code{eq?}.
But the target language @object-code{eq?} is a comparison flag, so we translate
@object-code{(fixnum? e)} to
@;@object-code{(if (eq? (bitwise-and e #b111) #b000) ##b00001110 #b00000110)}.
@object-code{(if (eq? (bitwise-and e #b111) #b000) #t #f)}.
Our representation of booleans supports optimizing this, as described earlier,
but we should leave that optimization for a separate pass.

@exercise{Design and implement the function @racket[specify-representation].
The source language is @tech{Exprs-data-lang v7} and the target language is
@tech{Exprs-bits-lang v7}.

Remember not to use magic constants, and instead use parameters for tags, shift
lengths, and masks.
See @share{a7-compiler-lib.rkt}, which defines some of these.}


@subsection{implement-safe-primops}

Next we design @deftech{Exprs-safe-data-lang v7}, which exposes only
dynamically checked versions of each unsafe operation.
It also only exposes operations as functions which should be used with
@object-code{apply} in the source language.
This pass should "link" the definitions of these function in, and replace the
reserved @object-code{primop} names for the functions with the appropriate fresh
labels.
Each safe function should raise a different error code depending on which
operation was attempted, and which argument was not well-typed.
Be sure to document your error codes.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[e     v
       (apply e e ...)
       (let ([aloc e]) e)
       (if e e e)]
[v     fixnum label prim-f aloc #t #f () (void) (error uint8) ascii-char-literal]
[prim-f binop unop]
[binop  * + - eq? < <= > >=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
]

In @tech{Exprs-safe-data-lang v7}, most ill-typed expressions are valid
programs.
For example, @object-code{(+ #t (eq? 5 (void)))} is a valid program.
The only invalid programs are those that attempt to @object-code{apply} a
non-function, or use a label in any position except the first operand of
@object-code{apply}; a limitation we will solve in the next assignment.

@exercise{
Design and implement the function @racket[implement-safe-primops].
The source language is @tech{Exprs-safe-data-lang v7} and the target language
is @tech{Exprs-data-lang v7}.
}

@subsection{uniquify}
Last but not least, we update uniquify.
The source language, @deftech{Exprs-lang v7}, is defined below.

@racketgrammar*[
[p     (module b ... e)]
[b     (define x (lambda (x ...) e))]
[e     v
       (apply e e ...)
       (let ([x e]) e)
       (if e e e)]
[x     name prim-f]
[v     fixnum x #t #f () (void) (error uint8) ascii-char-literal]
[prim-f binop unop]
[binop  * + - eq? < <= > >=]
[unop   fixnum? boolean? empty? void? ascii-char? error? not]
]

You are allowed to shadow @object-code{prim-f}s.

@exercise{
Redesign and extend the implementation of @racket[uniquify].
The source language is @tech{Exprs-lang v7} and the target language is
@tech{Exprs-safe-data-lang v7}.
}

@;  LocalWords:  lang rkt TODO eg eval behaviour pm url todo subsubsub tt ids
@;  LocalWords:  GitHub secref emph itemlist primops uniquify Rice's Exprs eq
@;  LocalWords:  racketgrammar fixnum uint ascii binop unop deftech primop int
@;  LocalWords:  racketblock lib nasm Dybvig Chez ptr fixnums immediates xor
@;  LocalWords:  ptrs Paren sar loc ie triv reg trg opand cmp addr fbp bnf ior
@;  LocalWords:  dispoffset unsyntax neq binops aloc rloc fvar fvars pre CPS
@;  LocalWords:  undead fx codeblock falsey unops
