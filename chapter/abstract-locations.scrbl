#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  "diagrams/sys-v-stack.rkt"
  scriblib/figure
  cpsc411/langs/v1
  cpsc411/langs/v2
  (for-label cpsc411/langs/v2)
  ; check-paren-x64 only defined in the a1 chapter
  (for-label (except-in cpsc411/reference/a2-solution check-paren-x64))
  (for-label (only-in cpsc411/reference/a1-solution check-paren-x64))
  (for-label (except-in cpsc411/compiler-lib compile))
  (for-label cpsc411/2c-run-time)
  #;(except-in "abstracting-boilerplate.scrbl" doc))

@(provide
  (except-out (all-defined-out) sb))

@(define sb
  (make-cached-eval "ch2-eval"
   '(require cpsc411/reference/a2-solution cpsc411/compiler-lib cpsc411/2c-run-time)
   '(current-stack-size 512)))

@declare-exporting[cpsc411/reference/a2-solution]

@; ----- Language Defs ------

@todo{Maybe local instead of module? Racket semantics are closer, and the syntax
is closer.}

@define[v2-graph
@dot->svg{
digraph {

node [ fontname="Courier", shape="box", fontsize=12 ]

/* The Languages */

L4 [label="Asm-lang-v2"];

L62 [label="Nested-asm v2"]
L7 [label="Para-asm-lang v2"];
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

L4 -> L62 [label=" assign-homes"]
L62 -> L7 [label=" flatten-begins"]
L7 -> L8 [label=" patch-instructions"];
L8 -> L9 [label=" implement-fvars"];
L9 -> L10 [label=" generate-x64"];
L10 -> L11 [label=" execute"];
L9 -> L11 [label = "interp-paren-x64"];
}
}
]

@title[#:tag "top" #:tag-prefix "chp2:"]{Abstract Locations (v2)}

@section{Preface: What's wrong with our language?}
In the last chapter, we designed our first language, @ch-bp-tech{Paren-x64 v1}, and
wrote our first compiler to implement it.
This language introduces the barest, although still useful,
abstraction---freedom from boilerplate. @todo{Make it freedom from the trivial concerns of syntax, then introduce abstract syntax vs concrete}
@ch-bp-tech{Paren-x64 v1} abstracts away from the boilerplate of @ch1-tech{x64}, and
details of exactly how to pass a value to the operating system.

While @ch-bp-tech{Paren-x64 v1} is an improvement of @ch1-tech{x64}, it has a
significant limitation for writing programs that we address in this chapter.
The language requires the programmer to manually manage a small number of
variables, the registers, while programming.
Human memory is much less reliable than computer memory, so we should@todo{OUGHT}
design languages that make the computer remember more and free the programmer to
remember less.
This would prevent the programmer from causing run-time errors when they inevitably
make a mistake and overwrite a register that was still in use.

To address this, we introduce @tech{abstract locations}, of which there are
an arbitrary number and that the programmer does not need to know what
@tech{physical location} they end up using.

In general, we cannot map the arbitrary number of @tech{abstract locations} to
the fixed number of registers.
So to implement @tech{abstract locations}, we'll need to expose a new feature
from our @ch1-tech{target language}, @ch1-tech{x64}.
We expose some limited access to memory in @ch1-tech{x64}, and introduce a new
abstraction, the @tech{frame variable}, to help us compile @tech{abstract
locations} to @tech{physical locations}.

We also need to design a new language, and some translations, that enable
instructions to work over @tech{abstract locations}, even though @ch1-tech{x64}
restricts what kinds of @tech{physical locations} instructions can use.

@section{Abstracting Physical Locations in the Source Language}
When designing a new abstraction, I often start by reading and writing some
programs using an existing abstraction until I spot a pattern I dislike.

Here's an example program @ch-bp-tech{Paren-x64 v1} that computes and returns @paren-x64-v1[120].
@paren-x64-v1-block[
(begin
  (set! rsp 1)
  (set! rsp (+ rsp 119))
  (set! rax rsp))
]

And here is an equivalent program:

@paren-x64-v1-block[
(begin
  (set! r15 1)
  (set! r15 (+ r15 119))
  (set! rax r15))
]

The only difference between these two programs is the choice of register used
to perform the computation: either @paren-x64-v1[rsp] or @paren-x64-v1[r15].
The choice is irrelevant, but I do need to make the choice to write the program.
I also need to be sure that register isn't in use.

Here's a different program.

@paren-x64-v1-block[
(begin
  (set! r15 1)
  (set! r14 #,(max-int 60))
  (set! r15 (+ r15 r14))
  (set! rax r15))
]

This program adds two numbers, @paren-x64-v1[1] and @paren-x64-v1[#,(max-int 60)].
Because the second number is large, I cannot use it directly as an operand to
the @paren-x64-v1[+] instruction.
I, the programmer, must remember this, and remember to pick a new arbitrary
register to store it in.

We've seen a few @ch-bp-tech{Paren-x64 v1} programs by now, and they all have a
pattern: all computations act on a small set of 16 registers.
This limits the way we can write computations.
We must, manually, order and collapse sub-computations to keep the number of
registers that are in use small.
We must keep track of which registers are still in use before we move a value,
or we will overwrite part of our computation.

Furthermore, the instructions we're given are idiosyncratic---they only work
with certain operands, such as requiring some integer literals to be 32-bit or
64-bit, depending on which instruction we're using.
The programmer is forced to consider the size of their data before choosing how
to proceed with a computation.

These limitations make programming cumbersome and error-prone.

Instead, we should free the programmers (ourselves), eliminating cumbersomeness
and removing error-prone programming patterns.
We should free the programmer to invent new locations at will if it helps their
programming, and not worry about irrelevant machine-specific restrictions on
these locations.

To address this, we can design a new language to abstract away from these two
machine-specific concerns.
Our goals in this language are: (1) we should be able to invent new, free,
unused locations at will; (2) we should be able to use instructions uniformaly
on these locations and integer literals, regardless of the size of the literal
or choice or location.

Below, we design one such language, @tech{Asm-lang v2}.


@bettergrammar*[asm-lang-v2]
@todo{Use a locals form instead of info field? More semantically justified?
OTOH, could view the info field as some block declarations, like require/provide statements}

To address the first goal, we introduce the abstraction @tech{abstract locations}.
An @deftech{abstract location} is name for some @tech{physical location}
to be chosen by the compiler.
The name is unique for some unit of compilation, which means globally unique
for whole-program compilation.

Without a direct representation of @tech{physical locations}, the language
cannot represent all the constraints on operands, dictacted in part by the
hardware, so we're also essentially forced to solve the second goal.
This will become more apparent when we see the constraints on operands
involving memory operands.
So, there are now only two instructions---move a value to an @tech{abstract
location}, or perform a binary operation on an @tech{abstract location}.
These act uniformly on their arguments, regardless of size.
The left-hand-side is always the destination location, and the right-hand-side
is an arbitrary trivial value.

The run-time system still requires that we pass the result to the @ch1-tech{OS}
in @paren-x64-v1[rax], but we've removed registers from the language.
This requires us to design some new feature that we can use to indicate the
result without exposing registers.
We add the @asm-lang-v2[halt] instruction, which indicates the end of the
computation with a particular value as the result.
@todo{Introduce physical location in chapter 1 or 2, initially only registers.}

@todo{Remove halt, use implicit return. Introduce select-instructions, probably as the first pass. That fits with the all-home loop.}

The language has one more unusual bit of syntax: the @asm-lang-v2[info] field.
In order to compile @tech{abstract locations}, we will need some information
about them, and eventually to map them to @tech{physical locations}.
In general, we may need metadata lots of metadata about a program in order to
efficiently compile it.
To store this metadata, we include an annotation on the program represented by
@asm-lang-v2[info].

The @deftech{info field} is an annotation in the program that serves only to
store metadata about the program for compilation.
This additional information will often be result of some program analysis or
preprocessing step that informs the next compiler pass.
In @tech{Asm-lang v2}, the @tech{info field} is unrestricted---it could contain
anything at all, as long as it's a valid @racket[info?].
Eventually, we will require it to contain specific metadata.

@margin-note*{
The module @racketmodname[cpsc411/info-lib] provides utilities for working with
the @tech{info field} representation.
It also provides the contract @racket[info/c], which formalizes the
specification language for @tech{info fields}.
}

We represent the @tech{info field} as an association list of keys to a proper
list whose first element is the value of the key, such as @racket[((key
value))].
In general, we only give a partial specification---it may contain arbitrary
other key-value pairs.
This lax specification is useful for debugging: you may leave residual
@asm-lang-v2[info] from earlier languages, or include your own debugging
@asm-lang-v2[info].
The @asm-lang-v2[info] field is also unordered.

@digression{
In a production compiler, we would probably not represent this information
directly as part of the program, but instead store the contents of the
@tech{info field} "on the side", as a separate data structure that is passed
through the compiler.
This would prevent us from deconstructing and reconstructing the syntax tree
when modifying or accessing the @tech{info field}.

However, directly representing the @tech{info field} as part of the language has
some advantages.
It becomes simple to compose passes under a single interface.
The program representation captures @emph{all} of its invariants, and we do not
need to consider an external data structure, making debugging, printing, and
reading programs simpler.

Furthermore, our representation of the @tech{info field} is not
particularly efficient.
The proper list representation uses strictly more memory than necessary, and
does not support random access.
We could improve it by using improper lists, such as @racket[((key . value))], or
perhaps a hash table.
However, the proper list representation is simpler to read and write in program
text, and hash tables have constant factors that can dominate the cost of
traversing small lists, so the choice of representation is not entirely
obvious.
}

@todo{This is a lot that is... irrelevant to the core concepts.
Particularly to the source language.
Could avoid some by introducing a locals form.
}

@section{Accessing Memory in x64: Displcement Mode Operands}
Abstracting away from registers introduces a problem.
We cannot necessarily compile a program with @tech{abstract locations} into one with registers.
Consider the following @tech{Asm-lang v2} program, which has 17 @tech{abstract locations}:

@asm-lang-v2-block[
#:datum-literals
(x.1 x.2 x.3 x.4 x.5 x.6 x.7 x.8 x.9 x.10 x.11
 x.12 x.13 x.14 x.15 x.16 x.17)
(module ()
  (begin
    (set! x.1 1)
    (set! x.2 (+ x.2 x.1))
    (set! x.3 (+ x.3 x.2))
    (set! x.4 (+ x.4 x.3))
    (set! x.5 (+ x.5 x.4))
    (set! x.6 (+ x.6 x.5))
    (set! x.7 (+ x.7 x.6))
    (set! x.8 (+ x.8 x.7))
    (set! x.9 (+ x.9 x.8))
    (set! x.10 (+ x.10 x.8))
    (set! x.11 (+ x.11 x.7))
    (set! x.12 (+ x.12 x.6))
    (set! x.13 (+ x.13 x.5))
    (set! x.14 (+ x.14 x.4))
    (set! x.15 (+ x.15 x.3))
    (set! x.16 (+ x.16 x.2))
    (set! x.17 (+ x.17 x.1))
    (halt x.17)))
]

To translate this into @ch1-tech{x64} requires assigning each of the 17
@tech{abstract locations} to @tech{physical locations}.
However, we cannot simply map each @tech{abstract location} to a register.
Instead, we need a new source of arbitrarily many @tech{physical locations}, or
we need to be very clever about reusing registers.
Being clever is a last resort, so we'd like to avoid it.
Even if we were clever, we could easily extend the program so that all 17 are
needed at the same time, making it impossible to reuse only 16 registers.
@todo{Transition}
Therefore, we need a source of arbitrary many @tech{physical locations}.

Thankfully, @ch1-tech{x64} does provide this feature: instruction operands that
enable operating on memory locations instead of registers.
In particular, we expose @tech{displacement mode operands} for memory locations.
The @deftech{displacement mode operand} is a new operand that can appear as a
@tech{physical location} in the the operand of some instructions.
This allows accessing memory locations using pointer arithmetic.
In @ch1-tech{x64}, a @tech{displacement mode operand} is written as @tt{QWORD
[reg - int32]} or @tt{QWORD [reg + int32]}, where @tt{reg} is a register
holding some memory address and the @tt{int32} is an offset number of bytes
from that base address, as a 32-bit integer.
The keyword @tt{QWORD}, which is an unintuitive spelling of "8 bytes",
indicates that this operand is accessing 8 bytes (64 bits) at a time.

@margin-note{
"Word" normally means the unit of addressing memory---64 bits in our case.
Unfortunately, in the past, the word size was different.
To avoid backwards incompatible changes, tools that use @tt{WORD} as
a keyword, like @tt{nasm}, didn't want to change it's meaning.
Instead, the keyword @tt{WORD} means 16 bits, not the word size, and prefixes
give us multiple of that notion of @tt{WORD}.
So @tt{QWORD} is 4 @tt{WORD}s, or 64 bits, which is the word size on @ch1-tech{x64}.
}

For example, if @tt{rbp} holds a memory address, we can move the value
@tt{42} to that memory address using the instruction
@tt{mov QWORD [rbp - 0], 42}.
We can move the value from memory into the register @tt{rax} using the
instruction @tt{mov rax, QWORD [rbp - 0]}.

@todo{Add memory diagram. Requires abstracting the stack diagram code.}
@;We can represent memory graphically as in @Figure-ref{fig:memory1}
@;@figure["fig:sysv-stack" @elem{The SYS V Initial Process Stack} sys-v-stack-diagram]

@todo{Be careful here about memory location vs memory address.}

As with other instructions, the hardware imposes arbitrary restrictions on the
combinations of operands.
A @tt{mov} instruction only supports both a memory location and a 32-bit
integer literal, not a 64-bit literal.
@tt{mov [rbp + 0], 9223372036854775807} is invalid; instead, the integer would
need to be moved into a register, first, as in:
@verbatim{
mov r9, 9223372036854775807
mov [rbp + 0], r9
}

The literal offset is the number of bytes to offset from the base pointer.
While @ch1-tech{x64} supports sub-word addressing, we will only address by word.
Since we are dealing primarily with 64-bit words, our offsets are always multiples of 8.
For example, the following snippet of code moves two values into memory, then
pulls them out and adds them.
@verbatim{
mov QWORD [rbp - 0], 21
mov QWORD [rbp - 8], 21
mov rax, QWORD [rbp - 8]
mov rbx, QWORD [rbp - 0]
add rax, rbx
}

@section{Run-time Support for Accessing Memory: The Stack}
Now that we know how to address memory, the next problem is where does the
memory come from: where do we get that initial base pointer, and how do we know
how far we're allowed to address?

This requires a little support from the @tech{run-time system}.
@ch1-tech{x64}, by itself, doesn't have raw access to computer memory---at
least, not under most @tech{operating systems}.
Instead, the @tech{run-time system} for our language will have work with the
@tech{OS} to get a new pointer to the start of some free memory.

Thankfully, on systems following the SYS V ABI (including Linux and macOS on
x64), when any process launches, @paren-x64-v2[rsp] is initialized to the stack
pointer.
We can visualize the initial process stack as in @Figure-ref{fig:sysv-stack}.

@figure["fig:sysv-stack" @elem{The SYS V Initial Process Stack} sys-v-stack-diagram]

The stack starts at the end of the virtual address space, and everything
@emph{below} @tt{rsp} is free space.
Above the stack pointer, @ie "on the stack", are values passed by the
operating system: the command line arguments.
@tt{[rsp + 0]} contains the argument count (@tt{argc}) of arguments passed to the
process, with each stack slot above it containing a pointer to an argument.
That is, @tt{[rsp + 8]} contains a pointer to the first argument to the
process, @tt{[rsp + 16]} the second, @emph{etc}.
Since everything above @tt{rsp} is in use, and we're at the end of the virtual
address space, we can do pointer arthimatic backwards to find indefinite
amounts of free space (assuming infinite memory).

@section{Exposing Memory Access in Intermediate Languages}
To provide access to the stack in our intermediate languages, we need two
things.
First, we need to redesign all intermediate langauges with direct access to
@tech{physical locations}, such as @ch-bp-tech{Paren-x64 v1}, to support access
to the stack.
We may want to add additional constraints on how the stack pointer is used, to
simplify compilation or interpretation of our intermediate languages.
Second, we need the @ch-bp-tech{run-time system} to provide access to the
initial stack pointer.
We could use @tt{rsp} directly, but we might choose to use it differently.

We start with the desgin for a new target language, @deftech{Paren-x64 v2}
(@racket[paren-x64-v2]), whose grammar is typeset below, displaying differnces
compared to @ch-bp-tech{Paren-x64 v1}.

@bettergrammar*-ndiff[
#:labels ("Diff vs Paren-x64 v1" "Paren-x64 v2" "Paren-x64 v1")
(#:exclude (reg) paren-x64-v1 paren-x64-v2)
(paren-x64-v2)
(paren-x64-v1)
]

We add the new non-terminal @paren-x64-v2[addr] to the language, and add
@paren-x64-v2[addr] as a production to @paren-x64-v2[loc].
The @paren-x64-v2[addr] non-terminal represents a @tech{displacement mode
operand} to an instruction.
We abstract the language over the @deftech{base frame pointer}, the pointer to
the start of the current stack frame, which is stored in the parameter
@racket[current-frame-base-pointer-register] and is
@paren-x64-v2[#,(current-frame-base-pointer-register)] by default.
An @paren-x64-v2[addr] may only be used with the
@racket[current-frame-base-pointer-register] as its first operand.
The offset of each @paren-x64-v2[addr] is restricted to be an integer that is
divisible by 8, the number of bytes in a machine word in @ch1-tech{x64}.
This ensures all memory accesses are machine-word aligned, meaning we leave
space for all bytes in the word between each access.
Note that the offset is @emph{negative}; we access the stack backwards, since
that's where all the free space is when the process starts.

All languages in our compiler will assume that the uses of
@racket[current-frame-base-pointer-register] obey the @deftech{stack
discipline}, defined below.
Setting its value directly is forbidden.
Pointer arithmetic, such as @paren-x64-v2[(set! fbp (+ fbp opand))], is allowed
only when the @paren-x64-v2[opand] is a @racket[dispoffset?].
Incrementing the pointer beyond its initial value given by the
@ch-bp-tech{run-time system} is forbidden.
We do not try to enforce these statically, since it may be impossible to do so
in general.
In this sense, all these forbidden uses are @ch-bp-tech{undefined behaviour}.

@digression{
The language is parameterized by the @racket[current-frame-base-pointer-register].
Parameterizing the language this way lets us avoid committing to particular
register choices, making the language inherently more machine and convention
agnostic.
This is helpful in designing a compiler with multiple machine backends.
A real compiler would want to support many machines, not just @ch1-tech{x64},
and parameterizing the language makes this simpler.
We could imagine retargeting a new operating system that uses a different stack
register, by changing the value of @racket[current-frame-base-pointer-register],
among other parameters.
If our language definitions were sufficiently parameterized, few if any compiler
passes would need to differ between target machines.
This language is not sufficiently abstract yet, but using parameterized languages
in this way is a common tool we will use.
}

Next, we need a @ch-bp-tech{run-time system}.
Our approach is to decrement the initial @tt{rsp} by 8, and move its value to
@racket[current-frame-base-pointer-register].
That way, @paren-x64-v2[(fbp - 0)] is the first free memory location, and we can
essentially treat @paren-x64-v2[fbp] as a 0-indexed array (using negative
multiple-of-8 indexes).

@margin-note*{
We provide such a @ch-bp-tech{run-time system} in @racketmodname[cpsc411/2c-run-time].

This new @ch-bp-tech{run-time system} also prints the value of
@paren-x64-v2[rax] to the standard output port , instead of returning it via an
exit code, and does the work of converting numbers to ASCII strings.
The @racket[execute] function uses @racket[nasm-run/read] to parse printed
output into a Racket datum.
If you're interested in how this is done, you can read the definition of
@racket[wrap-x64-run-time].

For the rest of the book, we assume that this @ch-bp-tech{run-time system} is
used until we introduce data types, which require additional support from the
@ch-bp-tech{run-time system}.
}

@section{Translating Abstract Locations into Physical Locations}

@nested[#:style 'inset
@defproc[(generate-x64 (p paren-x64-v2?))
         (and/c string? x64-instructions?)]{
Compile the @tech{Paren-x64 v2} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}

@examples[#:eval sb
(require racket/pretty)
(pretty-display
 (generate-x64 '(begin (set! rax 42))))
(pretty-display
 (generate-x64 '(begin (set! rax 42) (set! rax (+ rax 0)))))
(pretty-display
 (generate-x64
 '(begin
    (set! (rbp - 0) 0)
    (set! (rbp - 8) 42)
    (set! rax (rbp - 0))
    (set! rax (+ rax (rbp - 8))))))
(pretty-display
 (generate-x64
 '(begin
    (set! rax 0)
    (set! rbx 0)
    (set! r9 42)
    (set! rax (+ rax r9)))))

(current-pass-list
 (list
  check-paren-x64
  generate-x64
  wrap-x64-run-time
  wrap-x64-boilerplate))

(execute '(begin (set! rax 42)))
(execute '(begin (set! rax 42) (set! rax (+ rax 0))))
(execute
 '(begin
    (set! (rbp - 0) 0)
    (set! (rbp - 8) 42)
    (set! rax (rbp - 0))
    (set! rax (+ rax (rbp - 8)))))
(execute
 '(begin
    (set! rax 0)
    (set! rbx 0)
    (set! r9 42)
    (set! rax (+ rax r9))))
]
]

@section{Abstracting the Machine}
Now that we have effectively unlimited @deftech{physical locations}---registers and
memory locations that can store values---on the machine, we
want to begin abstracting away from machine details.

The first thing we do is to abstract away from the @tech{displacement mode operand},
introducing an abstract notion of @deftech{frame variable}, a variable that is
located in a particular slot on the frame.
This lets the programmer stop worrying about details like which register
contains the frame base, and what the constraints on displacement mode
offset are.

We define @deftech{Paren-x64-fvars v2} (@racket[paren-x64-fvars-v2]) below.
@bettergrammar*-ndiff[
#:labels ("Diff" "Paren-x64-fvars-v2" "Paren-x64-v2")
(paren-x64-v2 paren-x64-fvars-v2)
(paren-x64-fvars-v2)
(paren-x64-v2)
]

We replace the @paren-x64-v2[addr], the @tech{displacement mode operand}, with
the abstraction of an @paren-x64-fvars-v2[fvar],
which represents a unique location on the frame, relative to the current value
of @racket[current-frame-base-pointer-register].
These are written as the symbol @racket[fv] followed by a
number indicating the slot on the frame.
Conceptually, frame variables let us treat the stack frame like a normal 0-indexed array.
For example @paren-x64-fvars-v2[fv0] is the frame variable indicating the
current frame pointer and 0th slot in the frame, with @paren-x64-fvars-v2[fv1]
the next word sized slot, and so on.
We can visualize frame variables relative to the stack as in @Figure-ref{fig:fvar-stack}.
The number represents the index into the frame for the current function.
Frame variables are distinct from registers and abstract locations.

@figure[
"fig:fvar-stack"
"Frame variables relative to the process stack"
frame-var-diagram
]


@todo{
The @share{a6-compiler-lib.rkt} defines a few helpers for working with frame
variables: @racket[fvar?], @racket[make-fvar], @racket[fvar->index], and
@racket[fvar->addr].
}

In @tech{Paren-x64-fvars v2}, it is still @ch-bp-tech{undefined behaviour} to
violate stack discipline when using
@racket[current-frame-base-pointer-register].

@nested[#:style 'inset
@defproc[(implement-fvars (p paren-x64-fvars-v2?))
         paren-x64-v2?]{
Compiles the @tech{Paren-x64-fvars v2} to @tech{Paren-x64 v2} by reifying
@paren-x64-fvars-v2[fvar]s into @tech{displacement mode operands}.
The pass should use @racket[current-frame-base-pointer-register].
}
]

This is a useful step toward an abstract assembly language, but we're still
forced to remember odd restrictions on which kind of @tech{physical location}
each instruction takes as an operand.
Our instructions in @tech{Paren x64-v2} are restricted by @ch1-tech{x64}.
For example, binary operations must use a register as their first operand.

We can create a new language that allows the user to ignore the differences
between @tech{physical locations}, enabling @emph{any} instruction to work on
any kind of @tech{physical locations}.
This way, the language is responsible for managing these annoying details
instead of the programmer.

We do this by defining @deftech{Para-asm-lang v2} (@racket[para-asm-lang-v2]), a kind of
less finicky assembly language.
We can think of this language as @emph{para}meterized by the set of locations,
hence the name.

@bettergrammar*-ndiff[
#:labels ("Diff" "Para-asm-lang-v2" "Paren-x64-fvars-v2")
(paren-x64-fvars-v2 para-asm-lang-v2)
(para-asm-lang-v2)
(paren-x64-fvars-v2 )
]

The main difference is in the @para-asm-lang-v2[effect] non-terminal, which has
been renamed and simplified from the @paren-x64-fvars-v2[s] non-terminal.
Now, instructions can use an arbitrary kind of @para-asm-lang-v2[loc] as their
operands.
The semantics are otherwise unchanged.
@para-asm-lang-v2[(set! loc triv)] moves the value of @para-asm-lang-v2[triv] into
@para-asm-lang-v2[loc], and @para-asm-lang-v2[(set! loc_1 (+ loc_1 triv))] adds the
values of @para-asm-lang-v2[loc_1] and @para-asm-lang-v2[triv], storing the result in
@para-asm-lang-v2[loc_1].
Note that the two occurrences of @para-asm-lang-v2[loc_1] in a binary operations are
still required to be identical.
We're not trying to lift all restrictions from @ch1-tech{x64} yet.

We also add the @nested-asm-lang-v2[halt] instruction, which moves the value of
its operand into the @racket[current-return-value-register]
(@para-asm-lang-v2[#,(current-return-value-register)] by default).
Adding this abstraction frees the user from remembering which register is used for
this purpose.
This instruction is valid only as the final instruction executed in a program,
and it must be present.

Notice that two registers, @para-asm-lang-v2[r10], and @para-asm-lang-v2[r11], have been
removed from @para-asm-lang-v2[reg].
@tech{Para-asm-lang v2} assumes control of these registers, forbidding the
programmer from using them directly.
Instead, the language implementation will use this when compiling to
@tech{Paren-x64 v2}.
These auxiliary registers are defined by the parameter
@racket[current-auxiliary-registers].

Note that we do not have to restrict @para-asm-lang-v2[rax], despite the language
making use of it.
@question{Why not?}

@digression{
If we were trying to support multiple backends, we might parameterize the
language further by the set of unrestricted registers.
This set would describe the registers that can be used in an arbitrary way by
the program, and would exclude the set of @racket[current-auxiliary-registers].
Each restricted register would have some invariants associated with it, similar to
the @tech{stack discipline} invariant for the
@racket[current-frame-base-pointer-register].
}

@nested[#:style 'inset
@defproc[(patch-instructions (p para-asm-lang-v2?))
          paren-x64-fvars-v2?]{
Compiles @tech{Para-asm-lang v2} to @tech{Paren-x64-fvars v2} by patching
instructions that have no @ch1-tech{x64} analogue into a sequence of
instructions.
The implementation should use auxiliary registers from
@racket[current-patch-instructions-registers] when generating instruction
sequences, and @racket[current-return-value-register] for compiling
@para-asm-lang-v2[halt].

@examples[#:eval sb
(patch-instructions '(begin (set! rbx 42) (halt rbx)))

(patch-instructions
 '(begin
    (set! fv0 0)
    (set! fv1 42)
    (set! fv0 fv1)
    (halt fv0)))

(patch-instructions
 '(begin
    (set! rbx 0)
    (set! rcx 0)
    (set! r9 42)
    (set! rbx rcx)
    (set! rbx (+ rbx r9))
    (halt rbx)))
]
}
]

@section{Nesting Instruction Sequences}
@todo{This is sort of crowbarred in. Really belongs in "imperative abstractions", which is all about composition. But that requires a lot of motivation in that chapter, and a lot of new typesetting.}
One of the most restrictive limitations in our language is that expressions
cannot be nested.
Each program must be a linear sequence of instructions.
@todo{motivation is weak here}
@todo{This is must easier to motivate, I think, when going top-down ala the
removing-patch-instructions branch.
select-instructions needs to transform one effect into multiple instruction in
effect context.}

We can easily lift this restriction by designing a language that supports
nesting, and compiling it using our well-known operations for composing
@ch-bp-tech{instruction sequences}.
This simplifies the job of later passes that are now free to nest instructions
if it's helpful.

Below, we design @deftech{Nested-asm-lang-v2} (@racket[nested-asm-lang-v2]).

@todo{This doesn't render correctly due ambiguity in the tree diff}
@;bettergrammar*-diff[para-asm-lang-v2 nested-asm-lang-v2]
@bettergrammar*-ndiff[
#:labels ("Diff" "Para-asm-lang-v2" "Nested-asm-lang-v2")
#;(#:include (p tail effect)
  para-asm-lang-v2 nested-asm-lang-v2)
((((unsyntax (bnf:add "p")) tail)
 ((unsyntax (bnf:sub "p"))  (begin effect ... (halt triv)))
 ((unsyntax (bnf:add "tail"))
  (halt triv)
  (begin effect ... tail))
 (effect
   (set! loc triv)
   (set! loc_1 (binop loc_1 triv))
   (unsyntax (bnf:add "(begin effect ... effect)")))))
(para-asm-lang-v2)
(nested-asm-lang-v2)
]

We add a @para-asm-lang-v2[tail] production that loosely corresponds to the
top-level program from @tech{Para-asm-lang v2}.
However, these can be nested, before eventually ending in a @nested-asm-lang-v2[halt]
instruction.
The @nested-asm-lang-v2[tail] production represents the "tail", or last, computation
in the program.

We also enable nesting in effect position.
This essentially allows us to copy and paste some @ch-bp-tech{instruction
sequence} into the middle of a program.

@nested[#:style 'inset
@defproc[(flatten-begins [p nested-asm-lang-v2?]) para-asm-lang-v2?]{
Flatten all nested @nested-asm-lang-v2[begin] expressions.
}
]

@section{Abstracting Physical Locations}
We are still required to think about @tech{physical locations}.
We don't usually care which location a value is stored in, so long as it is
stored @emph{somewhere}.


Each @tech{abstract location} must be allocated a @tech{physical location}
somewhere on the machine, and we want to ensure the allocater can replace any
two instances of an @tech{abstract location} with the same @tech{physical
location}.

We define @deftech{Asm-lang v2} (@racket[asm-lang-v2]) below.
@tech{Asm-lang v2} is an imperative, assembly-like language.

@bettergrammar*-ndiff[
#:labels ("Asm-lang-v2" "Diff" "Nested-asm-lang-v2")
(asm-lang-v2)
(nested-asm-lang-v2 asm-lang-v2)
(nested-asm-lang-v2)
]

In @tech{Asm-lang v2}, we generalize instructions to work over @tech{abstract
location}, @asm-lang-v2[aloc].
An @asm-lang-v2[aloc] is a symbol that is of the form @code{<name>.<number>},
such as @asm-lang-v2[x.2]; this is captured by the @racket[aloc?] predicate.
We assume all @asm-lang-v2[aloc]s are unique up to some scope, and any reference
to the same @asm-lang-v2[aloc] is to the same location---these are not the
@ch3-tech{names} yet.
So far, the only scope we have is the global program scope, so all
@tech{abstract locations} are globally unique.

Implementing @tech{Asm-lang v2} is a multi-step process.
We gather all the @tech{abstract locations}, then assign them to
@tech{physical locations}.
@todo{'Implementing' concretely means compiling to nested-asm v2 right?
This should probably be moved to the beginning of this section and
also be more elaborate.}

For each step, we create an @deftech{administrative language}---an intermediate
language whose semantics does not differ at all from its parent language, but
whose syntax is potentially decorated with additional data that simplifies the next
step of the compiler.

First, we analyze the @tech{Asm-lang v2} to discover which
@tech{abstract locations} are in use.
This is a straight-forward analysis, traversing the program and collecting any
@tech{abstract location} we see into a set.

We define the @tech{administrative language} @deftech{Asm-lang v2/locals} (@racket[asm-lang-v2/locals])
(pronounced "Asm lang v2 with locals") to capture this information.

@bettergrammar*-diff[asm-lang-v2 asm-lang-v2/locals]

The only difference is in the @tech{info field}, which now contains a
@deftech{locals set}, a @racket[set?] of all @tech{abstract locations} used in
the associated @asm-lang-v2[tail].
A grammar production beginning with @racket[#:from-contract] is not specified
using BNF, but instead is specified by the @racket[contract] expression
following it.
In this case, the @tech{info field} is required to contain at least one entry
mapping the key @racket['locals] to a list of @racket[aloc?].
@margin-note*{
Contract expressions can be evaluated like predicates; see the documentation for
@racket[info/c] for examples.
}
In this language, there is an invariant that any @tech{abstract location} that
is used must appear in the @asm-lang-v2[locals] set in the @tech{info field}.

@nested[#:style 'inset
@defproc[(uncover-locals (p asm-lang-v2?))
         asm-lang-v2/locals?]{
Compiles @tech{Asm-lang v2} to @tech{Asm-lang v2/locals}, analysing which
@tech{abstract locations} are used in the program and decorating the program with
the set of variables in an @tech{info field}.

@examples[#:eval sb
(uncover-locals
 '(module ()
   (begin
     (set! x.1 0)
     (halt x.1))))

(uncover-locals
 '(module ()
   (begin
     (set! x.1 0)
     (set! y.1 x.1)
     (set! y.1 (+ y.1 x.1))
     (halt y.1))))
]
}
]

Next, we assign homes for all the @tech{abstract locations}.
For now, our strategy is trivial: we assign each @tech{abstract location} a
@tech{fresh} @tech{frame variable}.
A @deftech{fresh} variable is one that is globally unique, unused anywhere else in the program.

We capture this information in a new @tech{info field}, described in
@deftech{Asm-lang-v2/assignments}.

@bettergrammar*-diff[asm-lang-v2/locals asm-lang-v2/assignments]

The @asm-lang-v2/assignments[assignment] @tech{info field} records a mapping
from each @tech{abstract location} to some @tech{physical location}.
The language is more general than our implementation strategy; we allow an
@tech{abstract location} to be assigned to any valid @tech{physical location},
including registers, to permit future optimizations or handwritten code.

@nested[#:style 'inset
@defproc[(assign-fvars (p asm-lang-v2/locals?))
          asm-lang-v2/assignments?]{
Compiles @tech{Asm-lang v2/locals} to @tech{Asm-lang v2/assignments},
by assigning each @tech{abstract location} from the @asm-lang-v2/locals[locals]
@tech{info field} to a fresh @tech{frame variable}.

@examples[#:eval sb
(assign-fvars
 '(module
    ((locals (x.1)))
    (begin
      (set! x.1 0)
      (halt x.1))))

(assign-fvars
 '(module
    ((locals (x.1 y.1 w.1)))
    (begin
      (set! x.1 0)
      (set! y.1 x.1)
      (set! w.1 1)
      (set! w.1 (+ w.1 y.1))
      (halt w.1))))
]
}
]

Finally, we simply replace all the @tech{abstract locations} with
the @tech{physical location} assigned by @racket[assign-fvars].

@nested[#:style 'inset
@defproc[(replace-locations (p asm-lang-v2/assignments?))
          nested-asm-lang-v2?]{
Compiles @tech{Asm-lang v2/assignments} to @tech{Nested-asm-lang v2},
replacing each @tech{abstract location} with its assigned @tech{physical
location} from the @asm-lang-v2/assignments[assignment] @tech{info field}.
}

@examples[#:eval sb
(replace-locations
  '(module ((locals (x.1)) (assignment ((x.1 rax))))
    (begin
      (set! x.1 0)
      (halt x.1))))

(replace-locations
  '(module ((locals (x.1 y.1 w.1))
            (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
    (begin
      (set! x.1 0)
      (set! y.1 x.1)
      (set! w.1 1)
      (set! w.1 (+ w.1 y.1))
      (halt w.1))))
]
]

As these three passes implement a single true language, we wrap them up as a
single pass.

@nested[#:style 'inset
@defproc[(assign-homes (p asm-lang-v2?))
         nested-asm-lang-v2?]{
Compiles @tech{Asm-lang v2} to @tech{Nested-asm-lang v2}, replacing each
@tech{abstract location} with a @tech{physical location}.
}
]

@section{Appendix: Overview}

@figure["fig:v2-graph" "Overview of Compiler Version 2" v2-graph]
