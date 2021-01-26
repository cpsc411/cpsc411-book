#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  cpsc411/langs/v1
  cpsc411/langs/v2
  (for-label cpsc411/langs/v2)
  (for-label cpsc411/reference/a2-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  (except-in "abstracting-boilerplate.scrbl" doc))

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

@title[#:tag "top" #:tag-prefix "chp2:"]{Abstracting Machine Details}

@section{Preface: What's wrong with our language?}
In the last chapter, we designed our first language, @a1-tech{Paren-x64 v1}, and
wrote our first compiler to implement it.
This language introduces the barest, although still very useful,
abstraction---freedom from boilerplate.
@a1-tech{Paren-x64 v1} abstracts away from the boilerplate of @ch1-tech{x64}, and
details of exactly how to pass a value to the operating system.

While @a1-tech{Paren-x64 v1} is an improvement of @ch1-tech{x64}, it has a
significant limitation for writing programs that we will address this week.
The language requires the programmer to remember many machine details---in
particular, which of the small number of registers are in use---while
programming.
Human memory is much less reliable than computer memory, so we should
design languages that make the computer remember more and free the human to
remember less.
This will prevent the human from causing run-time errors when they inevitable
make a mistake and overwrite a register that was still in use.

@section{Introduction to Designing a Source language}
When designing a new language, I often start by writing some programs until I
spot a pattern I dislike.
@todo{Do some example Paren-x64 v1 programming}

The pattern in @a1-tech{Paren-x64 v1} is that all computations act on a small
set of 16 @tech{physical locations}.

This limits the way we can write computations.
We must, manually, order and collapse sub-computations to keep the number of
locations that are in use small.
We must keep track of which locations are still in use before we move a value
into a location, or we will overwrite part of our computation.

Furthermore, the instructions we're given are idiosyncratic---they only work
with certain operands, such as requiring some integer literals to be 32-bit or
64-bit, depending on which instruction we're using.
The programmer is forced to consider the size of their data before choosing how
to proceed with a computation.

These limitations make programming cumbersome and error-prone.

Instead, we should free the programmers (ourselves), eliminating cumbersomeness
and removing error-prone programming patterns.

We should free the programmer to invent new locations at will if it helps their
programming, and not worry about irrelevant machine-specific restrictions on instructions.

We design a new language, @tech{Asm-lang v2}, to abstract away from these two
machine-specific concerns.

@bettergrammar*[asm-lang-v2]

The language no longer knows about registers, but instead presents an
@tech{abstract location} to the programmer.
There are two assembly instructions---move a value to an @tech{abstract
location}, or perform a binary operation on an @tech{abstract location}.
These now act uniformly on their arguments.
The left-hand-side is always the destination location, and the right-hand-side
is an arbitrary trivial value.

@section{Exposing Memory in Paren-x64}
@todo{Transition}

We first need to expose @ch1-tech{x64} features to access memory locations.
In particular, we expose @tech{displacement mode operands} for memory locations.
The @deftech{displacement mode operand} is a new operand that can appear in some location
positions as the argument or an instruction.
This allows accessing memory locations using pointer arithmetic.
It is written as @tt{QWORD [reg - int32]} or
@tt{QWORD [reg + int32]} in @ch1-tech{x64}, where @tt{reg} is a register
holding some memory address and the @tt{int32} is an offset number of bytes
from that address to access, as a 32-bit integer.
The keyword @tt{QWORD}, which is an unintuitive spelling of "four bytes",
indicates that this operand is accessing 64 bits at a time.

For example, if @tt{rbp} holds a memory address, we can move the value
@tt{42} to that memory address using the instruction
@tt{mov QWORD [rbp - 0], 42}.
We can move the value from memory into the register @tt{rax} using the
instruction @tt{mov rax, QWORD [rbp - 0]}.

Our offsets will be multiples of 8.
The offset is a number of bytes, and since we are dealing primarily with 64-bit
values, we will increment pointers in values of 8.
For example, the following snippet of code moves two values into memory, then
pulls them out and adds them.
@verbatim{
mov QWORD [rbp - 0], 21
mov QWORD [rbp - 8], 21
mov rax, QWORD [rbp - 8]
mov rbx, QWORD [rbp - 0]
add rax, rbx
}

These accesses grow @emph{downwards}, subtracting from the base pointer rather
than adding, following common conventions about how stack memory is used.
This is an arbitrary choice, but we choose to follow the convention.

The new version of @deftech{Paren-x64 v2} is below.

@bettergrammar*-diff[paren-x64-v1 paren-x64-v2]

We add the new nonterminal @paren-x64-v2[addr] to the language, and add
@paren-x64-v2[addr] as a production to @paren-x64-v2[loc].
The @paren-x64-v2[addr] nonterminal represents a displacement mode operand to an
instruction.
We abstract the language over the @deftech{base frame pointer}, the pointer to
the start of the current stack frame, which is stored in the parameter
@racket[current-frame-base-pointer-register] and is @paren-x64-v2[rbp] by default.
An @paren-x64-v2[addr] may only be used with the
@racket[current-frame-base-pointer-register] as its first operand.
The offset of each @paren-x64-v2[addr] is restricted to be an integer that is
divisible by 8, the number of bytes in a machine word in @ch1-tech{x64}.
This ensures all memory accesses are machine-word aligned, meaning we leave
space for all bytes in the word between each access.
Note that the offset is @emph{negative}; we access the stack backwards,
following the @ch1-tech{x64} "stack grows down" convention.
@todo{introduce this convention}

All languages in our compiler assume that the uses of
@racket[current-frame-base-pointer-register] obey the @deftech{stack discipline}, defined
below; all other uses are @tech{undefined behavior}.
Setting its value directly is forbidden.
Pointer arithmetic, such as @paren-x64-v2[(set! rbp (+ rbp opand))], is allowed
only when the @paren-x64-v2[opand] is a @racket[dispoffset?].
Incrementing the pointer beyond its initial value given by the run-time system
is forbidden.
We do not try to enforce these statically, since it may be impossible to do so
in general.
Instead, they must be @tech{undefined behavior}.

@digression{
The language is parameterized by the @racket[current-frame-base-pointer-register].
Parameterizing the language this way lets us avoid committing to particular
register choices, making the language inherently more machine agnostic.
This is helpful in designing a compiler with multiple machine backends.
A real compiler would want to support many machines, not just @ch1-tech{x64},
and parameterizing the language makes this simpler.
We could imagine retargeting a new machine by changing the value of
@racket[current-frame-base-pointer-register], among other parameters.
If our language definitions were sufficiently parameterized, few if any compiler
passes would need to differ between target machines.
This language is not suffiently abstract yet, but using parameterized languages
in this way is a common tool we will use.
}

The @racket[current-frame-base-pointer-register] is initialized by the run-time system.
Implementing this will require a new run-time system to initialize the stack.
From now on, the run-time system is provided by the @racketmodname[cpsc411/compiler-lib]
library.
@todo{Adjust run-time to work with either bytes or ptrs. Not sure how yet...}
The run-time system provides a default stack of size 8 megabytes.
This should be enough for now, but if it's not, you can use the
@racket[parameter?] @racket[current-stack-size] to increase it.

The new run-time system also prints the value of @paren-x64-v2[rax] to the
screen, instead of returning it via an exit code, and does the work of
converting numbers to ASCII strings.
Now when you run your binary, you'll get a more familiar output.
The @racket[execute] function will parse this output into a Racket number.
If you're interested in how this is done, you can read the definition of
@racket[x86-64-runtime].

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
Now that we have effectively unlimited @tech{physical locations} on the machine, we
want to begin abstracting away from machine details.

The first thing we do is to abstract away from the displacement mode operand,
introducing an abstract notion of @deftech{frame variable}, a variable that is
located in a particular slot on the frame.
This lets the programmer stop worrying about exactly what the displacement mode
offset contraints are.

We define @deftech{Paren-x64-fvars v2} below.
@bettergrammar*-diff[paren-x64-v2 paren-x64-fvars-v2]

We replace the notation of an address with that of an @paren-x64-fvars-v2[fvar],
which represents a unique location on the frame, relative to the current value
of @racket[current-frame-base-pointer-register].
These are written as the symbol @racket[fv] followed by a
number indicating the slot on the frame.
For example @paren-x64-fvars-v2[fv1] is the frame variables indicating the
first slot on the frame.
Frame variables are distinct from labels, alocs, and rlocs.
The number represents the index into the frame for the current function.

@todo{
The @share{a6-compiler-lib.rkt} defines a few helpers for working with frame
variables: @racket[fvar?], @racket[make-fvar], @racket[fvar->index], and
@racket[fvar->addr].
}

In @tech{Paren-x64-fvars v2}, it is still undefined behaviour to violate stack
discipline when using @racket[current-frame-base-pointer-register].

@nested[#:style 'inset
@defproc[(implement-fvars (p Paren-x64-fvars-v2.p))
         Paren-x64-v2.p]{
Compile the @tech{Paren-x64-fvars v2} to @tech{Paren-x64 v2} by reifying
@paren-x64-fvars-v2[fvar]s into displacement mode operands.
The pass should use @racket[current-frame-base-pointer-register].
}
]

This is a useful step toward an abstract assembly language, but we're still
forced to remember odd restrictions on which kind of @tech{physical location}
each instruction takes as an operand.
Our instructions in @tech{Paren x64-v2} are restricted by @ch1-tech{x64}.
For example, binary operations must use a register as their first operand.

We can create a new language which allows the user to ignore the differences
between @tech{physical locations}, enabling @emph{any} instruction to work on
any kind of @tech{physical locations}.
This way, the language is responsible for managing these annoying details
instead of the programmer.

We do this by defining @deftech{Para-asm v2}, a kind of less finiky assembly
language.
We can think of this language as parameterized by the set of locations, hence the name.

@todo{If I enable para-asm to be parameterized by the set of registers, could enable Kent's "unspillable" optimization.
To do that, para-asm lang needs to change to have undefined behavior.
Don't think I want to do that yet.}

@bettergrammar*-diff[paren-x64-v2 para-asm-lang-v2]

The main difference is in the @para-asm-lang-v2[s] non-terminal.
Now, instructions can use an arbitrary kind of @para-asm-lang-v2[loc] as their
operands.
The semantics are otherwise unchanged.
@para-asm-lang-v2[(set! loc triv)] moves the value of @para-asm-lang-v2[triv] into
@para-asm-lang-v2[loc], and @para-asm-lang-v2[(set! loc_1 (+ loc_1 triv))] adds the
values of @para-asm-lang-v2[loc_1] and @para-asm-lang-v2[triv], storing the result in
@para-asm-lang-v2[loc_1].
Note that the two occurrences of @para-asm-lang-v2[loc_1] in a binary operations are
still required to be identical.
We're not trying to lift all of the restrictions from @ch1-tech{x64} yet; we're
doing @emph{one thing well}.
@todo{This is actually a change in semantics from last semester.}
@todo{I've forgotten what that one thing is...}

We also add the @nested-asm-lang-v2[halt] instruction, which moves the the value of
its operand into the @racket[current-return-value-register] (@para-asm-lang-v2[rax]
by default).
Adding this abstraction frees the user from remembering which register is used for
this purpose.
This instruction is valid only as the final instruction executed in a program,
and it must be present.

Notice that two registers, @para-asm-lang-v2[r10], and @para-asm-lang-v2[r11], have been
removed from @para-asm-lang-v2[reg].
@tech{Para-asm v2} assumes control of these registers, forbidding the
programmer from using them directly.
Instead, the language implementation will make use of this when compiling to
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

@todo{Patch-instructions is an uninformative name. concretize-machine?}
@nested[#:style 'inset
@defproc[(patch-instructions (p para-asm-lang-v2?))
          paren-x64-fvars-v2?]{
Compile the @tech{Para-asm v2} to @tech{Paren-fvars-x64 v2} by patching
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

@;@todo{Maybe this comes too early; in Jeremy's assignment, I had an easier time
@;doing select instructions from ANF. OTOH, taking that route, it's hard to insert
@;register allocation after the fact. In Kent's, it comes several assignments
@;later.}

@section{Nesting Instruction Sequences}
@todo{This is sort of crowbarred in. Really belongs in "imperative abstractions", which is all about composition. But that requires a lot of motivation in that chapter, and a lot of new typesetting.}
One of the most restrictive limitations in our language is that expressions
cannot be nested.
Each program must be a linear sequence of instructions.
@todo{motivation is weak here}

We can easily lift this restriction by designing a language that supports
nesting, and compiling it using our well-known operations for composing
@tech{instruction sequences}.
This simplifies the job of later passes that are now free to nest instructions
if it's helpful.

Below, we design @deftech{nested-asm-lang-v2}.

@bettergrammar*-diff[para-asm-lang-v2 nested-asm-lang-v2]

We add a @para-asm-lang-v2[tail] production which loosely corresponds to the
top-level program from @tech{Para-asm v2}.
However, these can be nested, before eventually ending in a @nested-asm-lang-v2[halt]
instruction.
The @nested-asm-lang-v2[tail] production represents the "tail", or last, computation
in the program.

We also enable nesting in effect position.
This essentially allows to copy and paste some @tech{instruction sequence} into
the middle of a program.

@nested[#:style 'inset
@defproc[(flatten-begins [p nested-asm-lang-v2]) para-asm-lang-v2]{
Flatten all nested @nested-asm-lang-v2[begin] expressions.
}
]

@section{Abstracting Physical Locations}
We are still required to think about @tech{physical locations}---the registers
and frame variables of @ch1-tech{x64}.
We don't usually care which location a value is stored in, so long as it is
stored @emph{somewhere}.

We can introduce an abstraction to capture this idea.
We define an @deftech{abstract location} to be a globally unique name for some
@tech{physical location}.
@todo{I think they aren't globally unique, but unique within a block.}

We define @deftech{Asm-lang v2} below.
@tech{Asm-lang v2} is an imperative, assembly-like language.

@bettergrammar*-diff[nested-asm-lang-v2 asm-lang-v2]

In @tech{Asm-lang v2}, we generalize instructions to work over abstract
location, @asm-lang-v2[aloc].
An @asm-lang-v2[aloc] is a symbol that is of the form @code{<name>.<number>};
this is captured by the @racket[aloc?] predicate.
Note that this overlaps with the definition of @racket[fvar?], but we will never
be able to compare the two, so we don't feel a need to distinguish them.
We assume all @asm-lang-v2[aloc]s are globally unique, and any reference to
the same @asm-lang-v2[aloc] is to the same location---these are not the
@ch3-tech{names} yet.

Implementing @tech{Asm-lang v2} is a multi-step process.
We gather up all the @tech{abstract locations}, then assign them to
@tech{physical locations}.
@todo{'Implementing' concretely means compiling to nested-asm v2 right?
This should probably be moved to the beginning of this section and
also be more elaborate.}

For each step, we create an @deftech{administrative language}---an intermediate
language whose semantics does not differ at all from its parent language, but
whose syntax is potentially decorated with additional data that simplifies the next
step of the compiler.
We represent this additional data in an @deftech{info field}, an annotation in
the program that serves only to store additional information for compilation.
This additional information is often the result of some program analysis or
preprocessing step that informs the next compiler pass.
In the parent language, @tech{Asm-lang v2} in this case, the @tech{info field}
is unrestricted---it could contain anything at all.
In fact, we can view the parent language as a family of languages, each
differing in its @tech{info field}.

We represent the @tech{info field} as an association list of keys to proper list
whose first element is the value of the key, such as @racket[((key value))].
The module @racketmodname[cpsc411/info-lib] provides utilities for working with
this representation.
In general, we will only give a partial specification---it may contain arbitrary
other key-value pairs, indicated by the @racket[any/c] pattern in the
definition.
This lax specification is useful for debugging: you may leave residual
@asm-lang-v2[info] from earlier languages, or include your own debugging
@asm-lang-v2[info].
The @asm-lang-v2[info] field is also unordered, so it should be accessed
through the @racketmodname[cpsc411/info-lib] interface.

@digression{
In a production compiler, we would probably not represent these
@tech{administrative languages} at all, but instead store the contents of the
@tech{info field} "on the side", as a separate data structure.
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
perhaps a a hash table.
However, this representation is simpler to read and write in program text.
}

First, we analyze the @tech{Asm-lang v2} to discover which
@tech{abstract locations} are in use.
This is a straight-forward analysis, traversing the program and collecting any
@tech{abstract location} we see into a set.

We define the @tech{administrative language} @deftech{Asm-lang v2/locals}
(pronounced "Asm lang v2 with locals") to capture this information.

@bettergrammar*-diff[asm-lang-v2 asm-lang-v2/locals]

The only difference is in the @tech{info field}, which now contains a
@deftech{locals set}, a @racket[set?] of all @tech{abstract locations} used in
the associated @asm-lang-v2[tail].
In this language, there is an invariant that any @tech{abstract location} that
is used must appear in the locals set.

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
fresh @tech{frame variable}.

We capture this information in a new @tech{info field}, described in
@deftech{Asm-lang-v2/assignments}.

@bettergrammar*-diff[asm-lang-v2/locals asm-lang-v2/assignments]

The @asm-lang-v2/assignments[assignment] @tech{info field} records a mapping
from each @tech{abstract location} to some @tech{physical location}.
The language is more general than our implementation strategy; we allow an
@tech{abstract location} to be assigned to any valid @tech{physical location},
including registers, to permit future optimizations or hand-written code.

@nested[#:style 'inset
@defproc[(assign-fvars (p Asm-lang-v2/locals.p))
          Asm-lang-v2/assignments.p]{
Compiles @tech{Asm-lang v2/locals} to @tech{Asm-lang v2/assignments},
by assigning each @tech{abstract location} from the @asm-lang-v2/locals[locals]
@tech{info field} to a fresh @tech{frame variable}.

@todo{Add examples back in}
@;@examples[#:eval sb
@;(assign-fvars
@;  '(begin ((locals (x.1)))
@;     (set! x.1 0)
@;     (halt x.1)))
@;
@;(assign-fvars
@; '(begin ((locals (x.1 y.1 w.1)))
@;    (set! x.1 0)
@;    (set! y.1 x.1)
@;    (set! w.1 (+ x.1 y.1))
@;    (halt w.1)))
@;]
}
]

Finally, we simply replace all the @tech{abstract locations} with
the @tech{physical location} assigned by @racket[assign-fvars].

@nested[#:style 'inset
@defproc[(replace-locations (p asm-lang-v2/assignments?))
          nested-asm-lang-v2?]{
Compiles @tech{Asm-lang v2/assignments} to @tech{Nested-asm v2},
replaced each @tech{abstract location} with its assigned @tech{physical
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
      (set! w.1 (+ w.1 y.1))
      (halt w.1))))
]
]

As these three passes implement a single true language, we wrap them up as a
single pass.

@nested[#:style 'inset
@defproc[(assign-homes (p asm-lang-v2?))
         nested-asm-lang-v2?]{
Compiles @tech{Asm-lang v2} to @tech{Nested-asm v2}, replacing each
@tech{abstract location} with a @tech{physical location}.
}
]

@section{Source Language Polishing}
@todo{Add validator to remove undefined behaviour}

@section{Appendix: Overview}

@figure["fig:v2-graph" "Overview of Compiler Version 2" v2-graph]

@section{Appendix: Language Definitions}

@declare-exporting[cpsc411/langs/v2]

@deflangs[
asm-lang-v2
asm-lang-v2/locals
asm-lang-v2/assignments
nested-asm-lang-v2
para-asm-lang-v2
paren-x64-fvars-v2
paren-x64-v2
]
