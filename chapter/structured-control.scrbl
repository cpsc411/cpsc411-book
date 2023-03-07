#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  (for-syntax racket/base)
  scriblib/figure
  scribble/racket
  (for-label cpsc411/reference/a4-solution)
  cpsc411/reference/a4-solution
  (for-label cpsc411/langs/v4)
  cpsc411/langs/v2
  cpsc411/langs/v3
  cpsc411/langs/v4
  (for-label (except-in cpsc411/compiler-lib compile)))

@(provide
  (except-out (all-defined-out) sb))

@; TODO: Copy/paste from scribble/bettergrammar, and value-orientation
@(define-for-syntax datum-literal-transformer
   (make-element-id-transformer
    (lambda (x)
      (quasisyntax/loc x
        (elem #:style symbol-color (to-element '#,x))))))

@declare-exporting[cpsc411/reference/a4-solution]

@(define sb
   (make-cached-eval
    "ch4-eval"
    '(require cpsc411/reference/a4-solution cpsc411/compiler-lib)
    '(current-stack-size 512)))

@; ----- Language Defs ------

@define[v4-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

L0 [label="Values-lang v4"];
L1 [label="Values-unique-lang v4"];
L2 [label="Asm-pred-lang v4"];
L15 [label="Imp-mf-lang v4"];
L16 [label="Imp-cmf-lang v4"];
L7 [label="Nested-asm-lang v4"];
L8 [label="Block-pred-lang v4"];
L9 [label="Block-asm-lang v4"];
L17 [label="Paren-x64-fvars v4"];
L10 [label="Para-asm-lang v4"];
L13 [label="x64"];
L14 [label="int64"]

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

  L3 [label="Asm-pred-lang v4/locals"];
  L4 [label="Asm-pred-lang v4/undead"];
  L5 [label="Asm-pred-lang v4/conflicts"];
  L6 [label="Asm-pred-lang v4/assignments"];
}

edge [fontname="Courier", fontsize=12, labeljust=right]

L2 -> L3 [label=" uncover-locals"];
L3 -> L4 [label=" undead-analysis"];
L4 -> L5 [label=" conflict-analysis"];
L5 -> L6 [label=" assign-registers"];
L6 -> L7 [label=" replace-locations"];

L2 -> L7 [label=" assign-homes-opt"];


L0 -> L1 [label=" uniquify"];
L1 -> L15 [label=" sequentialize-let"];
L15 -> L16 [label=" normalize-bind"];
L16 -> L2 [label=" select-instructions"];

L7 -> L7 [label=" optimize-predicates"];
L7 -> L8 [label=" expose-basic-blocks"];
L8 -> L9 [label=" resolve-predicates"]
L9 -> L10 [label=" flatten-program"];
L10 -> L17 [label=" patch-instructions"];
L17 -> L11 [label=" implement-fvars"];
L11 -> L13 [label=" generate-x64"];
L13 -> L14 [label=" execute"];

subgraph DoNotcluster1 {
  graph [labeljust=right,
    style=filled,
    color=lightgrey,
    fontname="Courier",
    fontsize=10,
    label = "interp-paren-x64";
  ];
  edge [fontname="Courier"]

  L11 [label="Paren-x64 v4"];
  L12 [label="Paren-x64-rt v4"];
}

  L11 -> L12 [label=" link-paren-x64"];
  L12 -> L14 [label=" interp-loop"];
  L11 -> L14 [label=" interp-paren-x64"];
}
}
]

@todo{opand, or triv?
(Lily: I converted the misc. opands to triv so the diffs display correctly.)}

@; --------------------------

@title[#:tag "top" #:tag-prefix "chp4:"]{Structured Control Flow}

@section{Preface: What's wrong with our language?}
For our last abstraction, we designed the language @ch3-tech{Values-lang v3}.
This language is an improvement over @ch1-tech{x64}, but has a significant
limitation: we can only express simple, straight-line arithmetic
computations. We'll never be able to write any interesting programs!

In this chapter, we will expose a machine feature, control flow instructions
in the form of labels and @tt{jmp} instructions, and systematically
abstract these into a structured control-flow primitive: @values-lang-v4[if]
expressions.
Control flow is complex, and adding it requires changes to nearly every pass and
every intermediate language.

The overview of this version of the compiler is given in
@Figure-ref{fig:v4-graph}.

@section{Designing a source language with structured control flow}
As usual, we'll start with our goal and then design the compiler bottom-up.
We want to extend @ch3-tech{Values-lang v3} with a structured control-flow
feature: a form of @values-lang-v4[if] expression.

Our goal is @tech{Values-lang v4}, duplicated below.

@bettergrammar*-ndiff[
#:labels ("Diff" "Values-lang v4" "Values-lang v3")
(values-lang-v3 values-lang-v4)
(values-lang-v4)
(values-lang-v3)
]

Note that an @values-lang-v4[if] expression, @values-lang-v4[(if pred e e)], is
limited: it cannot branch on an arbitrary expression.
It is restricted so that a comparison between two values must appear in the
predicate position.
This is a necessary artifact we'll inherit from @ch1-tech{x64}.
Even by the end of this chapter, we will not yet have enough support from the
low-level language to add values that represent the result of a comparison,
@ie we won't have booleans.

@digression{
This is a design choice.
With the abstraction we have so far, we could add pseudo-boolean expressions by
giving an arbitrary interpretation to any values in the predicate position.
For example, we could interpret @racket[0] as true and @racket[1] as false, or
vice versa.
Unfortunately, this would expose our source language to undefined
behaviour---what happens when an @values-lang-v4[if] expression branches on any
other value?
We could remedy this slightly by making any non-zero integer true, for example.
However, it also means we become unable to distinguish booleans from integers,
leading to type confusion when reading and writing data.
If a programmer sees the number @racket[-5] printed or in a program, is it a
boolean or a number?

There are many ways to solve the problem, but to keeping with our design
principles of designing clear abstraction boundaries, we will later add proper
booleans as a separate primitive data type.
That is a task separate from adding control flow, so we deal with it later, and
instead implement a limited form of structured control-flow first that booleans
can compile to later.
}

@section{Exposing Control-Flow Primitives}
When we want to add a new feature to the source language, we must always ask if
there's some existing abstraction in the target language that we can use.
So far, @ch2-tech{Paren-x64 v2} exposes only intructions to move data between
locations and perform simple arithmetic computation on locations.
This is insufficiently expressive, so we must reach even lower and expose new
primitives from the machine.

Thankfully, the machine does expose a primitive.
@ch1-tech{x64} exposes labels, written @tt{l:} before some instruction, and
jumps, written @tt{jmp trg} where @tt{trg} is either a label or a
register containing a label.
Labels can be chained in @ch1-tech{x64}, as we've seen in prior assignments.
For example, the following assigns two labels to the same instruction:
@;
@verbatim{
L1:
L2:
  mov rax, 42
}

To use labels, we have a variety of @tt{jmp} instructions, which take a label
and cause control to move to that label, instead of the next instruction.
In this example, @tt{rax} is set to @tt{5} at the end of the program, as the
instruction that sets @tt{rax} to @tt{42} is not executed due to the @tt{jmp}
instruction.
@verbatim{
  mov rax, 5
  jmp L1
  mov rax, 42
L1:
}

We'll begin the next version of our compiler by designing a new
@deftech{Paren-x64 v4} to expose the additional features of @ch1-tech{x64}
necessary to implement control flow abstractions.
This extends the previous @ch2-tech{Paren-x64 v2} with comparison operations,
labels, and conditional and unconditional jump operations.

@bettergrammar*-ndiff[
#:labels ("Diff" "Paren-x64 v4" "Paren-x64 v2")
(#:exclude (reg addr fbp binop) paren-x64-v2 paren-x64-v4)
(paren-x64-v4)
(paren-x64-v2)
]

Labels are too complex to define by grammar; instead, they're defined by the
@racket[label?] predicate in @racketmodname[cpsc411/compiler-lib].

NASM and @ch1-tech{x64} impose some additional restriction on labels, some of
which we've discussed already.
@itemlist[
@item{Recall that a @tt{mov} instruction to an @paren-x64-v4[addr] can only move
32-bit integers literals.
While on a 64-bit machine, labels could be 64-bits, the SYS V ABI also specifies
a @emph{small code} model.
In this model, all labels are 32-bits, so can be moved into memory directly.
}
@item{The only valid characters in labels are letters, numbers, _, $, #, @"@",
~, ., and ?. The only characters which may be used as the first character of an
identifier are letters, . (which has a special meaning and shouldn't be used), _
and ?.
}
@item{Labels cannot be longer than 4095 characters.}
]

The library function @racket[sanitize-label] turns a @racket[label?] into a
string that NASM will accept, as long as it is not too long.

In @tech{Paren-x64 v4}, we model labels with the @paren-x64-v4[(with-label label s)]
instruction, which defines a label @paren-x64-v4[label] at the instruction
@paren-x64-v4[s] in the instruction sequence.
This corresponds to the @ch1-tech{x64} string @tt{label:\n s}.
Note that they can be nested, allowing the same behaviour as chaining labels in
@ch1-tech{x64}.
For convenience, we assume all labels are symbols of the form
@paren-x64-v4[L.<name>.<number>], and are globally unique.

Note that @paren-x64-v4[(with-label label s)] does @emph{not} behave like a
@racket[define] in Racket or in @ch3-tech{Values-lang v3}.
The instruction @paren-x64-v4[s] gets executed after the previous instruction in
the sequence, even if the previous instruction was not a jump.
@paren-x64-v4[with-label] additionally names the instruction so that we can jump
to it later, the same as a label in @ch1-tech{x64}.

The new comparison instruction @paren-x64-v4[(compare reg opand)] corresponds to
the @ch1-tech{x64} instruction @tt{cmp reg, opand}.  This instruction
compares @paren-x64-v4[reg] to @paren-x64-v4[opand] and sets some flags in the
machine describing their relation, such as whether @paren-x64-v4[reg] is less
than @paren-x64-v4[opand], or whether they are equal.  The flags are used by the
next conditional jump instruction.
If the @paren-x64-v4[opand] is an integer literal, it must restricted to
32-bits.

The conditional jump instructions in @ch1-tech{x64}, in the same order as the
definition of @paren-x64-v4[relop], are: @tt{jl label},
@tt{jle label}, @tt{je label},
@tt{jge label}, @tt{jg label}, and @tt{jne label}
and each corresponds to "jump to @paren-x64-v4[trg] if the comparison flag is
set to ___".
For example, the instruction @tt{je label} jumps to @paren-x64-v4[label]
if the comparison flag "equal" is set.

In @tech{Paren-x64 v4}, we abstract the various conditional jump instructions
into a single instruction with multiple flags.
The instruction @tt{je l} corresponds to @paren-x64-v4[(jump-if = l)].
@tt{jl l} jumps to @paren-x64-v4[l] if comparison flag "less than" is
set, and corresponds to @paren-x64-v4[(jump-if < l)].
The rest of the instructions follow this pattern.

We make an additional simplifying restriction in @tech{Paren-x64 v4} compared to
@ch1-tech{x64}.
We assume that a @paren-x64-v4[compare] instruction is always followed
immediately by a @paren-x64-v4[jump-if], and similarly, that any
@paren-x64-v4[jump-if] is immediately preceeded by a @paren-x64-v4[compare].
This is necessay since other instructions in @ch1-tech{x64}, such as binary
operations, can affect comparison flags.
However, we do not want to try to reason about how the flags are affected by
arbitrary comparison, and our compiler will always generate a
compare-and-then-conditional-jump sequence of instructions.

To implement @tech{Paren-x64 v4}, we define the procedure @racket[generate-x64],
which simply converts each instruction to its @ch1-tech{x64} string form.

@nested[#:style 'inset
@defproc[(generate-x64 (p paren-x64-v4?))
         (and/c string? x64-instructions?)]{
Compile the @tech{Paren-x64 v4} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}
]

@subsection{The semantics of labels and jumps as linking}
Labels and jumps are a small change to the language syntactically, but have a
large effect on the semantics.
We can see this by writing an interpreter for the language with labels and jumps
and comparing it to an interpreter for a language without them.

We can no longer write the @tech{Paren-x64 v4} interpreter in one simple loop
over the instructions.
Instead, we need some way to resolve labels.
That way, when running the interpreter, we must be able to jump to any
expression at any time---a possibility the language now allows.
This process of resolving labels is called @deftech{linking}.

@margin-note*{
We only cover @tech{linking} local labels in this chapter.
More generally, @tech{linking} requires resolving references to
external labels, @ie, linking imported libraries, and in the context of many
operating systems, preparing some additional metadata so the operating system
knows where to begin executing.
The more general instance is conceptually similar to what we present here---the
key concept is transforming the abstraction of labels to code and data into some
encoding used by the underlying implementation.
}

We can view the process of linking as yet another compiler, and thus a language
design problem.
We design a simple linker for @tech{Paren-x64 v4} to give you a rough idea of
how the operating system's linker works.
We use a low-level linking implementation that is similar to the operating
system's linker: we first resolve all labels to their address in memory (in our
case, their index in the instruction sequence) and then implement jumps by
simply setting a program counter to the instruction's address.

To do this, we design a new language @deftech{Paren-x64-rt v4}
(@racket[paren-x64-rt-v4]), which represents the @emph{r}un-@emph{t}ime language
used by the interpreter after linking.

@bettergrammar*-ndiff[
#:labels ("Diff" "Paren-x64-rt v4" "Paren-x64 v4")
(#:exclude (reg triv opand loc binop addr fbp relop) paren-x64-v4 paren-x64-rt-v4)
(paren-x64-rt-v4)
(paren-x64-v4)
]

We remove the instruction @paren-x64-rt-v4[(with-label label s)] and turn all label
values into @paren-x64-rt-v4[pc-addr], a representation of an address recognized by
the interpreter.
This encodes the idea that the linker, the compiler from @tech{Paren-x64 v4} to
@tech{Paren-x64-rt v4}, resolves labels into addresses.
In our case, since programs are represented as lists of instructions, a
@paren-x64-rt-v4[pc-addr] is a natural number representing the position of the
instruction in the list.

To implement @tech{Paren-x64-rt v4} and thus perform linking, we define
the procedure @racket[link-paren-x64].

@nested[#:style 'inset
@defproc[(link-paren-x64 (p paren-x64-v4?))
         paren-x64-rt-v4?]{
Compiles @tech{Paren-x64 v4} to @tech{Paren-x64-rt v4} by resolving all labels
to their position in the instruction sequence.
}
]

Now we can give a semantics to @tech{Paren-x64-rt v4}, and thus give a semantics
to @tech{Paren-x64 v4} and understand the meaning of labels and jumps.
We define an interpreter @racket[interp-paren-x64] for @tech{Paren-x64 v4} by
first linking using @racket[link-paren-x64], and then running an interpreter
for @tech{Paren-x64-rt v4}.
The main loop of the @tech{Paren-x64-rt v4} interpreter uses a program counter
to keep track of its position in an instruction sequence.
To interpret a jump, we simply change the program counter to the number
indicated in the jump.

@nested[#:style 'inset
@defproc[(interp-paren-x64 (p paren-x64-v4?))
         int64?]{
Interpret the @tech{Paren-x64 v4} program @racket[p] as a value, returning the
final value of @paren-x64-v4[rax].
}

@defproc[(interp-loop (code (listof paren-x64-rt-v4.s))
                      (env dict?)
                      (pc natural-number/c))
         int64?]{
The main loop of the interpreter for @tech{Paren-x64-rt v4}.
@racket[code] does not change.
@racket[env] is a @racket[dict?] mapping @ch2-tech{physical locations} (as
@racket[symbol?]s) to their values (@racket[int64?]).
@racket[pc] is the program counter, indicating the current instruction being
executed as an index into the list @racket[code].
}
]

@subsection{Finding the next abstraction boundary}
Having exposed @ch1-tech{x64} features to our compiler internal languages, we
now need to find the right boundary at which to abstract away from the
low-level representation of control-flow---labels and jumps---and introduce a
more structured form of control flow, @values-lang-v4[if].

@todo{Opportunity for a "finger exercise"}

Our next two languages in the pipeline, bottom-up, are @tech{Paren-x64-fvars
v4} and @tech{Para-asm-lang v4}.
Both of these languages abstract machine-specific details about
@ch2-tech{physical locations}.
This doesn't seem very related to control-flow, so we simply want to propagate
our new primitives up through these layers of abstraction.
We expose the new instructions while abstracting away from the machine
constraints about which instructions work on which physical locations.
Now jumps can target arbitrary locations, and @para-asm-lang-v4[compare] can
compare arbitrary locations.
We can also move labels into arbitrary locations.
@todo{Talk about why trg and not just "loc"}

@todo{Talk about opand vs triv}

Below we typeset @deftech{Paren-x64-fvars v4} (@racket[paren-x64-fvars-v4]).

@bettergrammar*-ndiff[
#:labels ("Diff" "Paren-x64-fvars v4" "Paren-x64-fvars v2")
(#:exclude (reg binop int64 int32 fvar) paren-x64-fvars-v2 paren-x64-fvars-v4)
(paren-x64-fvars-v4)
(paren-x64-fvars-v2)
]

Nothing important changes in @tech{Paren-x64-fvars v4}.
We simply add the new control-flow primitives.

@nested[#:style 'inset
@defproc[(implement-fvars (p paren-x64-fvars-v4?))
         paren-x64-v4?]{
Compile the @tech{Paren-x64-fvars v4} to @tech{Paren-x64 v4} by reifying
@paren-x64-fvars-v4[fvar]s into displacement mode operands.
The pass should use @racket[current-frame-base-pointer-register].
}
]

Next we typeset @deftech{Para-asm-lang v4}.

@bettergrammar*-ndiff[
#:labels ("Diff" "Para-asm-lang v4" "Para-asm-lang v2")
(#:exclude (reg binop int64) para-asm-lang-v2 para-asm-lang-v4)
(para-asm-lang-v4)
(para-asm-lang-v2)
]

While @para-asm-lang-v4[halt] is still an instruction, we must handle it
differently now that we have control flow.
@para-asm-lang-v4[halt] is a control-flow effect, immediately ending the control
flow of the program, similar to @racket[exit].
Previously, this did not matter, since we had no control flow that could be
affected, and we ensured @para-asm-lang-v4[halt] was the final instruction.
Now, we have control flow.

We restrict @tech{Para-asm-lang v4}, requiring that there is exactly one
@emph{dynamic} halt and that it is the final instruction executed in the
program.
We cannot restrict the syntax to require this, since we now support jumps.
Jumps mean our syntax does not give us a clear indication of which instruction
is executed last.
It might be the case that @para-asm-lang-v4[halt] is the second instruction in
the instruction sequence, but is always executed last because of the control
flow of the program.
It could also be that there are multiple @para-asm-lang-v4[halt] instructions
syntactically, but only one will ever be executed due to conditional jumps.

This means compiling @para-asm-lang-v4[halt] is slightly more complicated.
When implementing @para-asm-lang-v4[halt], we must @emph{ensure} that
@para-asm-lang-v4[halt] is the last instruction executed.
Previously, all @para-asm-lang-v4[halt] had to do was set the appropriate
register, since it was always the final instruction.
When compiling @para-asm-lang-v4[halt], our job is to ensure our restrictions
about @para-asm-lang-v4[halt] when compiling.
This is simple to do by adding a unique label after all instructions in the
program, and implementing @para-asm-lang-v4[halt] with a jump to that label.
@margin-note{
For convenience, the @racketmodname[cpsc411/2c-run-time] provides such a label,
@(let-syntax ([done datum-literal-transformer]) @racket[done]), which
your implementation can use instead.
}

@;We continue to support nested @para-asm-lang-v4[tail]s for backwards compatibility,
@;but it will turn out that we no longer generate these in @tech{Para-asm-lang v4}.
@todo{Is that true? Kent gets rid of this in expose-basic-blocks, but my ebb
comes earlier in the pipeline.}

To implement @tech{Para-asm-lang v4}, we extend @racket[patch-instructions].
The implementation is essentially similar to the definition from @Chref[3 "top"].

The tricky operation to support is @para-asm-lang-v4[(jump-if relop loc)], since
@ch1-tech{x64} gives us only @para-asm-lang-v4[(jump-if relop label)].
We can do this by generating a 2-instruction sequence and negating the
@para-asm-lang-v4[relop].
@todo{This requires adding jne, because we don't do this in select-instructions
where we have predicates.}

@nested[#:style 'inset
@defproc[(patch-instructions (p para-asm-lang-v4?))
         paren-x64-fvars-v4?]{
Compiles @tech{Para-asm-lang v4} to @tech{Paren-x64-fvars v4} by patching
each instruction that has no @ch1-tech{x64} analogue into a sequence of
instructions using auxiliary register from
@racket[current-patch-instructions-registers].
}
]

@section{New Abstractions: Blocks and Predicates}
Working our way up the pipeline, the next language from the previous version of
our compiler is the @ch2-tech{Asm-lang v2} family of languages.
Recall that this family of languages includes several @ch2-tech{administrative
languages}, including @ch2-tech{Asm-lang v2/assignments}.
This is the output language of the register allocator.

So here we must stop and ask: is @ch2-tech{Asm-lang v2} the right place to start
from when abstracting away from labels and jumps?

For that, we need to think about what happens in @ch2-tech{Asm-lang v2}.
The register allocation and related analyses all happen in @ch2-tech{Asm-lang v2}.
If we continue to propagate the primitives up, then the register allocator
will be forced to deal with labels and jumps.
If we abstract away, then we can design an abstraction that might work better
with the register allocator and the analyses.

When control can jump to any instruction at any time, giving semantics to
programs, such as writing an interpreter or an analysis, is very difficult.
At any label, we must make assumptions about the state of the machine, since the
state is not affected only by the sequence of instructions that came before the
label in the instruction sequence, but potentially arbitrary instructions that
were executed prior to a jumping to the label.
This is why we introduced a linking pass before the interpreter.
We do not want to have to deal with linking during register allocation.

We therefore want to abstract away from labels and jumps before register
allocation.

@digression{
There are advantages to making the register allocator more aware of labels and
jumps.
We could write a more complex analysis that tries to resolve labels and jumps,
essentially resolving linking and then doing the analysis over the linked
program.
This would give the register allocator more accurate information about control
flow, allowing it to do a better job of minimizing register conflicts, but at
the expense of a more complex and slower analysis.
}

@question{Thinking ahead, what is the problem with analyzing jump instructions?
Which parts of the register allocator must change to handle them: conflict
analysis, undead analysis, register allocation, or some combination of the
three?}

@todo{These design digressions are opportunities for design exercises.}
@todo{This digression is a bit early, isn't it?}

To simplify reasoning about programs with control flow, we can organize code
into @deftech{basic blocks}, labeled blocks where control can only enter the
beginning of the block and must exit at the end of the block.
This gives us more structure on which to hang assumptions, and can make more
assumptions about code when writing analyses.
In particular, we will be able to annotate which registers are @ch-ra-tech{undead} on
entry to and on exit from a block, so our analysis does not have to resolve
labels and jumps.

We need to develop this @tech{basic block} abstraction before we get to the
register allocator, so we introduce it next, as an abstraction of
@tech{Para-asm-lang v4}.

We design @deftech{Block-asm-lang v4}, a basic-block-structured abstract assembly
language in which sequences of statements are organized into basic blocks, and
code can jump between blocks.
Labels are no longer instructions that can happen anywhere; instead, each block
is labeled.
Jumps cannot appear just anywhere; instead, they happen only at the end of a
block.

@bettergrammar*-ndiff[
#:labels ("Diff" "Block-asm-lang-v4" "Para-asm-lang-v4")
(#:exclude (reg relop binop int64 aloc fvar label) para-asm-lang-v4 block-asm-lang-v4)
(block-asm-lang-v4)
(para-asm-lang-v4)
]

In @tech{Block-asm-lang v4}, a program is a non-empty sequence of labeled
blocks. We consider the first block in the sequence to be the start of the
program.
A @para-asm-lang-v4[tail] represents a self-contained block of statements.
Jumps can only appear at the end of blocks, and jumps only enter the beginning
of blocks.

The basic block abstraction essentially forces us to add an @block-asm-lang-v4[if]
statement.
We want to ensure jumps happen only at the end of a block, but how could that be
if we only have separate @para-asm-lang-v4[jump-if] instructions as in
@tech{Para-asm-lang v4}?
At the very least, we would need to support a block that ends in three
instruction sequences: @para-asm-lang-v4[compare], followed by a
@para-asm-lang-v4[jump-if], followed by a @para-asm-lang-v4[jump].
This is the low-level implementation of an @block-asm-lang-v4[if] statement.
Rather than trying to recognize a three-instruction sequence, we simply abstract
the sequence into a single instruction: @block-asm-lang-v4[(if (relop loc opand)
(jump trg) (jump trg))].
This buys us simplicity in analyzing basic blocks.

The @block-asm-lang-v4[halt] instruction should only be executed at the end of the
final block; it cannot stop control flow, but only indicates that if the program
has ended, the @block-asm-lang-v4[opand] is the final value.
Again, we cannot enforce this syntactically due to jumps.
Instead, we require that @block-asm-lang-v4[halt] appears at the end of a block, and
assume only one @block-asm-lang-v4[halt] instruction is ever executed during
execution.

To implement @tech{Block-asm-lang v4}, we simply flatten blocks, moving the
@block-asm-lang-v4[label] from the @block-asm-lang-v4[define] to the first
instruction in the block using @para-asm-lang-v4[with-label].

@nested[#:style 'inset
@defproc[(flatten-program (p block-asm-lang-v4?))
          para-asm-lang-v4?]{
Compile @tech{Block-asm-lang v4} to @tech{Para-asm-lang v4} by flattening basic
blocks into labeled instructions.
}
]
@todo{Why block-asm-lang and para-asm?}

@subsection{Designing A Language for Optimization}
When introducing a new statement or expression, we should ask ourselves: what
equations do we want to be true of this expression?
For example, should we be able to rewrite

@block-pred-lang-v4[(if (< 0 1) (jump trg_1) (jump trg_2))]

to @block-pred-lang-v4[(jump trg_1)]?

This would be ideal, as it optimizes away the predicate test.
What about this: are the following two programs equivalent?

@block-pred-lang-v4[(if (< 0 1) (jump trg_1) (jump trg_2))]

@block-pred-lang-v4[(if (>= 0 1) (jump trg_2) (jump trg_1))]

This... should be true, but it's less obvious why we might do this.
But perhaps there are cases where @block-pred-lang-v4[>=] is faster than
@block-pred-lang-v4[<], or perhaps for some reason we would like @block-pred-lang-v4[(jump
trg_1)] to be the final jump because another optimization would be able to
inline that jump.

While it would be straightforward to write an analysis to support these
transformations, we could do better by recognizing a pattern and introducing an
abstraction.
In the first case, what we @emph{really} want to do is transform any expression
where the predicate is @emph{obviously true}---we'll write this as
@block-pred-lang-v4[(true)].
Then we could write a simple optimization to transform @block-pred-lang-v4[(if (true)
(jump trg_1) (jump trg_2))] into @block-pred-lang-v4[(jump trg_1)].
Similarly, if we had a predicate that was obviously false, written
@block-pred-lang-v4[(false)], we could rewrite
@block-pred-lang-v4[(if (false) (jump trg_1) (jump trg_2))] into @block-pred-lang-v4[(jump
trg_2)].
If we had a language with a @emph{predicate} abstraction, we could separate the
@emph{analysis} of which comparisons are obvious from the @emph{optimization}
that rewrites @block-pred-lang-v4[if] statements with obvious predicates.

We therefore introduce the language @deftech{Block-pred-lang v4}.
It introduces @block-pred-lang-v4[pred] position.
A @block-pred-lang-v4[pred] is @emph{not} a boolean; we can easily compile all
@block-pred-lang-v4[pred]s into either a simple @block-pred-lang-v4[(relop loc opand)] or
eliminate them entirely.
They exist as a way to express the output of some analysis over predicates and
enable us to easily rewrite @block-pred-lang-v4[if] statements.

@bettergrammar*-ndiff[
#:labels ("Diff" "Block-pred-lang v4" "Block-asm-lang v4")
(#:exclude (reg binop relop int64 aloc fvar label trg triv) block-asm-lang-v4 block-pred-lang-v4)
(block-pred-lang-v4)
(block-asm-lang-v4)
]

The @block-pred-lang-v4[pred] position allows @block-pred-lang-v4[relop]s as
before, but also obviously true and false predicates, and predicate negation.
This abstraction gives some later pass the ability to optimize
@block-pred-lang-v4[(> 1 0)] to @block-pred-lang-v4[(true)].

Obvious predicates, like @block-pred-lang-v4[(true)] and @block-pred-lang-v4[(false)] simply
compile by transforming the @block-pred-lang-v4[if] statement into either the first or
second branch.
The negation predicate, @block-pred-lang-v4[(not pred)], swaps the branches and
continues compiling @block-pred-lang-v4[(if pred (jump trg_2) (jump trg_1))].
We leave the @block-pred-lang-v4[relop] predicate alone.

We implement @tech{Block-pred-lang v4} with a simple compiler,
@racket[resolve-predicates].

@nested[#:style 'inset
@defproc[(resolve-predicates (p block-pred-lang-v4?))
         block-asm-lang-v4?]{
Compile the @tech{Block-pred-lang v4} to @tech{Block-asm-lang v4} by
manipulating the branches of @block-pred-lang-v4[if] statements to resolve
branches.
}
]

Note that this pass is not an optimization.
Optimization passes are intra-language.
However, its existence allows us to implement an optimization pass by
transforming predicates in the predicate language.
We delay writing this optimization for one more language, as an additional
abstraction will help us unlock further optimizations.

@subsection{Abstracting Away Jumps}
We have already introduced the basic-block abstraction to simplify the structure
of jumps for register allocation, but can simplify further.
Since our source language, @tech{Values-lang v4}, doesn't use jumps at all and
exposes only an @values-lang-v4[if] expression, there is no point (yet) to
exposing jumps further up the pipeline.
If we abstract away from them here and now, then no part of the register
allocator will need to deal with jumps.

To abstract away from jumps, we need to design a feature that is sufficient to
express @values-lang-v4[if] @emph{expressions} in terms of @values-lang-v4[if]
@emph{statements} without jumps.
The key difference between the two can be seen clearly in the
pseudo-grammar-diff below:

@bettergrammar*-diff[
((e (if pred (jump trg_1) (jump trg_2))))
((e (if pred e_1 e_2)))
]

In @values-lang-v4[if] expressions, like other expressions, we support arbitrarily
nested sub-expressions in the branches.
In @values-lang-v4[if] statements, we restrict the branches.

To compile @values-lang-v4[if] expressions to @values-lang-v4[if] statements, we
must generate new basic blocks with fresh labels from nested branches, and
transform the branches into jumps.
Phrased top-down, the question is if we should do that before or after register
allocation?
Phrased bottom-up, should we expose jumps through the register allocation
languages or not?

As discussed earlier when describing the semantics of labels and jumps, jumps
are difficult to give semantics to.
We want to avoid analyzing them if we can.
We therefore choose to @emph{not} expose jumps any further up the pipeline.

@todo{Rewrite this. The explanation focuses on jumps, which is a bit of a
red-herring. The real issue is that each new block }
@;{
Before we start modifying the register allocator, we will make a simplifying
assumption: we should allow nested @values-lang-v4[tail]s.
This means nested @values-lang-v4[if] statements, and @values-lang-v4[begin]s in the
branches of an @values-lang-v4[if] statement.

It is not obvious why we would want to make this change, and this is one place
where our back-to-front design fails us.
In truth, compiler design, like most software design, is an interative process.
We must look ahead a few steps in the compiler to see the problem.

If we were following our nose and propagating our new features up to the next
abstraction layer, the next step might be to simply start on the register
allocator, extending it with support for blocks.
As soon as we do that, we realize that analyzing arbitrary jump instructions is
kind of complicated.
However, analyzing @values-lang-v4[if] is somewhat less complicated, as the
structure of the control flow is more constrained.
By allowing @values-lang-v4[tail]s to be nested, we can allow more
@values-lang-v4[if]s and fewer apparent jumps in the source language of the
analyses.
The fewer jumps, the better job the analysis can do, and the better code it can
produce.
So we want to minimize jumps in the language that we analyze.

@question{
Starting from here, proceed bottom-up and try to expose @tt{jmp} through each
level of abstraction.
Predict at we would need to do in order to expose @tt{jmp} through the
compiler, and why.
}
}

We introduce @deftech{Nested-asm-lang v4}, which allows nesting
@nested-asm-lang-v4[begin] and @nested-asm-lang-v4[if] expressions that would
otherwise need to be expressed with labeled blocks and jumps.
This means we could have an @nested-asm-lang-v4[if] statement of the form
@;
@nested-asm-lang-v4[(if pred (begin s ... (halt loc)) (begin s ... (halt loc)))].
@;
The nesting structure allows all compiler passes over this language, and
targeting this language, to ignore jumps.
The language roughly corresponds to an imperative programming language without
loops, but one assembly-like feature still remains: @ch2-tech{physical locations}.
@todo{Need to stop with "earlier" and "later". "higher" and "lower"?}

@bettergrammar*-ndiff[
#:labels ("Diff" "Nested-asm-lang v4" "Block-pred-lang v4")
(#:exclude (loc reg binop relop int64 aloc fvar) block-pred-lang-v4 nested-asm-lang-v4)
(nested-asm-lang-v4)
(block-pred-lang-v4)
]

Note that @tech{Nested-asm-lang v4} enables much of the same nesting we find in
@ch3-tech{monadic form}.
We skipped over @ch3-tech{a-normal form}.
Unnesting @nested-asm-lang-v4[if] requires jumps, unless we want to duplicate
code; for efficiency and simplicity, it is beneficial to maintain
@ch3-tech{monadic form} until this very low level in the compiler.
@todo{elaborate}

To implement @tech{Nested-asm-lang v4}, we define the procedure
@racket[expose-basic-blocks].
The strategy for writing this is slightly complex.
Each helper for processing a non-terminal may need to introduce new basic blocks,
and transforming a nested @nested-asm-lang-v4[if] expression requires knowing
the target of each branch.

The transformation for predicates should transform predicates and generate an
@nested-asm-lang-v4[if] statement whose branches are jumps.
When processing a @nested-asm-lang-v4[pred], we need two additional
inputs, a "true" and a "false" label, used to generate the output
@nested-asm-lang-v4[if] instruction.
For a base predicate, such as @nested-asm-lang-v4[(true)] or
@nested-asm-lang-v4[(relop aloc triv)], you can generate an @nested-asm-lang-v4[if]
statement.
When you find an @nested-asm-lang-v4[if] in predicate position, you'll need to
generate two new basic blocks, and rearrange the current true and false labels.

The transformer for effects should take care to unnest @nested-asm-lang-v4[begin]
statements.
This is not really related to exposing basic blocks, but it is trivial to deal
with using the right abstraction, and so does not warrant a separate compiler
pass.
@todo{The same is true of predicates. Why then do we have resolve-predicates?...
for future optimization potential?}

@nested[#:style 'inset
@defproc[(expose-basic-blocks (p nested-asm-lang-v4?))
          block-pred-lang-v4?]{
Compile the @tech{Nested-asm-lang v4} to @tech{Block-pred-lang v4}, eliminating
all nested expressions by generating fresh basic blocks and jumps.
}
]

We can now express various optimizations in @tech{Nested-asm-lang v4}.
For example, we can express the following rewrites:
@tabular[
#:sep @hspace[3]
#:style 'boxed
#:column-properties  '(right center left)
#:row-properties  '(bottom-border ())
(list
 (list @bold{Source} "⇒" @bold{Target})
 (list
  @nested-asm-lang-v4[(begin (set! reg 1) (> reg 0))] "⇒"

  (let-syntax ([set! datum-literal-transformer]
               [begin datum-literal-transformer]
               [true datum-literal-transformer]
               [reg (make-variable-id 'reg)])
    @racketblock0[(begin (set! reg 1)
                         (true))]))
 (list
  @nested-asm-lang-v4[(begin (set! reg 1) (< reg 0))]
  "⇒"
  (let-syntax ([set! datum-literal-transformer]
               [begin datum-literal-transformer]
               [false datum-literal-transformer]
               [reg (make-variable-id 'reg)])
    @racketblock0[(begin (set! reg 1)
                         (false))]))
 (list
  (let-syntax ([set! datum-literal-transformer]
               [begin datum-literal-transformer]
               [< datum-literal-transformer]
               [reg (make-variable-id 'reg)]
               [opand_1 (make-variable-id 'opand_1)])
    @racketblock0[(begin (set! reg opand_1)
                         (< reg ,(max-int 64)))])
  "⇒"
  (let-syntax ([set! datum-literal-transformer]
               [begin datum-literal-transformer]
               [true datum-literal-transformer]
               [reg (make-variable-id 'reg)]
               [opand_1 (make-variable-id 'opand_1)])
    @racketblock0[(begin (set! reg opand_1)
                         (true))]))
 (list
  (let-syntax ([set! datum-literal-transformer]
               [begin datum-literal-transformer]
               [= datum-literal-transformer]
               [reg (make-variable-id 'reg)]
               [opand_1 (make-variable-id 'opand_1)])
    @racketblock0[(begin (set! reg opand_1)
                         (= reg opand_1))])
  "⇒"
  (let-syntax ([set! datum-literal-transformer]
               [begin datum-literal-transformer]
               [true datum-literal-transformer]
               [reg (make-variable-id 'reg)]
               [opand_1 (make-variable-id 'opand_1)])
    @racketblock0[(begin (set! reg opand_1)
                         (true))]))
 (list
  (let-syntax ([set! datum-literal-transformer]
               [begin datum-literal-transformer]
               [= datum-literal-transformer]
               [reg (make-variable-id 'reg)]
               [int64_1 (make-variable-id 'int64_1)]
               [int64_2 (make-variable-id 'int64_2)])
    @racketblock0[(begin (set! reg int64_1) (= reg int64_2))])
  "⇒" (let-syntax ([false datum-literal-transformer]
                   [begin datum-literal-transformer]
                   [set! datum-literal-transformer]
                   [reg (make-variable-id 'reg)]
                   [int64_1 (make-variable-id 'int64_1)])
        @racketblock0[(begin (set! reg int64_1) (false))])))
]

The language doesn't allow us to express relational opreations directly on
@nested-asm-lang-v4[opand]s, so we have to be a little more clever to record the
possible values of @ch2-tech{abstract locations}, and detect @nested-asm-lang-v4[(> loc
0)], when @nested-asm-lang-v4[loc] is surely greater than 0.

More generally, we might define an @deftech{abstract interpreter}.
This interpreter would run during compile-time, and thus over possibly
incomplete programs.
This means it has to define some abstract notion of the value of a statement.
In the worst case, such an abstract value will represent "any run-time value",
meaning that we don't have enough static information to predict the result.
However, we might be able to evaluate a predicate to determine that in
@nested-asm-lang-v4[(begin (set! fv0 5) (> fv0 5))], @nested-asm-lang-v4[fv0] is
surely 5, and that @nested-asm-lang-v4[(not (true))] is surely
@nested-asm-lang-v4[(false)] in the abstract interpreter, and if so, this
justifies optimizations.

Note that when rewriting predicates, we must be careful to preserve any effects,
since we can't (locally) know whether they're necessary or not.

@question{Can you think of any predicates that require using nested @nested-asm-lang-v4[if]
statements?}
@todo{Can i?}

@nested[#:style 'inset
@defproc[(optimize-predicates (p nested-asm-lang-v4?))
         nested-asm-lang-v4?]{
Optimize @tech{Nested-asm-lang v4} programs by analyzing and simplifying
predicates.
}
]

@section{Register Allocation}
Next, we design @deftech{Asm-pred-lang v4}, an imperative language that supports
some nested structured control-flow.
Like @ch2-tech{Asm-lang v2}, this language is a family of
@ch2-tech{administrative languages}, each differing only in its info fields.

@bettergrammar*-ndiff[
#:labels ("Diff w/ Nested-asm-lang v4" "Diff w/ Asm-lang v2" "Asm-pred-lang v4" "Nested-asm-lang v4")
(#:exclude (binop relop) nested-asm-lang-v4 asm-pred-lang-v4)
(asm-lang-v2 asm-pred-lang-v4)
(asm-pred-lang-v4)
(nested-asm-lang-v4)
]

The big difference is that @ch2-tech{physical locations} have changed to
@ch2-tech{abstract locations}.
Recall that this is the big abstraction register allocation buys us, so it ought
to be the only big change.

As before, we treat the register allocator as a single compiler from
@tech{Asm-pred-lang v4} to @tech{Nested-asm-lang v4}.

@nested[#:style 'inset
@defproc[(assign-homes-opt [p asm-pred-lang-v4?])
         nested-asm-lang-v4?]{
Compiles @tech{Asm-pred-lang v4} to @tech{Nested-asm-lang v4} by replacing all
@ch2-tech{abstract locations} with @ch2-tech{physical locations}.
}
]

Recall that in our register allocator, we are not designing layers of
abstraction like most of our compiler.
We are following an existing design: undead analysis, conflict analysis, graph
colouring register allocation.
We therefore walk through the register allocator in this order.

@subsection{Uncovering Locals}
First, we extend @racket[uncover-locals] to analyze @asm-pred-lang-v4[if].
We design the administrative language @deftech{Asm-pred-lang v4/locals} below.
As with other administrative languages, the only change is the
@asm-pred-lang-v4[info] field for the module.
It now contains a @ch2-tech{locals set}, describing all variables used in the
module.

@bettergrammar*-diff[
#:include (info) asm-pred-lang-v4 asm-pred-lang-v4/locals
]

@nested[#:style 'inset
@defproc[(uncover-locals (p asm-pred-lang-v4?))
         asm-pred-lang-v4/locals?]{
Compiles @tech{Asm-pred-lang v4} to @tech{Asm-pred-lang v4/locals}, analysing which
@ch2-tech{abstract locations} are used in the module and decorating the module with
the set of variables in an @asm-pred-lang-v4[info] field.
}
]

@subsection{Undead Analysis}
Now our undead analysis must change to follow the branches of @asm-pred-lang-v4[if]
statements.
@deftech{Asm-pred-lang v4/undead} defines the output of @racket[undead-analysis].

@bettergrammar*-diff[#:include (info) asm-pred-lang-v4/locals asm-pred-lang-v4/undead]

@todo{mention that basic blocks are not allowed to have cycles and therefore, we don't need to compute a fixpoint?}

The key to describing the analysis is designing a representation of
@tech{undead-set tree} that can representing the new structure of our
statements.
Now, statements can branch at @asm-pred-lang-v4[if].

@;@racketblock[
@;(define (undead-set? x)
@;  (and (list? x)
@;       (andmap aloc? x)
@;       (= (set-count (list->set x)) (length x))))
@;
@;(define (undead-set-tree? ust)
@;  (match ust
@;    [(? undead-set?) #t]
@;    [(list (? undead-set?) (? undead-set-tree?) (? undead-set-tree?)) #t]
@;    [`(,(? undead-set-tree?) ... (? undead-set-tree?)) #t]
@;    [else #f]))
@;]

Our new @deftech{undead-set tree} is one of:
@itemlist[
@item{an @ch-ra-tech{undead-out set} @asm-pred-lang-v4[(aloc ...)], corresponding to the
@ch-ra-tech{undead-out set} for a single instruction such as @asm-lang-v2[(halt triv)]
or @asm-pred-lang-v4[(set! aloc triv)].}
@item{a list of @tech{undead-set tree}s, @asm-pred-lang-v4[(undead-set-tree?_1
... undead-set-tree?_2)], corresponding to a @asm-pred-lang-v4[begin] statement
@asm-pred-lang-v4[(begin effect_1 ... effect_2)] or @asm-pred-lang-v4[(begin
effect_1 ... tail)].
The first element of the list represents @tech{undead-set tree} for the first
@asm-pred-lang-v4[effect], the second element represents the @tech{undead-set tree}
for the second @asm-pred-lang-v4[effect], and so on.
}
@item{a list of exactly three @tech{undead-set tree}s,
@asm-pred-lang-v4[(undead-set-tree?_p undead-set-tree?_1 undead-set-tree?_2)],
corresponding to a @asm-pred-lang-v4[if] statement @asm-pred-lang-v4[(if pred
tail_1 tail_2)].
@asm-pred-lang-v4[undead-set-tree?_p] corresponds to the @tech{undead-set tree}
of @asm-pred-lang-v4[pred], while @asm-pred-lang-v4[undead-set-tree?_1]
corresponds to @asm-pred-lang-v4[tail_1] and
@asm-pred-lang-v4[undead-set-tree?_2] corresponds to @asm-pred-lang-v4[tail_2].
}
]

@emph{Warning:} this datatype is non-canonical. The second and third cases can
overlap.
We always traverse an @tech{undead-set tree} together with a corresponding
program; the @tech{undead-set tree} cannot be interpreted in isolation.

@todo{Changed this example to remove jump. Should also just compute the example instead of manually doing it.}
For example, consider the following @tech{undead-set tree}.
@(require racket/pretty)
@(pretty-format
 (info-ref
  (cadr
   (undead-analysis
    `(module
       ((locals (x.1 y.2 b.3 c.4)))
       (begin
         (set! x.1 5)
         (set! y.2 x.1)
         (begin
           (set! b.3 x.1)
           (set! b.3 (+ b.3 y.2))
           (set! c.4 b.3)
           (if (= c.4 b.3)
               (halt c.4)
               (begin
                 (set! x.1 c.4)
                 (halt c.4))))))))
  'undead-out) #:mode 'write)

This corresponds to the following @asm-pred-lang-v4[tail].
@(let-syntax ([set! datum-literal-transformer]
              [begin datum-literal-transformer]
              [module datum-literal-transformer]
              [locals datum-literal-transformer]
              [= datum-literal-transformer]
              [+ datum-literal-transformer]
              [if datum-literal-transformer]
              [halt datum-literal-transformer]
              [x.1 datum-literal-transformer]
              [x.2 datum-literal-transformer]
              [b.3 datum-literal-transformer]
              [c.4 datum-literal-transformer])
   @racketblock[
   (module
     ((locals (x.1 y.2 b.3 c.4)))
     (begin
       (set! x.1 5)
       (set! y.2 x.1)
       (begin
         (set! b.3 x.1)
         (set! b.3 (+ b.3 y.2))
         (set! c.4 b.3)
         (if (= c.4 b.3)
             (halt c.4)
             (begin
               (set! x.1 c.4)
               (halt c.4))))))
   ])

@nested[#:style 'inset
@defproc[(undead-analysis (p asm-pred-lang-v4/locals?))
         asm-pred-lang-v4/undead?]{
Performs undeadness analysis, decorating the program with
@tech{undead-set-tree}.
Only the info field of the program is modified.
}
]

@subsection{Conflict Analysis}
Next we need to compute the conflict graph.

Below, we design @deftech{Asm-pred-lang v4/conflicts} below with structured
control-flow.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v4/undead asm-pred-lang-v4/conflicts]

The @racket[conflict-analysis] does not change significantly.
We simply extend the algorithm to support the new statements.
Note that new statements only reference but never define an @ch2-tech{abstract
location}.

@nested[#:style 'inset
@defproc[(conflict-analysis (p asm-pred-lang-v4/undead?))
         asm-pred-lang-v4/conflicts?]{
Decorates a program with its @ch-ra-tech{conflict graph}.
}
]

@subsection{Assign Registers}
Finally, we design the register allocator as the administrative
language @deftech{Asm-pred-lang v4/assignments} (pronounced "Asm-pred-lang v4, with
assignments").

@bettergrammar*-diff[#:include (info) asm-pred-lang-v4/conflicts asm-pred-lang-v4/assignments]

The allocator should run the same algorithm as before.
Since the allocator doesn't traverse programs, it shouldn't need any changes.

@nested[#:style 'inset
@defproc[(assign-registers (p asm-pred-lang-v4/conflicts?))
         asm-pred-lang-v4/assignments?]{
Performs @ch-ra-tech{graph-colouring register allocation}, compiling @tech{Asm-pred-lang
v4/conflicts} to @tech{Asm-pred-lang v4/assignments} by decorating programs with
their register assignments.
}
]

@subsection{Replace Locations}
Finally, we actually replace @ch2-tech{abstract locations} with
@ch2-tech{physical locations}.
In the process, we're free to discard the info from the analyses.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v4/assignments asm-pred-lang-v4]

@nested[#:style 'inset
@defproc[(replace-locations [p asm-pred-lang-v4/assignments?])
         nested-asm-lang-v4?]{
Compiles @tech{Asm-pred-lang v4/assignments} to @tech{Nested-asm-lang v4} by replacing all
@ch2-tech{abstract location} with @ch2-tech{physical locations} using the assignments
described in the @asm-pred-lang-v4[assignment] info field.
}
]

@section{Imperative Abstractions}
Finally, we have all the abstractions in place to abstract away from imperative
statements.

First we abstract to an imperative language from our abstract assembly language.
We design @deftech{Imp-cmf-lang v4}, a pseudo-@ch3-tech{ANF} restricted imperative
language.

@bettergrammar*-ndiff[
#:labels ("Diff vs Target" "Diff vs v3" "Imp-cmf-lang v4" "Asm-pred-lang v4")
(#:exclude (triv binop relop aloc int64) asm-pred-lang-v4 imp-cmf-lang-v4)
(imp-cmf-lang-v3 imp-cmf-lang-v4)
(imp-cmf-lang-v4)
(asm-pred-lang-v4)
]

It is mostly a straightforward extension of @ch3-tech{Imp-cmf-lang v3} to
include the @imp-cmf-lang-v4[if] and @imp-cmf-lang-v4[pred].
However, note that it breaks @ch3-tech{ANF} by allowing nested
@imp-cmf-lang-v4[effect]s in @imp-cmf-lang-v4[pred] position.
This means the language is even more non-canonical.
The following two programs are equal:

@imp-cmf-lang-v4[(if (begin (set! x.1 5) (= x.1 5)) x.1 6)]

@imp-cmf-lang-v4[(begin (set! x.1 5) (if (= x.1 5) x.1 6))]

@todo{Seems like we could easily deal with that one.}

@nested[#:style 'inset
@defproc[(select-instructions (p imp-cmf-lang-v4?))
         asm-pred-lang-v4?]{
Compiles @tech{Imp-cmf-lang v4} to @tech{Asm-pred-lang v4}, selecting
appropriate sequences of abstract assembly instructions to implement the
operations of the source language.
}
]

Similarly, we easily extend @ch3-tech{Imp-mf-lang v3} to @deftech{Imp-mf-lang v4},
defined below.

@bettergrammar*-ndiff[
#:labels ("Diff vs Target" "Diff vs v3" "Imp-mf-lang v4")
(#:exclude (relop binop) imp-cmf-lang-v4 imp-mf-lang-v4)
(imp-mf-lang-v3 imp-mf-lang-v4)
(imp-mf-lang-v4)
]

@nested[#:style 'inset
@defproc[(normalize-bind (p imp-mf-lang-v4?))
         imp-cmf-lang-v4?]{
Compiles @tech{Imp-mf-lang v4} to @tech{Imp-cmf-lang v4}, pushing
@imp-mf-lang-v3[set!] under @imp-mf-lang-v4[begin] and @imp-mf-lang-v4[if] so
that the right-hand-side of each @imp-mf-lang-v3[set!] is a simple value-producing
operation.

This normalizes @tech{Imp-mf-lang v4} with respect to the equations
@tabular[
#:sep @hspace[3]
#:column-properties '(left center right)
(list
 (list
  @;imp-mf-lang-v4[(set! aloc (begin effect_1 ... value))]
  @(let-syntax ([set! datum-literal-transformer]
                [begin datum-literal-transformer]
                [... datum-literal-transformer]
                [aloc (make-variable-id 'aloc)]
                [effect_1 (make-variable-id 'effect_1)]
                [value (make-variable-id 'value)])
     @racketblock0[(set! aloc
                         (begin effect_1 ...
                                value))])
  "="
  @(let-syntax ([set! datum-literal-transformer]
                [begin datum-literal-transformer]
                [... datum-literal-transformer]
                [aloc (make-variable-id 'aloc)]
                [effect_1 (make-variable-id 'effect_1)]
                [value (make-variable-id 'value)])
     @racketblock0[(begin effect_1 ...
                          (set! aloc value))])
  @;imp-mf-lang-v4[(begin effect_1 ... (set! aloc value))]
  )
 (list
  @(let-syntax ([set! datum-literal-transformer]
                [if datum-literal-transformer]
                [aloc (make-variable-id 'aloc)]
                [pred (make-variable-id 'pred)]
                [value_1 (make-variable-id 'value_1)]
                [value_2 (make-variable-id 'value_2)])
     @racketblock0[(set! aloc
                         (if pred
                             value_1
                             value_2))])
  @;imp-mf-lang-v4[(set! aloc (if pred value_1 value_2))]
  "="
  @;imp-mf-lang-v4[(if pred (set! aloc value_1) (set! aloc value_2))]
  @(let-syntax ([set! datum-literal-transformer]
                [if datum-literal-transformer]
                [aloc (make-variable-id 'aloc)]
                [pred (make-variable-id 'pred)]
                [value_1 (make-variable-id 'value_1)]
                [value_2 (make-variable-id 'value_2)])
     @racketblock0[(if pred
                       (set! aloc value_1)
                       (set! aloc value_2))])
  ))
]
}
]

We describe @deftech{Values-unique-lang v4} below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v3" "Diff vs Target" "Values-unique-lang v4")
(values-unique-lang-v3 values-unique-lang-v4)
(imp-mf-lang-v4 values-unique-lang-v4)
(values-unique-lang-v4)
]

In @tech{Values-unique-lang v4}, we extend expressions with an
@values-unique-lang-v4[if] expression that takes a predicate.
The predicate form, or sub-language, is still not a true boolean datatype.
They cannot be bound to @ch2-tech{abstract locations} or returned as values.
We have to restrict the predicate position this way since we have no explicit
run-time representation of the value that a comparison operation produces, @ie
we don't have booleans.

@nested[#:style 'inset
@defproc[(sequentialize-let (p values-unique-lang-v4?))
         imp-mf-lang-v4?]{
Compiles @tech{Values-unique-lang v4} to @tech{Imp-mf-lang v4} by picking a
particular order to implement @values-unique-lang-v4[let] expressions using
@imp-mf-lang-v4[set!].
}
]

Finally, we abstract away from @ch2-tech{abstract locations} and introduce
@ch3-tech{lexical identifiers}.

We defined @deftech{Values-lang v4} below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v3" "Diff vs Target" "Values-lang v3")
(values-lang-v3 values-lang-v4)
(values-unique-lang-v4 values-lang-v4)
(values-lang-v4)
]

@nested[#:style 'inset
@defproc[(uniquify (p values-lang-v4?))
          values-unique-lang-v4?]{
Compiles @tech{Values-lang v4} to @tech{Values-unique-lang v4} by resolving
@ch3-tech{lexical identifiers} into unique @ch2-tech{abstract locations}.
}
]

@section{Polishing Version 4}
@todo{Anything I want to say here? check-values-lang? interpreter?}

@nested[#:style 'inset
@defproc[(interp-values-lang (p values-lang-v4?))
         int64?]{
Interpret the @tech{Values-lang v4} program @racket[p] as a value.
For all @racket[p], the value of @racket[(interp-values-lang p)] should equal
to @racket[(execute p)].
}
]

@nested[#:style 'inset
@defproc[(check-values-lang (p any/c))
         values-lang-v4?]{
Takes an arbitrary value and either returns it, if it is a valid
@tech{Values-lang v4} program, or raises an error with a descriptive
error message.
}
]

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v4-graph" "Overview of Compiler Version 4" v4-graph]
