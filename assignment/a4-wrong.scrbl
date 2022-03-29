#lang reader "assignment-lang.rkt"

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@(define eg
   (make-cached-eval
    "a4-wrong-eval"
    '(require cpsc411/v1-reference/a4-franken-solution cpsc411/deprecated/a4-compiler-lib)
    '(current-stack-size 512)))

@title[#:tag "top" #:tag-prefix "a4:wrong"]{Compiler 4: Adding Control Flow (Original)}

@section{Assignment Summary}

The goals of this assignment are to (1) practice exposing low-level features
from the target language to the compiler, (2) introduce designing abstractions
to tame low-level operations, (3) demonstrate the problems that control flow
causes with program analysis, (4) introduce linking, and (5) practice thinking
about programming at different levels of abstraction.  We'll expose labels and
@object-code{jmp} instructions from @a0-tech{x64}, provide abstractions for
using them in the source language, compile the latter abstraction to the former
implementation, and practice reasoning about our new source language.

This assignment is due Friday Febuary 14, 2020 at 11:59pm; you have 2 weeks
this time.

By Friday Febuary 7, 2020 at 11:59pm, you should have tests for all exercises,
should have finished implementing 1/3--1/2 of the exercises.  We will provide
interim feedback by running any tests you've pushed to your repository against
the reference solution at this time.

@subsubsub*section{Assignment Checklist}

You should find a new repository in your
@url{https://github.students.cs.ubc.ca} account named @tt{a4_<team ids>} with a
code skeleton and a support library.  You should complete the assignment in
that git repository and push it to the GitHub Students instance.

You should first copy your solution to @secref[#:tag-prefixes '("a3:")]{top}
into the starter code provided.

If we patch the code skeleton or library after the release, the most recent
version will be available here at @share{a4-skeleton.rkt} and
@share{a4-compiler-lib.rkt}, and the functional graph library
@share{a4-graph-lib.rkt}.  The name of the skeleton is @share{a4-skeleton.rkt}
to avoid accidentally overwriting your files, but your file in the Git
repository should be named @tt{a4.rkt}.

Per @secref[#:tag-prefixes '("style:")]{top}, you are not required to keep all
of your code in a single file, but @tt{a4.rkt} must export the at least the
names exported by @tt{a4-skeleton.rkt}, and you must follow the instructions on
organizing your code from @secref[#:tag-prefixes '("style:")]{top}.

Regardless of how your code is organized, to simplify grading, you must add a
comment with the exercise number before the code for each of your exercises:
@;
@racketblock[
(code:comment "Exercise 20")
(define (values-lang-fib n) ...)

(code:comment "Exercise 21")
(code:comment "When I compared the two implementations I found ...")
]

For non-obvious test cases, add a comment explaining which language feature or
edge case you are testing.  For trivial tests, such as
@;
@racket[(check-exn exn:fail? (thunk (check-values-lang '())))],
this is not necessary and will only make your code less readable.

@;todo{These should generated in a way that checks the input/output languages.}
@;todo{Use the "redesign and extend the implementation of" phrase more consistently}
@itemlist[
@item{Redesign and extend the implementation of @racket[generate-x64], a
compiler from @tech{Paren-x64 v3} to @a0-tech{x64}.}
@item{Design and implement @racket[link-paren-x64], a linker for the
@tech{Paren-x64 v3} (@emph{equivalently}, a compiler from @tech{Paren-x64 v3} to
@tech{Paren-x64-rt}).}
@item{Redesign and extend the implementation of @racket[interp-paren-x64], the
interpreter for @tech{Paren-x64 v3}}
@item{Redesign and extend the implementation of @racket[patch-instructions], a
compiler from @tech{Paren-asm v2} to @tech{Paren-x64 v3}.}
@item{Design and implement @racket[flatten-program], a compiler from
@tech{Block-asm-lang} to @tech{Paren-asm v2}.}
@item{Design and implement @racket[expose-basic-blocks], a compiler from
@tech{Block-nested-lang} to @tech{Block-asm-lang}.}
@item{Redesign and extend the implementation of @racket[replace-locations], a
compiler from @tech{Block-assigned-lang} to @tech{Block-nested-lang}.}
@item{Design and implement @racket[discard-call-live], a compiler from
@tech{Block-jump-live-lang} to @tech{Block-assigned-lang}.}
@item{Redesign and extend the implementation of @racket[assign-registers], a
compiler from @tech{Conflict-lang v2} to @tech{Block-jump-live-lang}.}
@item{Redesign and extend the implementation of @racket[conflict-analysis], a
compiler from @tech{Undead-block-lang} to @tech{Conflict-lang v2}.}
@item{Redesign and extend the implementation of @racket[undead-analysis], a
compiler from @tech{Block-locals-lang} to @tech{Undead-block-lang}.}
@item{Redesign and extend the implementation of @racket[uncover-locals], a
compiler from @tech{Block-lang} to @tech{Block-locals-lang}.}
@item{Redesign and extend the implementation of @racket[select-instructions], a
compiler from @tech{Values-unique-lang v2} to @tech{Block-lang}.}
@item{Redesign and extend the implementation of @racket[uniquify], a compiler
from @tech{Values-lang v2} to @tech{Values-unique-lang v2}.}
]


@section{Language Diagram}

@dot->svg{
digraph {

node [ shape="box" ]

/* The Languages */

L0 [label="Values-lang v2"];
L1 [label="Values-unique-lang v2"];
L2 [label="Block-lang"];
L3 [label="Block-locals-lang"];
L4 [label="Undead-block-lang"];
L5 [label="Conflict-lang v2"];
L6 [label="Block-jump-live-lang"];
L7 [label="Block-assigned-lang"];
L8 [label="Block-nested-lang"];
L9 [label="Block-asm-lang"];
L10 [label="Paren-asm v2"];
L11 [label="Paren-x64 v3"];
L12 [label="Paren-x64 rt"];
L13 [label="x64"];


/* The Passes */

edge [fontname="Courier"]

L0 -> L1 [label=" uniquify"];
L1 -> L2 [label=" select-instructions"];
L2 -> L3 [label=" uncover-locals"];
L3 -> L4 [label=" undead-analysis"];
L4 -> L5 [label=" conflict-analysis"];
L5 -> L6 [label=" assign-registers"];
L6 -> L7 [label=" discard-call-live"];
L7 -> L8 [label=" replace-locations"];
L8 -> L9 [label=" expose-basic-blocks"];
L9 -> L10 [label=" flatten-program"];
L10 -> L11 [label=" patch-instructions"];
L11 -> L12 [label=" link-paren-x64"];
L11 -> L13 [label=" generate-x64"];

L11 -> "integer" [label=" interp-paren-x64"];
}
}

@section{Related Reading}

You can find additional reading material on register allocation and undead
analysis at the resources below.  These comes from other courses that use a
compiler that is similar to, but different from, your compiler.  Their
intermediate languages are different, and their compiler pipeline is different.
There is no one universal truth of how these passes work for all compilers in
all situations, so you will need to generalize from these lessons and apply
them to your compiler.

Still, the lessons should be helpful if used with care:
@itemlist[
@item{R. Kent Dybvig's notes on register allocation
@url{kents-notes-on-register-alloc.html}.
This language allows mixing abstract locations and real locations in the same
language, unlike your intermediate languages.

These notes are courtesy of R. Kent Dybvig, author of the Chez Scheme compiler
@url{https://github.com/cisco/ChezScheme}.
}
@item{@emph{Essentials of Compilation}, Chapters 3.2, 3.3, and 3.4
@share{book.pdf}.
These chapters include notes on calling conventions and function call/return,
which your compiler should not support yet.

This is a freely available book written by the group at Indiana University:
@url{https://github.com/IUCompilerCourse/Essentials-of-Compilation}}
]



@section{Preface: What's wrong with Values-lang}

The language @a2-tech{Values-lang} has a significant limitation: we can only
expression simple, straight-line arithmetic computations.  We'll never be able
to write any interesting programs!

In this assignment, we will expose a machine feature, control flow instructions
in the form of labels and @object-code{jmp} instructions, and systematically
abstract these.  This requires changes to nearly every pass and every
intermediate language.

We'll proceed bottom-up, but first let's take a look at our goal.  We want to
extend @a2-tech{Values-lang} with control-flow features: a form of
@object-code{if} expression, top-level function definitions, and non-returning
function calls (tail calls).

@racketgrammar*[
(p (module b ... e))
(b (define x (lambda (x ...) e)))
(e tail (let ([x n]) e) (if (cmp v v) e e))
(n v (binop v v))
(tail n (apply x v ...))
(x name)
(v int64 x)
[binop * +]
[cmp neq? eq? < <= > >=]
]

@;todo{Would be nice to typeset changes in BNF from previous assignment}

Note that an @object-code{if} expression, @object-code{(if (cmp v v) e e)},
cannot branch on an arbitrary expression.  It is restricted so that a
comparison between two values must appear in the predicate position.  This is a
necessary artifact inherited from @a0-tech{x64}.  Even by the end of this
assignment, we will not yet have enough support from the low-level language to
add values that represent the result of a comparison, @emph{i.e.}, we won't
have booleans.

Note also that a function can only be called as the very last expression.  This
is because we will not have introduced enough to implement @emph{returning}
from a function, so all function calls are restricted to only jump
@emph{forward}.

Despite the limitations of @tech{Values-lang v2}, the language is finally
expressive enough to write interesting programs.  We'll therefore implement a
type checking tool to help us program in @tech{Values-lang v2}, and spend some
time writing programs to understand its expressiveness and its limitations.  If
you prefer, you can jump to the last few exercises and complete those first.

@section{Exposing Control-Flow Primitives}

We'll start by designing a new @deftech{Paren-x64 v3} to expose the additional
features of @a0-tech{x64} necessary to implement control flow abstractions.
This extends the previous @a2-tech{Paren-x64 v2} with comparison operations,
labels, and conditional and unconditional jump operations.

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
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (rbp - dispoffset) (rbp + dispoffset)]
  [binop * +]
  [cmp   neq? eq? < <= > >=]
]

@racketblock[
  (define (label? s)
    (and (symbol? s)
         (regexp-match-exact? #rx"L\\..+\\.[0-9]+" (symbol->string s))))
]

@;The @object-code{(compare reg triv)} instruction is further restricted by
@;@a0-tech{x64}: @object-code{triv} cannot be a label.

@a0-tech{x64} exposes labels, written @code{l:} before some instruction, and
jumps, written @object-code{jmp trg} where @code{trg} is either a label or a
register containing a label.  Labels can be chained in @a0-tech{x64}, as we've
seen in prior assignments.  For example, the following assigns two labels to
the same instructions:
@;
@verbatim{
L1:
L2:
  mov rax, 42
}

In @tech{Paren-x64 v3}, we model labels with the @object-code{(define label s)}
instruction, which defines a label @object-code{label} at the instruction
@object-code{s} in the instruction sequence.  This corresponds to the
@a0-tech{x64} string @object-code{label:\n s}.  Note that they can be nested,
allowing the same behaviour as chaining labels in @a0-tech{x64}.  For
convenience, we assume all labels are symbols of the form
@object-code{L.<name>.<number>}, and are unique.

The new comparison instruction @object-code{(compare reg opand)} corresponds to
the @a0-tech{x64} instruction @object-code{cmp reg, opand}.  This instruction
compares @object-code{reg} to @object-code{opand} and sets some flags in the
machine describing their relation, such as whether @object-code{reg} is less
than @object-code{opand}, or whether they are equal.  The flags are used by the
next condition jump instruction.

The conditional jump instructions in @a0-tech{x64}, in the same order as the
@object-code{cmp}, are: @object-code{jne trg}, @object-code{je trg},
@object-code{jl trg}, @object-code{jle trg}, @object-code{jg trg}, and
@object-code{jge trg}.  Each corresponds to "jump to @object-code{trg} if the
comparison flag is set to ___".  For example, the instruction @object-code{je
trg} jumps to @object-code{trg} if the comparison flag "equal" is set.

In @tech{Paren-x64 v3}, we abstract the various conditional jump instructions
into a single instruction with multiple flags.  The instruction @object-code{je
l} corresponds to @object-code{(jump-if eq? l)}.  @object-code{jl l} jumps to
@object-code{l} if comparison flag "less than" is set, and corresponds to
@object-code{(jump-if < l)}.  The rest of the instructions follow this pattern.

@exercise{Redesign and extend the implementation of @racket[generate-x64] to
support control-flow primitives.  The source language is @tech{Paren-x64 v3}
and the target language is @a0-tech{x64}.}

Labels and jumps are a small change to the language syntactically, but have a
large effect on the semantics.  You'll notice this, for example, when writing
the interpreter for a language with labels and jumps compared to a language
without them.

We can no longer write the @tech{Paren-x64 v3} interpreter in one simple loop
over the instructions.  Instead, we need some way to resolve labels.  That way,
when running the interpreter, we can easily jump to any expression at any
time---a possibility the language now allows.  This process of resolving labels
is called @deftech{linking}.

We're going to write a simple linker for @tech{Paren-x64 v3} to give you a
rough idea of how the operating system's linker works.  We'll use a low-level
linking implementation that is similar to the operating systems linker: we
first resolve all labels to their address in memory (in our case, their index
in the instruction sequence) and then implement jumps by simply setting a
program counter to the instruction's address.

To do this, we design a new language @deftech{Paren-x64-rt}, which represents
the @emph{r}un-@emph{t}ime language used by the interpreter after linking.

@racketgrammar*[
  [p     (begin s ...)]
  [s     (set! loc triv)
         (set! reg loc)
         (set! reg_1 (binop reg_1 int32))
         (set! reg_1 (binop reg_1 loc))
         (jump trg)
         (compare reg opand)
         (jump-if cmp pc-addr)]
  [trg   reg pc-addr]
  [triv  trg int64]
  [opand int64 reg]
  [loc   reg addr]
  [reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (rbp - dispoffset) (rbp + dispoffset)]
  [binop * +]
  [cmp   neq? eq? < <= > >=]
]

@racketblock[
  (define pc-addr? natural-number/c)
]

We remove the instruction @object-code{(define label s)}, which the linker will
remove, and turn all label values into @object-code{pc-addr}, a representation
of an address recognized by the interpreter.  In our case, since programs are
represented as lists of instructions, a @object-code{pc-addr} is a natural
number representing the position of the instruction in the list.

@exercise{Design and implement @racket[link-paren-x64], which resolves all
labels into the address of the instruction in the instruction sequence.  The
source language is @tech{Paren-x64 v3} and the target language is
@tech{Paren-x64-rt}.}

@exercise{Redesign and extend the implementation of @racket[interp-paren-x64]
to support control-flow primitives.  The source language is @tech{Paren-x64 v3}
and the output is a Racket integer.

It should use @racket[link-paren-x64] to resolve all labels, and should use a
program counter to loop over the instruction sequence instead of folding over
the sequence.  This means some inner helper function accepts
@tech{Paren-x64-rt} programs.

You may want to refer to the code skeleton in @share{a4-skeleton.rkt}.
}

Next, we redesign @deftech{Paren-asm v2}, exposing the new instructions while
abstracting away from the machine constraints about which instructions work on
which physical locations.  Now jumps can target arbitrary locations, and
compare can compare arbitrary locations.  We can also move labels into
locations.

@;todo{Talk about why trg and not just "loc"}

@racketgrammar*[
  [p     (begin s ...)]
  [s     (set! loc triv)
         (set! loc (binop loc opand))
         (define label s)
         (halt opand)
         (jump trg)
         (compare loc opand)
         (jump-if cmp label)]
  [binop * +]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [reg   rsp rbp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (rbp - dispoffset) (rbp + dispoffset)]
  [cmp   neq? eq? < <= > >=]
]

While we're redesigning, we also lift some restrictions to allow our compiler
to "not be stupid".  We generalize @tech{Paren-asm v2} to allow literal values
in some operands.  We allow jumps to take either a location or a label
directly, and allow @object-code{binop}s and @object-code{halt} to take either
a value or a location.  Relaxing this restriction makes @tech{Paren-asm v2}
less uniform, but makes it easier to work with, and allows us to produce better
code.

Forcing ourselves to "be stupid" is a common hazard when designing abstraction
boundaries.  Previously, in our attempt to abstract away from annoying details
of @a0-tech{x64} and make a more uniform language, we actually @emph{forced}
our compiler to produce worse code.  We required all operations in
@a2-tech{Paren-asm} to work uniformly over @emph{locations} @emph{only}, even
though @a0-tech{x64} supports working over values for many instructions.  This
required all compiler passes that target @a2-tech{Paren-asm} to introduce extra
instructions.  While we could "optimize" those instructions away, it's often
better to "not be stupid"---to avoid generating bad code that you have to clean
up, since optimizations are often incomplete.

@;@margin-note{This was @emph{definitely} an intentional lesson plan and not
@;your professor falling into the pitfall of premature abstraction while
@;designing your compiler.}

@; Is that true?
@;If your compiler was well designed, this should require no changes to
@;@racket[patch-instructions].

@exercise{Redesign and extend the implementation of @racket[patch-instructions]
to implement the new instructions jump and compare instructions that don't
exist in @a0-tech{x64} by instruction sequences that are valid in
@a0-tech{x64}.  The source language is @tech{Paren-asm v2} and the target is
@tech{Paren-x64 v3}.

It will be tricky to implement the instruction @object-code{(compare addr
addr)} with only one temporary register.  You can do it in four @tech{Paren-x64
v3} instructions.

You should continue to use @code{rax} as a temporary register.
}

@section{Blocks: Abstracting labeled instructions}

In @tech{Paren-asm v2}, control can jump to any instruction at any time.  This
makes giving semantics to programs, such as writing an interpreter or an
analysis, very difficult.  At any label, we must make assumptions about the
state of the machine, since the state is not affected only by the sequence of
instructions that came before the label in the instruction sequence, but
potentially arbitrary instructions that were executed prior to a jumping to the
label.

To simplify reasoning about programs, we can organizing code into labeled
blocks, assuming that control flow only enters the beginning of a block and
exits the end of a block.  This block abstraction simplifies reasoning about
control flow.  We have more structure on which to hang assumption, and can make
more assumptions about code when writing analyses.  We want to introduce this
abstraction before we have to extend the register allocator, which currently
does the most analysis in our compiler.

We design @deftech{Block-asm-lang}, a block-structured abstract assembly
language in which sequences of statements are organized into blocks, and code
can jump between blocks.  Labels are no longer instructions that can happen
anywhere; instead, each block is labeled.  Jumps cannot appear just anywhere;
instead, they happen only at the end of a block.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  (any ...)]
  [tail  (begin s ... tail)
         (jump trg)
         (if (cmp loc opand) (jump trg) (jump trg))
         (halt opand)]
  [s     (set! loc triv)
         (set! loc (binop loc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [reg   rsp rbp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (rbp - dispoffset) (rbp + dispoffset)]
  [cmp   neq? eq? < <= > >=]
]
@;todo{The names "b" and "tail" are bad. "b" is a definition, while "tail" is a block...}

In @tech{Block-asm-lang}, a program is a non-empty sequence of labeled blocks.
We consider the first block in the sequence to be the start of the program.
@;We also consider blocks as in-order, so if a block does not end in a jump or,
@;then control should transfer to the next block in the sequence.
A @object-code{tail} represents a self-contained block of statements.  Jumps
can only appear at the end of blocks, and jumps only enter the beginning of
blocks.  The @object-code{halt} instruction should only appear at the end of
the final block; it cannot stop control flow, but only indicates that if the
program has ended, the @object-code{opand} is the final value.

Note that now there is an @object-code{info} field for each block.  Many
analyses that previously worked over the entire program must now work over each
block.

@exercise{Design and implement @racket[flatten-program] to flatten all blocks
into straight-line code.  The source language is @tech{Block-asm-lang} and the
target language is @tech{Paren-asm v2}.}

@challenge{
In @tech{Paren-asm v2}, it's unnecessary to have a jump when the target of the
jump is the next instruction.  Design and implement an optimization pass,
called @racket[inline-jump], that eliminates these unnecessary jumps.  The
source language is @tech{Paren-asm v2} and target is @tech{Paren-asm v2}
}

@challenge{
Branches are often thought of as expensive, so real compilers often perform
@emph{trace scheduling}, an optimization that rearranges blocks and takes
advantage of fall-through between blocks to reduce the number of jumps.

Implement a function @racket[trace-schedule] to perform this optimization.  The
source language is @tech{Block-asm-lang} and the target language is
@tech{Block-asm-lang}.

This optimization works in conjunction with the previous optimization.
}

Before we start modifying the register allocator, we will make a simplifying
assumption: we should allow nested @object-code{tail}s.  This means nested
@object-code{if} statements, and @object-code{begin}s in the branches of an
@object-code{if} statement.

It is not obvious why we would want to make this change, and this is one place
where our back-to-front design fails us.  In truth, compiler design, like most
software design, is an interative process.  We must look ahead a few steps in
the compiler to see the problem.

If we were following our nose and propagating our new features up to the next
abstraction layer, the next step might be to simply start on the register
allocator, extending it with support for blocks.  As soon as we do that, we
realize that analyzing arbitrary jump instructions is kind of complicated.
However, analyzing @object-code{if} is somewhat less complicated, as the
structure of the control flow is more constrained.  By allowing
@object-code{tail}s to be nested, we can allow more @object-code{if}s and fewer
apparent jumps in the source language of the analyses.  The fewer jumps, the
better job the analysis can do, and the better code it can produce.  So we want
to minimizes jumps in the language that we analyze.

We introduce @deftech{Block-nested-lang}, which allows nesting some sequences
of expressions that would otherwise require additional blocks and jumps.  In
particular, @object-code{if} and @object-code{begin} can be nested.  This means
we could have an @object-code{if} statement of the form
@;
@object-code{(if (cmp loc loc) (begin s ... (jump loc)) (begin s ... (jump
loc)))}.
@;
Compiling this require introducing a new fresh label.  The nesting structure
allows all earlier compiler passes to ignore jumps that are quite easy to
eliminate.  In general, jumps are difficult to deal with (as we will see), so
avoiding them for as long as possible is generally a good design choice.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  (any ...)]
  [tail  (begin s ... tail)
         (jump trg)
         (if (cmp loc opand) tail tail)
         (halt opand)]
  [s     (set! loc triv)
         (set! loc (binop loc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 loc]
  [trg   label loc]
  [loc   reg addr]
  [reg   rsp rbp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (rbp - dispoffset) (rbp + dispoffset)]
  [cmp   neq? eq? < <= > >=]
]

@exercise{
Design and implement @racket[expose-basic-blocks], which creates new blocks of
any nested @object-code{tail}s and replaces those nested @object-code{tail}s
with jumps.  The source language is @tech{Block-nested-lang} and the target
language is @tech{Block-asm-lang}.

You may want to use @racket[fresh-label] from @share{a4-compiler-lib.rkt}.
}

Next we need to extend the register allocator and analyses to support blocks.
Each block will have its own assignment of variables, and conflicts and undead
sets will be computed per block.

We design @deftech{Block-assigned-lang} below, an extension of
@a2-tech{Loc-assigned-lang} with blocks.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  ((assignment ((aloc rloc) ...)) any ...)]
  [tail  (begin s ... tail)
         (jump trg)
         (if (cmp aloc opand) tail tail)
         (halt opand)]
  [s     (set! aloc triv)
         (set! aloc (binop aloc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 aloc]
  [trg   label aloc]
  [rloc  reg addr]
  [reg   rsp rbp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (rbp - dispoffset) (rbp + dispoffset)]
  [cmp   neq? eq? < <= > >=]
]

@racketblock[
(define (aloc? s)
  (and (symbol? s)
       (not (register? s))
       (not (label? s))
       (regexp-match-exact? #rx".+\\.[0-9]+" (symbol->string s))))
]

Note that we modify the definition of @racket[aloc?] to ensure
@object-code{aloc}s and @object-code{labels} are distinct.

@exercise{
Redesign and extend the implementation of @racket[replace-locations] to support
blocks and the control-flow primitives.  The source language is
@tech{Block-assigned-lang} and the target language is @tech{Block-nested-lang}.
}

Before we can extend our analyses, we need to assume that the program has some
additional structure: that each jump tells us which locations are live across a
jump.  Without this, conflict analysis and undead analysis will have some
trouble analyzing a jump statement.

Below, we design @deftech{Block-jump-live-lang} with a modified jump
instruction to include all locations that the target of the jump might use.
You can think of these as the "arguments" to the block.  However, their only
purpose here it to communicate to the analysis which locations should be
considered undead.

Notice that if we did not allow nested @object-code{tail}s, the branches of
each @code{if} expression would require "arguments".  While we get to
@emph{assume} the arguments are given to us for now, later we must
@emph{produce} these arguments.  Trying to produce arguments for every
@object-code{if} expression would add needless complexity, slow down our
analyses, and make our compiler work harder to optimize jumps.

There are many design choices in a compiler like this.  If we introduce an
abstraction too late, our compiler may suffer.  However, as we saw in the
design of @a2-tech{Paren-asm} vs @tech{Paren-asm v2}, premature abstraction
boundaries can cause us to suffer as well.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  ((assignment ((aloc rloc) ...)) any ...)]
  [tail  (begin s ... tail)
         (jump trg aloc ...)
         (if (cmp aloc opand) tail tail)
         (halt opand)]
  [s     (set! aloc triv)
         (set! aloc (binop aloc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 aloc]
  [trg   label aloc]
  [rloc  reg addr]
  [reg   rsp rbp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [addr  (rbp - dispoffset) (rbp + dispoffset)]
  [cmp   neq? eq? < <= > >=]
]

@exercise{
Design and implement the function @racket[discard-call-live], which throws away
the set of locations live across a jump, @emph{i.e.}, translates each
@racket[(jump trg aloc ...)] into @racket[(jump trg)], discarding the
undeadness annotations.  The source language is @tech{Block-jump-live-lang} and
the target is @tech{Block-assigned-lang}.
}

Next, we must extend the register allocator to support blocks.
As before, the register allocator gets to assume it has information about locals
and conflicts.
We extend that idea to blocks: each block is now annotated with its locals and
its conflicts.
Conceptually, the allocator should now run the same algorithm as before, but on
each block independently.
However, to different blocks may refer to the same abstract location.
Conceptually, an abstract location is just the name of a unique location, so all
references to it must be to the same physical location.
This means the allocator should always assign the same abstract location the
same physical location.

We design @deftech{Conflict-lang v2} below with blocks and control-flow
primitives.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  ((locals (aloc ...))
          (conflicts ((aloc (aloc ...) ...)))
          any ...)]
  [tail  (begin s ... tail)
         (jump trg aloc ...)
         (if (cmp aloc opand) tail tail)
         (halt opand)]
  [s     (set! aloc triv)
         (set! aloc (binop aloc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 aloc]
  [trg   label aloc]
  [cmp   neq? eq? < <= > >=]
]

@exercise{
Redesign and extend the implementation of @racket[assign-registers] to assign
registers for each block.  The source language is @tech{Conflict-lang v2} and
the target language is @tech{Block-jump-live-lang}.

Recall that you may not use the register @object-code{rax}, since it is used to
patch instructions, or @object-code{rbp}, since it contains the frame pointer.
}

We must also extend each analysis to support blocks.

First we extend conflict analysis.  Below we define
@deftech{Undead-block-lang}.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  ((locals (aloc ...))
          (undead ((aloc ...) ...))
          any ...)]
  [tail  (begin s ... tail)
         (jump trg aloc ...)
         (if (cmp aloc opand) tail tail)
         (halt opand)]
  [s     (set! aloc triv)
         (set! aloc (binop aloc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 aloc]
  [trg   label aloc]
  [cmp   neq? eq? < <= > >=]
]


@exercise{
Redesign and extend the implementation of @racket[conflict-analysis] to
decorate each block with a conflict graph.  The source language is
@tech{Undead-block-lang} and the target language is @tech{Conflict-lang v2}.
}

Finally, we extend the undead analysis.  We design @deftech{Block-locals-lang}
below.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label info tail)]
  [info  ((locals (aloc ...)) any ...)]
  [tail  (begin s ... tail)
         (jump trg aloc ...)
         (if (cmp aloc opand) tail tail)
         (halt opand)]
  [s     (set! aloc triv)
         (set! aloc (binop aloc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 aloc]
  [trg   label aloc]
  [cmp   neq? eq? < <= > >=]
]

@exercise{
Redesign and extend the implementation of @racket[undead-analysis] to collect
@a3-tech{undead-in sets} for each block.  The source language is
@tech{Block-locals-lang} and the target is @tech{Undead-block-lang}.  Only the
@object-code{info} field of each block of the output program is modified, with
the addition of the the @a3-tech{undead-in set}.
}

Finally, we extend @a2-tech{Loc-lang} to @deftech{Block-lang}, a
block-structured location-oriented language.

@racketgrammar*[
  [p     (module b ... b)]
  [b     (define label tail)]
  [tail  (begin s ... tail)
         (jump trg aloc ...)
         (if (cmp aloc opand) tail tail)
         (halt opand)]
  [s     (set! aloc triv)
         (set! aloc (binop aloc opand))]
  [binop * +]
  [triv  opand label]
  [opand int64 aloc]
  [trg   label aloc]
  [cmp   neq? eq? < <= > >=]
]

@exercise{
Redesign and extend the implementation of @racket[uncover-locals] to add a list
of all variables used in each block to the info field of the block.  The source
language is @tech{Block-lang} and the target language is
@tech{Block-locals-lang}.
}

@section{Abstracting Low-level Control Flow}

We've successfully exposed low-level branching and jump operations from
@a0-tech{x64} up through @tech{Block-lang}.  Now we need to design
value-oriented versions of @object-code{if} and @object-code{jump}.

In @tech{Values-lang v2}, we extend expressions with an @object-code{if}
expression that takes a comparison between two values.  We have to restrict the
predicate position because we have no explicit representation of the value that
a comparison operation produces, @emph{i.e.}, we don't have booleans.  All the
intermediate languages simply use the comparison instruction, which sets
@a0-tech{x64} flags that we do not yet have access to.  The lower level
languages we've design essentially treat compare-and-branch as an atomic
statement.

We also add an @object-code{apply} expression that can call a declared function
with some arguments.  We must restrict calls to be in @emph{tail position},
@emph{i.e.}, as the final expression in a block, since we have no way to return
from a function call yet.

First we modify @deftech{Values-unique-lang v2} to include the @object-code{if}
and @object-code{apply} expressions.

Notice that we also modify a program syntax to be a list of function
definitions followed by an expression.  The final expression is where code
begins executing.

@racketgrammar*[
  (p     (module b ... e))
  (b     (define label (lambda (aloc ...) e)))
  (e     tail (let ([aloc n]) e) (if (cmp v v) e e))
  (n     v (binop v v))
  (tail  n (apply x v ...))
  (x     label aloc)
  (v     int64 x)
  [binop * +]
  [cmp   neq? eq? < <= > >=]
]

Now that we have more than one kind of value (we have @object-code{int64} and
@object-code{label}), we begin to run into the problem of ill-typed operations.
For example, our run-time system assumes that we halt with an integer, but the
syntax of @tech{Values-unique-lang v2} allows the final expression to be a
@object-code{label}.  Similarly, a @object-code{binop} will only succeed if it
is given two integers, but not if we try to add an integer and a label.

We have two options to make our language make sense: we can add dynamic type
checking, or we can assume the problem never happens because the constraints of
a higher language and its compilation strategy avoid this scenario.
As we're still dealing with quite a low-level language, we choose the latter
option.

We further restrict the language by disallowing the result of any block or
program to be a label.  It is undefined in the target language to try to halt
with a label, so we rule it out here.  This is difficult to enforce
without a type system, and hard to write a reasonable grammar for.  It means
you can assume a label on its own is not a valid @object-code{tail}.  For
example, the following is an invalid @tech{Values-unique-lang v2} program:
@;
@racketblock[
  (module (define L.t.1 (lambda () 5)) L.t.1)
]

Similarly, we assume that labels are never used as arguments to
@object-code{binop}s, @object-code{cmp}s.
They can be used as arguments to @object-code{apply}, though.

Note that you won't be able to write a checker to enforce this property on all
programs, so you must simply assume it.

@exercise{
Redesign and extend the implementation of @racket[select-instructions] to
translate value-oriented operations into location-oriented instruction.  The
source language is @tech{Values-unique-lang v2} and the target language is
@tech{Block-lang}.
}

Finally, we discharge the last assumption: that we know which names refer to
which abstract locations, and thus introduce the last abstraction.  The new
source language, @deftech{Values-lang v2}, is defined below.

@racketgrammar*[
  (p     (module b ... e))
  (b     (define x (lambda (x ...) e)))
  (e     tail (let ([x n]) e) (if (cmp v v) e e))
  (n     v (binop v v))
  (tail  n (apply x v ...))
  (x     name)
  (v     int64 x)
  [binop * +]
  [cmp   neq? eq? < <= > >=]
]

We continue to assume the result of any block or program cannot be a label, and
that labels are not used as argument to @object-code{binop} or
@object-code{cmp}.

You may find this implementation of alpha equivalence for @tech{Values-lang v2}
helpful: @share{a4-alpha-equal.rkt}.  The code is annotated to demonstrate
idiomatic Racket style, and explain feature you may be unfamiliar with.

@exercise{
Redesign and extend the implementation of @racket[uniquify].  The source
language is @tech{Values-lang v2} and the target language is
@tech{Values-unique-lang v2}.
}

@section{Thinking like a compiler}

We now, finally, have a source language in which we can write interesting
programs.  It is not like most programming languages that you're likely to have
written programs in.  It has limitations designed into it because of what we
know how to implement, and implement efficiently.  This will always be the
case, in everything language you ever write, and so it is useful to learn to
think in a new layer of abstraction.

@exercise{
Redesign and implement the function @racket[check-values-lang], which checks
that its input is a valid @tech{Values-lang v2} program, or raises a helpful
error if not.  In this version, you must check that undefined variables are
never used; it is now an error for a programmer to write a free variable in
@tech{Values-lang v2}.
}

@exercise{
Design and implement a type checker for @tech{Values-lang v2} called
@racket[type-check-values-lang].
The input is a syntactically valid @tech{Values-lang v2} program, and the output
is a @tech{Values-lang v2} program.
However, it should perform that following checks and potentially raise an error:
@itemlist[
@item{If @object-code{apply} is called on a @emph{known function}, then there
are exactly as many arguments as declared by the function.  Otherwise, raise a
type error.  A known function is a name that definitely refers to a function.
}
@item{If @object-code{apply} is called on a name that refers to a number, then
raise a type error.}
@item{If a @object-code{binop} is called with a @emph{known function} as either
argument, then raise a type error.}
@item{If a @object-code{cmp} is called with a @emph{known function} as either
argument, then raise a type error.}
]
}

@exercise{
You cannot implement a type checker for @tech{Values-lang v2} that can catch
all type errors possible in the language.  Briefly explain why not, and give
three examples of type errors still possible in @tech{Values-lang v2}.  Explain
what will happen when each of these examples is executed.  Think through the
program; don't just try to compile and run them.  Suggest how @tech{Values-lang
v2} could be modified so it would be possible to catch each of your examples.
Can you imagine any benefits for your compiler if you could guarantee that
these examples were impossible?
}

@exercise{
Design and implement the function @racket[interp-values-lang], an interpreter
for @tech{Values-lang v2}.  It should receive a @tech{Values-lang v2} program
as input and return a Racket @racket[number?].  The interpreter may call
@racket[type-check-values-lang] on its input.
}

@exercise{
Design and implement the function @racket[values-lang-fact], a Racket function
that should take a Racket natural number as input and return a
@tech{Values-lang v2} program.  The result should be a program that calls the
factorial function, implemented in @tech{Values-lang v2}, on the number
provided.

While factorial should be implemented in @tech{Values-lang v2}, since
@tech{Values-lang v2} cannot take input from the user, we simulate input by
generating a new @tech{Values-lang v2} program with the input provided.

Make sure you use the function to test your interpreter against your compiler.
}

@examples[
(eval:alts
 (interp-values-lang
  (values-lang-fact 1))
 1)

(eval:alts
 (interp-values-lang
  (values-lang-fact 5))
 120)

(eval:alts
 (interp-values-lang
  (values-lang-fact 6))
 720)
]

@exercise{
Design and implement the function @racket[values-lang-fib], a Racket function
that should take a Racket natural number as input and return a
@tech{Values-lang v2} program.  The result should be a program that calls the
Fibonacci function, implemented in @tech{Values-lang v2}, on the number
provided.

While the Fibonacci function should be implemented in @tech{Values-lang v2},
since @tech{Values-lang v2} cannot take input from the user, we simulate input
by generating a new @tech{Values-lang v2} program with the input provided.

Make sure you use the function to test your interpreter against and your
compiler.
}

@exercise{
Last week, you were asked to compared two versions of your compiler: one with
@racket[assign-homes] against one with @racket[assign-homes-opt].  Now, you can
express programs with loops and indirection.  Try the experiment again.  What
do you find?
}


@;x64 cannot distinguish between booleans and numbers: it has only bits.  We,
@;therefore, must come up with a way to distinguish them.
@;
@;A common approach to distinguishing data types is to steal a few bits off the
@;end of every machine words to represent a type tag.  For example, we might
@;decide that the last three bits of any machine word represent the type, and
@;if they are @code{101} then it's a number but if they are @code{100} then
@;it's a boolean.
@;
@;Designing a good type tag representation is tricky.  The more bits you can
@;steal, the more data types you can represent directly as register sized
@;values, resulting in fewer memory accesses and more efficient code.  However,
@;the more bits you steal, the smaller the size of the data type you can fit in
@;a single machine word.  For large instances of that data type, such as a
@;number that requires more than a 61 bits to represent, you require a
@;different representation that uses memory and will reduce performance.

@section{Hints}
@itemlist[
@item{To compile @object-code{halt} to Paren-x64, it may help if you generate
a labeled instruction that essentially does nothing.
This way, you can insert a label at the end of your program.
Otherwise, you'll need to think carefully about how to arrange and generate
blocks.
}
@item{The register allocator must ensure that abstract locations shared across
multiple blocks are assigned the same physical location.
This is a small but subtle change to the algorithm.

Essentially, we @racket[foldr] the previous algorithm over the list of blocks.

We apply the earlier algorithm to produce an assignment for each block.
That algorithm returns an assignment.
That assignment should be passed to the next block in the list as the current
assignment.
Before running the core algorithm, you should "pre-allocate" all names in the
current assignment by removing them from the locals list and conflict graph.
This prevents them from being re-allocated by the core algorithm.
The core algorithm should be modified to return the current assignment in the
base case.
}
@item{The undead analysis is greatly simplified if you think hard about the
data structure used to represent undead sets.
In the previous assignment, undead sets were a list of sets of names.
This works because programs were essentially lists of instructions, so the
mapping was one-to-one.
Now, programs are not lists, but trees.

Here's an example of applying @racket[undead-analysis] to a program with
interesting features.

The reference compiler generates both undead-in and undead-out sets.
Due to some confusion in the description of A3 vs the examples from A3, your
compiler may use either.
(Either can be used in @racket[conflict-analysis].)

@examples[#:eval eg
(require racket/pretty)
(pretty-print-current-style-table (pretty-print-extend-style-table (pretty-print-current-style-table) '(module) '(begin)))
(pretty-display
 (undead-analysis
 '(module
      (define L.main.1
        ((locals (x.1 x.2 x.3 x.4)))
        (begin
          (set! x.1 0)
          (set! x.2 0)
          (begin
            (set! x.3 0)
            (set! x.4 0)
            (if (neq? x.3 x.4)
                (jump L.exit.1 x.1)
                (halt x.2)))))
      (define L.exit.1
        ((locals (x.1)))
        (halt x.1)))))
]

}
@item{For @racket[select-instructions], you need to translate an apply in to a
jump.
Given the abstractions of the lower languages, we can't really "call a
function".
We can only set some locations and jump.
So we can't implement a call unless we know what abstract locations to set
before jumping.
This means we can only jump to a known function.
We'll need calling conventions before we can handle indirect jumps, i.e., jumps
to labels that are not statically known.

This restricts the language further than the syntactic restrictions.

Given the abstractions of the lower languages, also don't know how to implement
"returning" from a function call, so we can't have anything left to do after a
function call.
This restriction is already encoded in the syntax.
@margin-note{Ron notes that if you @emph{really wanted} to support indirect
function calls without calling conventions, and you have a whole program, you
could set @emph{all possible abstract locations} for @emph{all labels} that
could be the target of the jump.
This would be very slow, and result in lots of unreadable code, but might work.
}
}

]
