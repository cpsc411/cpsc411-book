#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a6-solution)
  (for-label (only-in cpsc411/reference/a6-solution [check-values-lang v5:check-values-lang]))
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v2
  cpsc411/langs/v3
  cpsc411/langs/v4
  cpsc411/langs/v5
  cpsc411/langs/v6
  (for-label cpsc411/langs/v6))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a6-solution]

@(define sb
   (make-cached-eval
    "ch6-eval"
    '(require racket/pretty cpsc411/reference/a6-solution cpsc411/compiler-lib)))

@define[v6-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

L0 [label="Values-lang v6"];
L1 [label="Values-unique-lang v6"];
L2 [label="Proc-imp-mf-lang v6"];
L3 [label="Imp-mf-lang v6"];
L4 [label="Imp-cmf-lang v6"];
L5 [label="Asm-pred-lang v6"];
L6 [label="Asm-pred-lang v6/locals"];
L7 [label="Asm-pred-lang v6/undead"];
L8 [label="Asm-pred-lang v6/conflicts"];
L81 [label="Asm-pred-lang v6/pre-framed"];
L82 [label="Asm-pred-lang v6/framed"];
L83 [label="Asm-pred-lang v6/spilled"];
L9 [label="Asm-pred-lang v6/assignments"];
L10 [label="Nested-asm-lang-fvars v6"];
L10_1 [label="Nested-asm-lang v6"];
L11 [label="Block-pred-lang v6"];
L12 [label="Block-asm-lang v6"];
L12_1 [label="Para-asm-lang v6"];
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

L0 -> L0 [label=" check-values-lang"];
L0 -> L1 [label=" uniquify"];
L1 -> L2 [label=" sequentialize-let"];
L2 -> L3 [label=" impose-calling-conventions"]
L3 -> L4 [label=" canonicalize-bind"];
L4 -> L5 [label=" select-instructions"];

L10 -> L10 [label=" optimize-predicates"]
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

  L16 [label="Paren-x64 v6"];
  L17 [label="Paren-x64-rt v6"];
}

  L16 -> L17 [label=" link-paren-x64"];
  L17 -> L15 [label=" interp-loop"];
  L16 -> L15 [label=" interp-paren-x64"];
}
}
]

@title[#:tag "top" #:tag-prefix "chp-return:"]{Procedural Abstraction: Return}
@(define (ch5-tech . rest)
   (apply tech #:tag-prefixes '("book:" "chp5:") rest))

@section{Preface: What's wrong with our language?}
In @ch5-tech{Values-lang v5}, we added a limited form of @ch5-tech{procedure}
which supported only the @ch5-tech{call} abstraction.
This necessarily limited in which context @ch5-tech{procedures} could be called,
restricting them to @values-lang-v5[tail] context.
This is unfortunate, since we may want to use procedural abstraction to abstract
over side effects, and use such @ch5-tech{procedures} in @values-lang-v5[effect]
context, or to compute values as part of some computation to be used in
@values-lang-v5[value] context.
We can emulate these behaviours by manually writing our code in
continuation-passing style (CPS) (sometimes known as "callback hell"), but
we don't want to be sent to The Hague, so we will not design a language that
forces programmers use it.

To allow @ch5-tech{calls} in arbitrary context, we need the @deftech{return}
abstraction, which allows the language to essentially jump back from a
@ch5-tech{procedure} call into the middle of a computation.
Thankfully, we already have the abstractions required to implement this: labels,
jumps, and a calling convention.
All we need to do is slightly generalized the calling convention to introduce a
new label at the return point of a procedure call, store that label
somewhere, and arrange for the caller to jump back to that label.

@digression{
We might think we could pre-process @tech{Values-lang v6} to lift all non-tail
calls into new procedure and rewrite the program to have only tail calls.
This would correspond to a CPS transformation and would be difficult to optimize
correctly, particularly at this level of abstraction.
Each non-tail call would introduce an entire procedure call setup to the "rest"
of the body, as a continuation.
Any parameters live across the non-tail call would need to be packaged and
explicitly passed as arguments to the continuation.
By creating a return point abstraction, later in the compiler when we have
access to lower-level abstraction (basic blocks), we'll instead be able to
generate a single jump back to this return point, instead of a whole procedure
call.
}

@section{Designing a source language with return}
Below, we define @deftech{Values-lang v6} by extending @ch5-tech{Values-lang-v5}
with a @tech{return}, and with @ch5-tech{calls} in arbitrary contexts.

@bettergrammar*-diff[values-lang-v5 values-lang-v6]

The only syntactic change is the addition of @values-lang-v6[(call triv triv
...)] in @values-lang-v6[value] context.
The implementation of this requires a semantic change to call, to ensure it
@tech{returns}.
Otherwise, an expression such as @values-lang-v6[(let ([x (call f 5)]) (+ 1 x))]
would return the value @values-lang-v6[x], and not @values-lang-v6[(+ 1 x)] as
intended.

Note that @values-lang-v6[tail] and @values-lang-v6[value] context now coincide.
We do not collapse them yet, as imposing a distinction will improve our
compiler, by allowing us to transform @ch5-tech{tail calls} (which need not
@tech{return}) separately from @deftech{non-tail calls}, @ie
@ch5-tech{calls} in any context other than @values-lang-v6[tail] context.
This will let us maintain the performance characteristics of @ch5-tech{tail
calls}, namely that they use a constant amount of stack space, and do not need
to jump after their computation is complete.

We also add subtraction as a @values-lang-v6[binop].
This week, we start to need subtraction a lot more and it's tiring to encode it
when @ch1-tech{x64} supports it.
This requires almost no changes to the compiler if it parameterized by the set
of @values-lang-v6[binop]s.

@section{Extending our Calling Convention}

@;{

Prior to each call, the caller will store a return address in the parameter
@racket[current-return-address-register].
The default value is @racket['#,(current-return-address-register)].
Before a call, we store the return address in that register.
When a call returns, the callee jumps to the return address stored in that
register.

When returning from a call, we store the return value in
@racket[current-return-value-register].
The default value is @racket['#,(current-return-value-register)], which we've
been using as the final result of a program.
With proper function calls, we will never quite know when the program is
"complete".
We must assume that we will always return to some prior call.

}

In @Secref[#:tag-prefixes '("book:" "chp5:")]{top}, we designed a @ch5-tech{calling
convention}, but only aimed to support @ch5-tech{calls} without @tech{return}.
We need to modify it in three ways to support @tech{return}, and @tech{non-tail calls}.

First, we modify how we transform each procedure.
When generating code for a procedure, we cannot know (Rice's Theorem) whether it
will need to return or not, @ie whether it will be called via a @ch5-tech{tail
call} or @tech{non-tail call}.
We therefore design our calling convention to enable any procedure to
@tech{return}.

We modify our @ch5-tech{calling convention}, designating a register
@racket[current-return-address-register] in which to pass the
@deftech{return address}, the label to which the @ch5-tech{procedure} will jump
after it is finish executing.
On entry to any @ch5-tech{procedure}, we load the
@racket[current-return-address-register] into a fresh @ch2-tech{abstract
location}, which we call @imp-mf-lang-v6[aloc_tmp-ra].
This is necessary since the @racket[current-return-address-register] needs to be
available immediately for any new calls, but we will not need the
@tech{return address} until the end of the procedure.

For convenience, we slightly generalize from @ch5-tech{procedures} and define
@tech{entry points} that load the @racket[current-return-address-register].
An @deftech{entry point} is the top-level @proc-imp-mf-lang-v6[tail] expression
that begins execution of code.
This happens either as the body of a @ch5-tech{procedure}, or as the initial
@proc-imp-mf-lang-v6[tail] in a module @proc-imp-mf-lang-v6[(module tail)].

@digression{
This generalization lets us treat all @tech{returns}, including returning the
final value to the run-time system, uniformly, and allows us to eliminate the
@imp-mf-lang-v5[halt] instruction.
This isn't necessary; we could continue to support
@imp-mf-lang-v5[halt] and treat the module-level @tech{entry point} separate
from @ch5-tech{procedures}.
However, there is no benefit to doing so, and it clutters the compiler with
special cases.
The only benefit of keeping @imp-mf-lang-v5[halt] would be saving a single jump
instruction, but this has almost no cost, will probably be predicted by a CPU's
branch predictor, and could easily be optimized away by a small pass nearly
anywhere in the compiler pipeline.
}

By default, we use @racket[(unsyntax (current-return-address-register))]
as the @racket[current-return-address-register].

Second, we need to modify @tech{non-tail calls} to create the @tech{return
address} and update the @racket[current-return-address-register] prior to
jumping to the procedure.
Since we do not have access to labels @ch5-tech{Imp-mf-lang v5}, we will need to
introduce an abstraction for creating a @tech{return address} in our new
intermediate language.
After returning, the code at the @tech{return address} will read from a
designated register and continue the rest of the computation.
We reuse the @racket[current-return-value-register] as this designated register.

Finally, we need to explicitly return a value in @imp-mf-lang-v6[tail] position.
Previously, the final value in @imp-mf-lang-v6[tail] position was also the final
value of the program.
This value was implicitly returned to the run-time system.
However, now, a value in @imp-mf-lang-v6[tail] position may either be returned
to the run-time system, or may be returned to some other computation from a
non-tail call.
We transform a value in @imp-mf-lang-v6[tail] position by moving it into the
@racket[current-return-value-register], and jumping to the @tech{return
address} stored in @racket[aloc_tmp-ra].


@itemlist[
@item{When transforming an @tech{entry point} @proc-imp-mf-lang-v6[entry], we
generate:
@racketblock[
`(begin
  (set! ,tmp-ra ,(current-return-address-register))
  ,entry)
]
where:
@itemlist[
@item{@racket[tmp-ra] is a fresh @ch2-tech{abstract location} used to store the
@tech{return address} for this @tech{entry point}.}
]
}

@item{
When transforming a procedure @racket[`(lambda (,x_0 ... ,x_n-1 ,x_n ...
,x_n+k-1) ,body)], we generate:

@racketblock[
`(begin
   (set! ,x_0 ,r_0)
   ...
   (set! ,x_n-1 ,r_n-1)
   (set! ,x_n ,fv_0)
   ...
   (set! ,x_n+k-1 ,fv_k-1)
   ,body)
]
where:
@itemlist[
@item{@racket[fv_0 _... fv_k-1] are the first @racket[k] @ch2-tech{frame variables}.
}
@item{@racket[r_0 _... r_n-1] are the @racket[n] physical locations from
@racket[current-parameter-registers].}


]

The order of the @imp-mf-lang-v5[set!]s is not important for correctness, but
can influence optimizations.
We want to limit the live ranges of registers to help the register allocator make
better use of registers, so we should generate accesses to registers first to
limit their live ranges.
}

@item{When transforming a @ch5-tech{tail call}, @racket[`(call ,v ,v_0 ... ,v_n-1
,v_n ... ,v_n+k-1)], we generate:

@racketblock[
`(begin
   (set! ,fv_0 ,v_n) ...
   (set! ,fv_k-1 ,v_n+k-1) ...
   (set! ,r_0 ,v_0) ...
   (set! ,r_n-1 ,v_n-1) ...
   (set! ,ra ,tmp-ra)
   (jump ,v ,fbp ,ra ,r_0 ... ,r_n-1 ,fv_0 ... ,fv_k-1))
]
where:
@itemlist[
@item{@racket[fbp] is the physical location storing the frame base pointer,
@racket[current-frame-base-pointer-register].}
@item{all other meta-variables are the same as in the case for transforming
@object-code{lambda}.}
@item{@racket[tmp-ra] is the same fresh variable generated on entry to the
current @tech{entry point}.}
@item{@racket[ra] is the physical location storing the return address,
@racket[current-return-address-register].}
]
}

@item{When transforming a base @imp-mf-lang-v6[value] (either a
@imp-mf-lang-v6[triv] or @imp-mf-lang-v6[(binop opand opand)]) @imp-mf-lang-v6[value] in @imp-mf-lang-v6[tail]
position we generate:
@racketblock[
`(begin
   (set! ,rv ,value)
   (jump ,tmp-ra ,fbp ,rv))
]
where:
@itemlist[
@item{@racket[rv] is the physical location used to store the return value,
@racket[current-return-value-register].}
@item{@racket[fbp] is the physical location storing the frame base pointer,
@racket[current-frame-base-pointer-register].}
@item{@racket[tmp-ra] is the designated @ch2-tech{abstract location} created for the
current @tech{entry point}.}
]

This explicitly returns the value to the current @tech{return address}.
}

@item{When transforming a call in @emph{non-tail position}, @ie a @tech{non-tail
call} @racket[`(call ,v ,v_0 ... ,v_n-1 ,v_n ... ,v_n+k-1)], we generate the
following.
@todo{Kent adds non-tail calls in effect context in this assignment.}
@racketblock[
`(return-point ,rp-label
   (begin
     (set! ,nfv_0 ,v_n) ...
     (set! ,nfv_k-1 ,v_n+k-1) ...
     (set! ,r_0 ,v_0) ...
     (set! ,r_n-1 ,v_n-1) ...
     (set! ,ra ,rp-label)
     (jump ,v ,fbp ,ra ,r_0 ... ,r_n-1 ,nfv_0 ... ,nfv_k-1)))
]
where:
@itemlist[
@item{@racket[rp-label] is a fresh label.

Intuitively, we need some way to introduce a label at @emph{this instruction},
so when the call is complete we can return to the instruction after the call.
This @imp-mf-lang-v6[return-point] will be a new instruction in the target
language that introduces a fresh label @racket[rp-label].

Note that our non-tail calls can appear in value position, for example, as the
right-hand side of a @imp-mf-lang-v6[(set! aloc value)] instruction.
Recall that after the non-tail call, the return value of the procedure
will be stored in @racket[(current-return-value-register)].
When normalizing the @imp-mf-lang-v6[set!] instructions
@racket[canonicalize-bind], we can translate this instruction as:
@racketblock[
`(begin
   ,translation-of-non-tail-call
   (set! ,aloc ,rv))
]
}
@item{@racket[nfv_0 _... nfv_k-1] should be the first @racket[k] @emph{frame
variables}, @ie locations on the @emph{callee's} frame.

However, we can't make these frame variables yet.
These variables are assigned in the caller, and the callee may not have
the same frame base as the caller.
Consider the following expression with a non-tail call using frame variables.
@racketblock[
`(begin
   (set! fv0 1)
   (set! rax 42)
   (return-point L.rp.1
     (set! fv0 rax)
     (set! r15 L.rp.1)
     (jump L.label.1 rbp r15 fv0))
   (set! r9 fv0)
   (set! rax (+ rax r9)))
]

For this example, suppose we've exhausted our
@racket[(current-parameter-registers)], which we simulate by making it the
empty set.

The frame location @racket['fv0] is live across the call; we use it after
the call returns.
However, we need to store @racket['rax] in the callee's @racket['fv0].
If we try to use frame variables directly, we overwrite the original value of
@racket['fv0].
We first need to figure out how large the caller's frame is, and then move
@racket['rax] to the index @emph{after} the last frame location used by the
caller.

Prior to undead analysis, we don't know how much we need to increment the base
frame pointer to save variables live after this non-tail call.
This makes figuring the exact index for these new frame variables non-trivial.
We need to know the base of the callee's frame, while we only know the base of
our frame, @ie the caller's frame.

But to do undead analysis, we really need both of those occurrences of
@racket['fv0] to be unique.
We've assumed all locations are uniquely identified by name, but the return
point changes the interpretation of frame variables, so two occurrences of
the same frame variable are not necessarily the same.
We want to avoid introducing such an ambiguous location.

Instead, we'll generate something like this:
@racketblock[
`(begin
   (set! fv0 1)
   (set! rax 42)
   (return-point L.rp.1
     (set! nfv.0 rax)
     (set! r15 L.rp.1)
     (jump L.label.1 rbp r15 nfv.0))
   (set! r9 fv0)
   (set! rax (+ rax r9)))
]
where @imp-mf-lang-v6[nfv.0] must be assigned to frame location 0 in the
callee's frame.
For now, we record these @emph{new-frame variables} in an info field, and
we'll figure out how to assign frames later.

Note that this is only a problem in a non-tail call.
In a tail call, we can reuse the caller's frame as the callee's frame, since we
never return.
}
]
}
]

@section{Extending front-end with support for non-tail calls}
As we don't require exposing any new low-level primitives, we modify our
compiler proceeding top-down instead of bottom-up.

The main required changes are in the calling convention, so we begin by
extending all the passes between the source language and
@racket[impose-calling-conventions] to support @tech{non-tail calls}.

We first update @racket[check-values-lang] to allow @tech{non-tail calls}.
The heuristics remain the same as in @racket[v5:check-values-lang].

@nested[#:style 'inset]{
@defproc[(check-values-lang [p values-lang-v6?]) values-lang-v6?]{
Validates that the @tech{Values-lang v6} is well bound and well typed: all
procedure calls pass the correct number of arguments, and all
@values-lang-v6[binop] and @values-lang-v6[relop] are never used with labels.
}
}

Next, we extend @racket[uniquify].
First, of course, we design the updated @deftech{Values-unique-lang v6}.
We typeset the differences with respect to @ch5-tech{Values-unique-lang v5}.

@bettergrammar*-diff[values-unique-lang-v5 values-unique-lang-v6]

This requires no changes specific to @tech{non-tail calls}, so the changes
compared to @racket[v5:uniquify] are trivial.

@nested[#:style 'inset
@defproc[(uniquify (p values-lang-v6?)) values-unique-lang-v6?]{
Compiles @tech{Values-lang v6} to @tech{Values-unique-lang v6} by resolving
top-level @ch3-tech{lexical identifiers} into unique labels, and all other
@ch3-tech{lexical identifiers} into unique @ch2-tech{abstract locations}.
}
]

Finally, we expose @tech{non-tail calls} through @racket[sequentialize-let].
Below we define @deftech{Proc-imp-mf-lang v6}, where we transform lexical
binding into sequential imperative assignments.

@bettergrammar*-ndiff[
#:labels ("v5 Diff (excerpts)" "Full")
(#:exclude (triv opand relop int64 aloc label) proc-imp-mf-lang-v5 proc-imp-mf-lang-v6)
(proc-imp-mf-lang-v6)
]

Note that this language contains a definition @proc-imp-mf-lang-v6[entry]
designating the top-level tail used as the @tech{entry point} for each
@ch5-tech{procedure} and for the module as a whole.
There is no syntactic distinction, but making a semantic distinction will
simplify our implementation of the @tech{calling convention} to support @tech{return}.

@todo{Kent adds non-tail calls in effect context in this assignment. We could
add it here for future-proofing, but probably not important to do so. It's not
exposed in the surface yet.}

@nested[#:style 'inset
@defproc[(sequentialize-let (p values-unique-lang-v6?))
         proc-imp-mf-lang-v6?]{
Compiles @tech{Values-unique-lang v6} to @tech{Proc-imp-mf-lang v6} by picking a
particular order to implement @values-unique-lang-v6[let] expressions using
@proc-imp-mf-lang-v6[set!].
}
]

@section{Extending Calling Convention}

Next we design @deftech{Imp-mf-lang v6}, the target language of the calling
convention translation.
Below, we typeset the differences compared to @ch5-tech{Imp-mf-lang v5}.

@bettergrammar*-diff[imp-mf-lang-v5 imp-mf-lang-v6]

@todo{Jump disappears from value context, transformed into effect context.
Should point that out.. and double check.}

We now allow @imp-mf-lang-v6[(jump trg opand ...)] in @imp-mf-lang-v6[value]
position.
This corresponds to the addition of @tech{non-tail calls} to the source
language.

We also add the @imp-mf-lang-v6[return-point] form to effect context.
This instruction introducing a new, non-top-level @imp-mf-lang-v6[label] in the
middle of an instruction sequence, which is expected to be exclusively used by
the calling convention to implement @tech{return}.
By introducing this new abstraction, we are required to implement this
abstraction lower in the compiler pipeline.

We further assume that a @imp-mf-lang-v6[return-point] cannot appear inside
another @imp-mf-lang-v6[return-point], @ie there are no nested
@imp-mf-lang-v6[return-point]s.
Our compiler can never generate this code, and there is no reason to support.

The implicit return value, @imp-mf-lang-v6[value] in @imp-mf-lang-v6[tail]
position, is no longer valid.
Instead, the run-time system will set the first return address, and the final
result is returned to the run-time system using the @ch5-tech{calling conventions}.
The run-time system initializes the @racket[current-return-address-register] to
be the address of the exit procedure.

To implement @tech{return}, we modify every @tech{entry point} to store the
@racket[current-return-address-register] as described by our calling convention.
Then we explicitly @tech{return} the base expressions in @imp-mf-lang-v6[value]
context.

To implement @imp-mf-lang-v6[fvar]s later, we require that
@racket[current-frame-base-pointer-register] is assigned only by
incrementing or decrementing it by an integer literal.
Other uses @racket[current-frame-base-pointer-register] are @emph{invalid
programs}.
Later passes will assume this in order to compute frame variable locations.

The @imp-mf-lang-v6[info] field records all the new frames created in the block,
and will be used later to push new frames on to the stack, and assign new-frame
variables to frame locations.
The new-frame variables should be in order.
Each new-frame variable must only appear in one list in the
@imp-mf-lang-v6[new-frames] field.
Recall that @imp-mf-lang-v6[aloc]s are unique, and the @imp-mf-lang-v6[new-frames]
field represents newly defined @imp-mf-lang-v6[aloc]s.
It would not make sense for the same @imp-mf-lang-v6[aloc] to appear in two frames.

@nested[#:style 'inset
@defproc[(impose-calling-conventions [p proc-imp-mf-lang-v6?]) imp-mf-lang-v6?]{
Compiles @tech{Proc-imp-mf-lang v6} to @tech{Imp-mf-lang v6} by imposing calling
conventions on all calls (both tail and non-tail calls), and @tech{entry
points}.
The registers used to passing parameters are defined by
@racket[current-parameter-registers], and the registers used for returning are
defined by @racket[current-return-address-register] and
@racket[current-return-value-register].
}
]

After implementing the calling conventions, we have two abstractions that we
need to implement.

First, we must implement frames, or more specifically, a @tech{stack of frames}
(also known as a @tech{stack}).
@tech{Non-tail calls} cannot reuse their frame since some @ch2-tech{abstract
locations} may be @ch-ra-tech{live} (will be @ch-ra-tech{undead}) after the call.
In general, there will not be enough registers to keep them all around, so we
store them on the frame.
But if the caller starts writing to the frame, it would overwrite live values.
So we need to install a new frame for the caller before executing the
@tech{non-tail call}.
We've already collected the new frame variables, and we need to modify the
register allocator to determine the size and allocate new frames.
This requires explicitly manipulating the frame base pointer, which also changes
how @ch2-tech{frame variables} are implemented.

Second, we need to implement @imp-mf-lang-v6[return-point]s.
These will be compiled to raw labels, so we essentially preserve them until a
low-level language with access to raw labels.

@section{Implementing A Stack of Frames}

Our calling convention passes the first @racket[n] arguments as registers,
using the set of registers defined in the parameter
@racket[current-parameter-registers].
To deal with an arbitrary number of arguments, we may need more than the
@racket[n] registers we have available for parameters.
For the rest, we use fresh frame locations.

Unfortunately, the callee will not know the exact offset into the frame that we,
the caller, might be using.
We therefore need to agree a priori on which frame locations are used across the
call.
Since there might be arbitrary frame locations currently in use (for example,
because the register allocator will be putting some abstract locations on the
frame), there is no particular index that it's safe to start from.

The solution is to introduce a @tech{stack of frames}.
Each function assumes that, prior to being called, it was given a
@emph{brand-new frame} from which it can start indexing at 0.
The callee knows how many arguments it expected to receive, and can start
counting from 0.
The caller knows how many frame locations it has used, and can increment the
frame base pointer beyond its own locations to a safe starting index.
After the function call returns, the caller can decrement the frame base pointer
by the same amount, restoring its own frame.
These increment/decrement operations can be interpreted as "pushing" and
"popping" a new frame on and off the @deftech{stack of frames} (which we will
usually shorten to just @deftech{stack}).

This usage of @tech{stack} differs from what @ch1-tech{x64} provides.
We now require the @tech{stack} to be frame-aligned, and any accesses outside
the current frame boundaries results in undefined behaviour.

To compute the size of each caller's frame, we need to know how many variables
might be live across a call, so this must wait until after
@racket[undead-analysis].
To do a good job assigning these to frame locations, we also want to wait until
after @racket[conflict-analysis], so we can try to assign non-conflicting
variables to the same frame location.

@subsection{Updating intermediate passes}
Before allocating frames, there are a few passes we must update to pass through
our new abstractions.

First, we extend @racket[canonicalize-bind].
We define @deftech{Imp-cmf-lang v6} below.
We typeset the differences compared to @ch5-tech{Imp-cmf-lang v5}.

@bettergrammar*-diff[imp-cmf-lang-v5 imp-cmf-lang-v6]

We simply extend @ch5-tech{Imp-cmf-lang v5} with our new abstractions, including
@tech{non-tail calls} and return points.
We also require the @imp-cmf-lang-v6[new-frames] declaration in the @imp-cmf-lang-v6[info] field.

@nested[#:style 'inset
@defproc[(canonicalize-bind (p imp-mf-lang-v6?))
imp-cmf-lang-v6?]{
Compiles @tech{Imp-mf-lang v6} to @tech{Imp-cmf-lang v6}, pushing
@imp-mf-lang-v6[set!] under @imp-mf-lang-v6[begin] so that the right-hand-side
of each @imp-mf-lang-v6[set!] is base value-producing operation.

This canonicalizes @tech{Imp-mf-lang v6} with respect to the equations:
@tabular[
(list
(list
@imp-mf-lang-v6[(set! aloc (begin effect_1 ... value))]
"="
@imp-mf-lang-v6[(begin effect_1 ... (set! aloc value))])
(list
@imp-mf-lang-v6[(set! aloc (if pred value_1 value_2))]
"="
@imp-mf-lang-v6[(if pred (set! aloc value_1) (set! aloc value_2))])
(list
@imp-mf-lang-v6[(set! aloc (return-point label tail))]
"="
@imp-mf-lang-v6[(begin (return-point label tail) (set! aloc
,(current-return-value-register)))])
)
]
}
]

Next we impose some machine restrictions on our language with
@racket[select-instructions].
We define @deftech{Asm-pred-lang v6} below, with changes typeset with respect to
@ch5-tech{Asm-pred-lang v5}.

@bettergrammar*-diff[asm-pred-lang-v5 asm-pred-lang-v6]

There are no new restrictions for @asm-pred-lang-v6[return-point].
We simply extend the pass to support @asm-pred-lang-v6[jumps] in effect context.

@nested[#:style 'inset
@defproc[(select-instructions (p imp-cmf-lang-v5?))
         asm-pred-lang-v5?]{
Compiles @tech{Imp-cmf-lang v6} to @tech{Asm-pred-lang v6}, selecting
appropriate sequences of abstract assembly instructions to implement the
operations of the source language.
}
]

@subsection{Analyzing Return Points and Non-tail Calls}

We first extend @racket[uncover-locals] to find locals in non-tail calls and
return points.
Below we define @deftech{Asm-pred-lang-v6/locals}.
We typeset changes compared to @ch5-tech{Asm-pred-lang v5/locals}.

@bettergrammar*-diff[asm-pred-lang-v5/locals asm-pred-lang-v6/locals]

Updating this analysis is not complicated.
We simply add a case to handle @asm-pred-lang-v6[jump] in tail position, and to
traverse return points.
Remember that the "arguments" to @asm-pred-lang-v6[jump] are only used for later
analyses and not consider locals for this analysis.

@nested[#:style 'inset
@defproc[(uncover-locals (p asm-pred-lang-v6?))
          asm-pred-lang-v6/locals?]{
Compiles @tech{Asm-pred-lang v6} to @tech{Asm-pred-lang v6/locals}, analysing
which @ch2-tech{abstract locations} are used in each block, and each block and
the module with the set of variables in an @racket[info?] fields.

The new-frame variables should be listed in the locals set for the enclosing
block.
}
]

@todo{hint:
It will help if you interpret the locals set as the set of unassigned variables,
and remove variables from the set as they are assigned.
}

Next we extend @racket[undead-analysis].
We design @deftech{Asm-pred-lang-v6/undead} below, typeset with respect to @ch5-tech{Asm-pred-lang v5/undead}.

@bettergrammar*-diff[asm-pred-lang-v5/undead asm-pred-lang-v6/undead]

We add two new @asm-pred-lang-v6[info] fields: @asm-pred-lang-v6/undead[undead-out] and @asm-pred-lang-v6/undead[call-undead].
The @asm-pred-lang-v6/undead[undead-out] field will continue to store the
@ch-ra-tech{undead-set tree}, which we must update to track
@asm-pred-lang-v6[return-point]s.
The @asm-pred-lang-v6/undead[call-undead] is the set of all locations that are live
after @emph{any} non-tail call in a block.

The @asm-pred-lang-v6/undead[call-undead] field stores @emph{every}
abstract location or frame variable that is in the undead-out set of a return
point.
These must be allocated separately from other variables, so we store them
separately.

First, we update the definition of @racket[undead-set-tree?]
to handle the @asm-pred-lang-v6[return-point] instruction, which includes a
nested @asm-pred-lang-v6[tail].
@todo{Since the predicate changes, probably need to add multiple definitions to
cpsc411-lib.}

@verbatim{
Undead-set-tree is one of:
- Undead-set
- (list Undead-set Undead-set-tree)
- (list Undead-set Undead-set-tree Undead-set-tree)
- (listof Undead-set-tree)

WARNING: datatype is non-canonical since Undead-set-tree can be an
         Undead-set, so third and fourth case, and the second and fourth case can
         overlap.
         An Undead-set-tree is meant to be traversed simultaneously with an
         Asm-pred-lang tail, so this ambiguity is not a problem.

interp. a tree of Undead-sets.
The structure of the tree mirrors the structure of a Asm-pred-lang tail.
There are three kinds of sub-trees:

(1) a set! node is simply an undead set;
(2) a return-point node is a list whose first element is the undead-set
    representing the undead-out, of the return-point (the locations undead after
    the call), and whose second element is the Undead-set-tree of the nested
    tail.
(3) an if node has an Undead-set for the predicate and two sub-trees for the
    branches.
(4) a begin node is a list of Undead-sets-trees, each corresponding to an
    instruction in a begin tail.
}

@racketblock[
(define (undead-set-tree? ust)
  (match ust
    (code:comment "for an instruction")
    [(? undead-set?) #t]
    (code:comment "for a return point")
    [(list (? undead-set?) (? undead-set-tree?))]
    (code:comment "for an if")
    [(list (? undead-set?) (? undead-set-tree?) (? undead-set-tree?)) #t]
    (code:comment "for a begin")
    [`(,(? undead-set-tree?) ...) #t]
    [else #f]))
]

Analyzing non-tail jumps is no different from other jumps; we reuse the
"arguments" annotated on the jump as the undead-out set, and discard the
annotation.

Analyzing @asm-pred-lang-v6[return-point] requires making explicit a fact from
our calling convention.
After returning, we expect @racket[current-return-value-register] to be live.
We model this as treating a @asm-pred-lang-v6[return-point] as assigning the
@racket[current-return-value-register].


@nested[#:style 'inset
@defproc[(undead-analysis (p asm-pred-lang-v6/locals?))
          asm-pred-lang-v6/undead?]{
Performs undead analysis, compiling @tech{Asm-pred-lang v6/locals} to
@tech{Asm-pred-lang v6/undead} by decorating programs with their
@ch-ra-tech{undead-set trees}.
}
]

@examples[#:eval sb
(pretty-display
 ((compose
   undead-analysis
   uncover-locals
   select-instructions
   canonicalize-bind
   impose-calling-conventions
   sequentialize-let)
  '(module
     (define L.swap.1
       (lambda (x.1 y.2)
         (if (< y.2 x.1)
             x.1
             (let ([z.3 (call L.swap.1 y.2 x.1)])
               z.3))))
     (call L.swap.1 1 2))))

(parameterize ([current-parameter-registers '()])
  (pretty-display
   ((compose
     undead-analysis
     uncover-locals
     select-instructions
     canonicalize-bind
     impose-calling-conventions
     sequentialize-let)
    '(module
       (define L.swap.1
         (lambda (x.1 y.2)
           (if (< y.2 x.1)
               x.1
               (let ([z.3 (call L.swap.1 y.2 x.1)])
                 z.3))))
       (call L.swap.1 1 2)))))

]

The following example shows the output on an intermediate representation of
non-tail recursive factorial, compiled with @racket[current-parameter-registers]
set to @racket['()] to force the compiler to generate frame variables and test
edge cases.
@examples[#:eval sb
(pretty-display
 (undead-analysis
  '(module
     ((new-frames ()) (locals (ra.12)))
     (define L.fact.4
       ((new-frames ((nfv.16)))
        (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17)))
       (begin
         (set! x.9 fv0)
         (set! ra.13 r15)
         (if (= x.9 0)
             (begin (set! rax 1) (jump ra.13 rbp rax))
             (begin
               (set! tmp.14 -1)
               (set! tmp.15 x.9)
               (set! tmp.15 (+ tmp.15 tmp.14))
               (set! new-n.10 tmp.15)
               (return-point
                   L.rp.6
                 (begin
                   (set! nfv.16 new-n.10)
                   (set! r15 L.rp.6)
                   (jump L.fact.4 rbp r15 nfv.16)))
               (set! factn-1.11 rax)
               (set! tmp.17 x.9)
               (set! tmp.17 (* tmp.17 factn-1.11))
               (set! rax tmp.17)
               (jump ra.13 rbp rax)))))
     (begin
       (set! ra.12 r15)
       (set! fv0 5)
       (set! r15 ra.12)
       (jump L.fact.4 rbp r15 fv0)))))
]

Next we update the @racket[conflict-analysis].
Below, we define @deftech{Asm-pred-lang v6/conflicts}, typeset with differences
compared to @ch5-tech{Asm-pred-lang v5/conflicts}.

@bettergrammar*-diff[asm-pred-lang-v5/conflicts asm-pred-lang-v6/conflicts]

We need to assign the new-frame variables to frame locations.
However, we also reuse frame locations when possible, to minimize the size of
frame and thus memory usage.
@todo{Don't we also need to include all physical locations in conflicts?}

This is straightforward to solve.
We run @racket[conflict-analysis], but also collect conflicts between
@ch2-tech{abstract locations} and @ch2-tech{physical locations}.

Recall that @racket[current-return-value-register] is assigned by a non-tail call.
Also note that @racket[current-frame-base-pointer-register] and
@racket[current-return-value-register] are likely to end up in conflict with
everything, even though we have removed them from the
@racket[current-assignable-registers] set.

The interpretation of the conflict graph will be somewhat more difficult than in
prior versions.
It might contain conflicts between physical locations, which will never matter
since we don't try to assign physical locations.
@;The code will be extremely similar to @racket[register-conflict-analysis], and
@;you might want to design a single function that abstracts each.

@;@todo{Should detect fvars and add a conflict with rbp? Or just assume rbp is
@;always live.}

@margin-note{If our frame allocation was more clever, we would need to adjust
the conflict analysis to make all caller saved registers in conflict with a
non-tail call.
However, we instead assign all call-undead variables to the frame, so we don't
need to do very much for non-tail calls.
}

@nested[#:style 'inset
@defproc[(conflict-analysis (p asm-pred-lang-v6/undead?))
          asm-pred-lang-v6/conflicts?]{
Performs conflict analysis, compiling @tech{Asm-pred-lang v6/undead}
to @tech{Asm-pred-lang v6/conflicts} by decorating programs with their
conflict graph.
}
]

@subsection{Frame Allocation}

@todo{This probably belong earlier, before undead and conflict analysis, because
this design motives those changes.}

The size of a frame @racket[n] (in slots) for a given @tech{non-tail call} is
one more than the maximum of:
@itemlist[
@item{the number of locations in the undead-out set for the non-tail call, or}

@item{the index of the largest frame location in the undead-out set for the
non-tail call.}
]
The frame for the call must save all location live across the call, since the
caller might overwrite any register.
Since we're allowing physical locations in our source language, we could have a
frame variable live after a call, and must preserve up to that index.

We can model this as follows, although our implementation will be simpler as we
discuss.
Prior to the call, we push all locations live across the all onto the frame,
then increment the base frame pointer.
After the call, we decrement the base frame pointer, restoring the caller's
frame, and load all locations live after the call from the frame.

@margin-note{In practice, calling conventions distinguish between two sets of
registers: callee-saved and caller-saved, to allow some registers to be live
across a call.
We ignore this for simplicity and assume all registers are caller-saved.}

Intuitively, we want to transform @racket[`(return-point ,rp ,tail)] into:
@racketblock[
`(begin
   (set! ,nfv_0 ,x_0)
   ...
   (set! ,nfv_n-1 ,x_n-1)

   (set! ,fbp (+ ,fbp ,nb))
   (return-point ,rp ,tail)
   (set! ,fbp (- ,fbp ,nb))

   (set! ,x_0 ,nfv_0 )
   ...
   (set! ,x_n-1 ,nfv_n-1))
]
where:
@itemlist[

@item{@racket[nb] is the number of bytes required to save @racket[n] slots on
the frame, @ie @racket[(* n (current-word-size-bytes))].}

@item{@racket[fbp] is the value of the parameter
@racket[current-frame-base-pointer-register].}

@item{@racket[x_0], ... @racket[x_n] are the locations in the undead-out set for
the non-tail call.}

@item{@racket[nfv_0], ... @racket[nfv_n-1] are @racket[n] free frame variables.}
]

@;Unfortunately, there are two problems with this desired transformation.
@;
@;... conflict analysis

Unfortunately, we can't implement this transformation as it.
We want to avoid producing new @asm-pred-lang-v6[set!]s.
First, they will invalidate the undead analysis we've just performed.
Second, some or all the moves might be unnecessary.
We don't know whether those variables @racket['x_0 ... 'x_n-1] need to be in
registers, so it would be potentially more efficient to assign them to frame
locations in the first place and leave some other pass to move them into
registers when necessary.

Instead, we perform this transformation in two steps.
First, a new pass @racket[assign-call-undead-variables] assigns each location
that is live across a call to frame locations, instead of producing new moves.
This produces a partial assignment of abstract locations to frame
variables, which the register allocator will work from.
Second, a new pass, @racket[allocate-frames] does the work of updating the frame
base pointer, effectively allocating a frame for each non-tail call.

We define @deftech{Asm-pred-lang-v6/pre-framed} below, typeset with changes with
respect to @tech{Asm-pred-lang-v6/conflicts}.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v6/conflicts asm-pred-lang-v6/pre-framed]

The core of @racket[assign-call-undead-variables] is similar to
@racket[assign-registers].
The core algorithm is a straight-forward recursion over the
@asm-pred-lang-v6/pre-framed[call-undead] set, and produces an
@asm-pred-lang-v6/pre-framed[assignment].
@itemlist[
@item{If the input set is empty, return the default
@asm-pred-lang-v6/pre-framed[assignment].}
@item{Choose a variable, @racket[x], from the input set of variables.}
@item{Recur with @racket[x] removed from the input set and the
conflict graph.
The recursive call should return an assignment for all the remaining variables.}
@item{Select a compatible frame variable for @racket[x].

A variable @racket[x] is compatible with a frame location @asm-pred-lang-v6[fvar_i]
if it is not directly in conflict with @asm-pred-lang-v6[fvar_i], and it is not
in conflict with a variable @racket[y] that has been assigned to
@asm-pred-lang-v6[fvar_i].

An easy way to find a compatible frame variable is to find the set of frame
variables to which @racket[x] cannot be assigned.
Then, starting from @asm-pred-lang-v6[fv0], assign the first frame variable that
is not in the incompatible set.

Finally, add the assignment for the @racket[x] to the result of the recursive
call.
}
]

The default @object-code{assignment} for this pass is the empty
@object-code{assignment}, since nothing has been assigned yet.

@todo{hint
You might want to use @racket[make-fvar] from @share{a6-compiler-lib.rkt}.
}

Since many frame variables will be assigned prior to register allocation,
you will need to modify @racket[assign-registers] to use a similar algorithm for
spilling, instead of a naive algorithm that starts spilling at frame location 0.

@todo{hint
If carefully designed, most of this code can be reused later when we modify
@racket[assign-registers].

As in You should remove the assigned variables from the locals set, to allow later
passes to assume the locals set are all unassigned.
}

@nested[#:style 'inset
@defproc[(assign-call-undead-variables (p asm-pred-lang-v6/conflicts?))
         asm-pred-lang-v6/pre-framed?]{
Compiles @tech{Asm-pred-lang-v6/conflicts} to @tech{Asm-pred-lang-v6/pre-framed}
by pre-assigning all variables in the
@asm-pred-lang-v6/conflicts[call-undead] sets to
to frame locations.
}]

@;@todo{Change locals to homeless? Then when introducing the fixed point
@;algorithm, could name the break condition "homelessness-eliminated".}
@examples[#:eval sb
(parameterize ([current-parameter-registers '()])
  (pretty-display
   ((compose
     assign-call-undead-variables
     conflict-analysis
     undead-analysis
     uncover-locals
     select-instructions
     canonicalize-bind
     impose-calling-conventions
     sequentialize-let)
    '(module
       (define L.swap.1
         (lambda (x.1 y.2)
           (if (< y.2 x.1)
               x.1
               (let ([z.3 (call L.swap.1 y.2 x.1)])
                 z.3))))
       (call L.swap.1 1 2)))))
]

Now we can allocate frames for each non-tail call.
For each block, we compute the size of the frames for @emph{all} non-tail call,
and adjust each.
The size @racket[n] is one plus the maximum of the index of all frame variables
in the @asm-pred-lang-v6/pre-framed[call-undead] set for the enclosing block.

To adjust the callee's frame, we transform @racket[`(return-point ,rp ,tail)]
into
@racketblock[
`(begin
   (set! ,fbp (- ,fbp ,nb))
   (return-point ,rp ,tail)
   (set! ,fbp (+ ,fbp ,nb)))
]
where:
@itemlist[
@item{@racket[nb] is the number of bytes required to save @racket[n] slots on
the frame, @ie @racket[(* n (current-word-size-bytes))].
}
@item{@racket[fbp] is @racket[(current-frame-base-pointer-register)].}
]
Recall that the stack grows downward, so we @emph{subtract} @racket[nb] bytes
from the current frame base pointer to allocate a frame of with @racket[n]
slots.

@margin-note{We could allocate a different sized frame for each call, but this
would require associating @asm-pred-lang-v6/pre-framed[call-undead] sets with
each return point, and complicate the assignment of new-frame variables.}

We also assign each of the new-frame variables from the
@asm-pred-lang-v6/pre-framed[new-frame] lists.
In order, each new-frame variable is assigned to a frame variable starting with
@racket[(make-fvar n)].
These assignments are added to the @asm-pred-lang-v6/pre-framed[assignment]
field the enclosing block.

The output is @deftech{Asm-pred-lang-v6/framed}, which only changes in its
info fields compared to @tech{Asm-pred-lang-v6/pre-framed}.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v6/pre-framed asm-pred-lang-v6/framed]

The only differences are in the info field.
The call-undead sets and new-frame fields are removed.

We remove the assigned variables from the locals set, to allow later
passes to assume the locals set are all unassigned.

@nested[#:style 'inset
@defproc[(allocate-frames (p asm-pred-lang-v6/pre-framed?))
         asm-pred-lang-v6/framed?]{
Compiles @tech{Asm-pred-lang-v6/pre-framed} to @tech{Asm-pred-lang-v6/framed} by
allocating frames for each non-tail call, and assigning all new-frame variables
to frame variables in the new frame.
}
]

@examples[#:eval sb
(parameterize ([current-parameter-registers '()])
  (pretty-display
   ((compose
     allocate-frames
     assign-call-undead-variables
     conflict-analysis
     undead-analysis
     uncover-locals
     select-instructions
     canonicalize-bind
     impose-calling-conventions
     sequentialize-let)
    '(module
       (define L.swap.1
         (lambda (x.1 y.2)
           (if (< y.2 x.1)
               x.1
               (let ([z.3 (call L.swap.1 y.2 x.1)])
                 z.3))))
       (call L.swap.1 1 2)))))
]

@examples[#:eval sb
(parameterize ([current-parameter-registers '()])
  (pretty-display
   ((compose
     conflict-analysis
     undead-analysis
     uncover-locals
     select-instructions
     canonicalize-bind
     impose-calling-conventions
     sequentialize-let)
    '(module
       (define L.swap.1
         (lambda (x.1 y.2)
           (if (< y.2 x.1)
               x.1
               (let ([z.3 (call L.swap.1 y.2 x.1)])
                 z.3))))
       (call L.swap.1 1 2)))))]


Because the frame allocator sits in the middle of our register allocation
pipeline, the optimized allocator @racket[assign-homes-opt] is no longer a drop-in
replacement for the naive @racket[assign-homes].
We therefore remove @racket[assign-homes-opt] and in-line the passes in the
@racket[current-pass-list].

@subsection{Adjusting the Register Allocator}
Frames are now implemented and all new-frame variables and variables live across
a call are assigned to frame locations.
We need to adjust our register allocator so that it does not try to spill
variables into frame variables that are already taken.

To do this, we essentially remove spilling from @racket[assign-registers].
Instead, in the output, the @asm-pred-lang-v6/spilled[locals] set should include
only spilled locations.
A separate pass (which looks suspiciously like
@racket[assign-call-undead-variables]) handles spilling.

@todo{This commented out discussion?!}
@;@digression{A more general solution that will produce better code in general is
@;to run the undead and conflict analysis, register allocator, spilling, and patch
@;instructions in a giant fixed point iteration.
@;Patch instruction would generate "unspillable" variables instead of using
@;temporary registers, directing the register allocator that these must be
@;assigned registers.
@;Since it generates new instructions, the undead and conflict analyses must be
@;run again.
@;Then register allocation can run, possibly producing spills.
@;The spills are allocated, then instructions are patched, possibly producing new
@;unspillables.
@;This algorithm terminates with a small number of registers available because the
@;generated unspillables have very small live ranges.
@;}

@deftech{Asm-pred-lang-v6/spilled} is defined below, only changed by allowing
assignments to arbitrary @asm-pred-lang-v6/spilled[rloc]s.
We typeset the differences with respect to @tech{Asm-pred-lang-v6/framed}.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v6/framed asm-pred-lang-v6/spilled]

@nested[#:style 'inset
@defproc[(assign-registers (p asm-pred-lang-v6/framed?))
          asm-pred-lang-v5/spilled?]{
Performs graph-colouring register allocation, compiling
@tech{Asm-pred-lang v6/framed} to @tech{Asm-pred-lang v6/spilled} by
decorating programs with their register assignments.
}
]

@;Before running the register allocator, you should remove from the conflict graph
@;any location known to be assigned to frame variables.
@;This includes the @object-code{call-undead} set

@;@margin-note{
@;It's possible to reuse the more general cross-block allocation strategy to
@;optimize register use across blocks, but it requires that we distinguish between
@;functions that "escape" (might be called outside of our unit of compilation),
@;and those that don't.
@;Escaping functions must strictly adhere to calling conventions.
@;We assume all functions escape.
@;}

The final change to the register allocator is to assign spilled variables to
frame locations.

The new language, @deftech{Asm-pred-lang-v6/assignments}, is the familiar output
of our register allocation process, which has all abstract locations assigned to
physical locations.

@bettergrammar*-diff[#:include (info) asm-pred-lang-v6/spilled asm-pred-lang-v6/assignments]

After assigning frame variables, we can discard all the assorted info fields and
keep only the @asm-pred-lang-v6/assignments[assignment].

@nested[#:style 'inset
@defproc[(assign-frame-variables (p asm-pred-lang-v6/spilled?))
         asm-pred-lang-v6/assignments?]{
Compiles @tech{Asm-pred-lang-v6/spilled} to @tech{Asm-pred-lang-v6/assignments}
by allocating all abstract locations in the locals set to free frame locations.
}]

Finally, we actually replace @ch2-tech{abstract locations} with
@ch2-tech{physical locations}.
Below we define @deftech{Nested-asm-lang-fvars v6}, typeset with differences compared
to @ch5-tech{Nested-asm-lang v5}.

@bettergrammar*-ndiff[
#:labels ("v5 Diff (excerpts)" "vs Source Diff (excerpts)" "Full")
(#:exclude (pred triv opand trg loc reg relop int64 aloc fvar label)
 nested-asm-lang-v5 nested-asm-lang-fvars-v6)
(#:exclude (pred triv opand trg loc reg relop int64 aloc fvar label)
 asm-pred-lang-v6/assignments nested-asm-lang-fvars-v6)
(nested-asm-lang-fvars-v6)
]

We need to update the pass to handle @nested-asm-lang-fvars-v6[return-point]s.

@nested[#:style 'inset
@defproc[(replace-locations [p asm-pred-lang-v6/assignments?])
         nested-asm-lang-fvars-v6?]{
Compiles @tech{Asm-pred-lang v6/assignments} to @tech{Nested-asm-lang-fvars v6} by
replacing all @ch2-tech{abstract location} with @ch2-tech{physical locations} using the
assignment described in the @asm-pred-lang-v6/assignments[assignment] info
field.
}
]

@examples[#:eval sb
(parameterize ([current-parameter-registers '()])
  (pretty-display
   ((compose
     replace-locations
     assign-frame-variables
     assign-registers
     allocate-frames
     assign-call-undead-variables
     conflict-analysis
     undead-analysis
     uncover-locals
     select-instructions
     canonicalize-bind
     impose-calling-conventions
     sequentialize-let)
    '(module
       (define L.swap.1
         (lambda (x.1 y.2)
           (if (< y.2 x.1)
               x.1
               (let ([z.3 (call L.swap.1 y.2 x.1)])
                 z.3))))
       (call L.swap.1 1 2)))))
]

@section{Adjusting Frame Variables}
We changed the invariants on @nested-asm-lang-v6[fbp], the
@racket[current-frame-base-pointer-register], when added the @tech{stack of
frames}.
We now allow it to be incremented and decremented by an integer literal.
This affects how we implement frame variables.

Previously, the frame variables @nested-asm-lang-fvars-v6[fv1] represented the address
@paren-x64-v6[(fbp - 8)] in all contexts.
However, after now the compilation is non-trivial, as it must be aware of
increments and decrements to the @nested-asm-lang-fvars-v6[fbp].

Consider the example snippet
@racketblock[
`(begin
   (set! rbp (- rbp 8))
   (return-point L.rp.8
     (begin
       (set! rdi fv3)
       (jump L.f.1)))
   (set! rbp (+ rbp 8)))
]

In this example, the frame variable @nested-asm-lang-v6[fv3] is being passed to
the procedure @nested-asm-lang-v6[L.f.1] in a non-tail call.
@nested-asm-lang-v6[fv3] does not refer to 3rd frame variable on callee's, but
the 3rd frame variable on the caller's frame.
Since the frame is allocated prior to the return point, we need to fix-up this
index by translating frame variables relative to frame allocations introduced
around return points.

To do this, we change @racket[implement-fvars] to be aware of the current
@nested-asm-lang-v6[fbp] offset.
The simplest way to do this is to relocate @racket[implement-fvars] in the
compiler pipeline, to before @racket[expose-basic-blocks].
This allows the compiler to make use of the nesting structure of the program
while tracking changes to @nested-asm-lang-v6[fbp].

To update @racket[implement-fvars], we need to keep an accumulator of the
current offset from the base of the frame.
On entry to a block, frame variables start indexing from the base of the frame,
so the offset is 0.
So, @nested-asm-lang-v6[fv3] corresponds to @paren-x64-v6[(fbp - 24)]
(@racket[(- (* 3 (current-word-size-bytes)) 0)]).
After "pushing" or allocating a frame, such as @nested-asm-lang-v6[(set! fbp (- fbp
24))], @nested-asm-lang-v6[fv3] corresponds to @paren-x64-v6[(fbp - 0)]
(@racket[(- (* 3 (current-word-size-bytes)) 24)]).
After "popping" or deallocating a frame, such as @paren-x64-v6[(set! fbp (+ fbp
24))] @nested-asm-lang-v6[fv3] corresponds to @paren-x64-v6[(fbp - 24)] again.

@todo{Should create an example to use here and in allocate-frames.}

Recall that @nested-asm-lang-v6[fbp] is only incremented or decremented by
integer literal values, like those generated by @racket[allocate-frames].
Other assignments to @nested-asm-lang-v6[fbp] are invalid programs.
This means we don't have to consider complicated data flows into
@nested-asm-lang-v6[fbp].

The source language for @racket[implement-fvars], @deftech{Nested-asm-lang-fvars v6},
is defined below typeset with respect to @deftech{Nested-asm-lang v5}.

@bettergrammar*-diff[nested-asm-lang-v5 nested-asm-lang-fvars-v6]

The language does not change much, only adding a new @nested-asm-lang-v6[binop].

The target language simply changes @nested-asm-lang-fvars-v6[fvar]s to
@nested-asm-lang-v6[addr]s.
We define @deftech{Nested-asm-lang-v6} below.

@bettergrammar*-diff[nested-asm-lang-fvars-v6 nested-asm-lang-v6]

All languages following this pass need to be updated to use
@nested-asm-lang-v6[addr]s instead of @nested-asm-lang-fvars-v6[fvars].
This should not affect most passes.

@nested[#:style 'inset
@defproc[(implement-fvars (p nested-asm-lang-fvars-v6?))
          nested-asm-lang-v6?]{
Reifies @nested-asm-lang-fvars-v6[fvar]s into displacement mode operands.
}
]

@todo{Add a good example}

@section{Implementing Return Points}
Finally, to accommodate non-tail calls, we introduced a new abstraction: return
points.
We must now implement this abstraction.

To implement return points, we need to compile all the instructions following
the return points into labelled blocks, since that is our low-level
implementation of labels.
We lift all the instructions following the return point in to a new block, and
merge the tail implementing the call into the @block-asm-lang-v6[begin] of the caller.
Essentially, we transform:
@racketblock[
`(begin
   ,ss1 ...
   ,(return-point ,rp ,tail)
   ,ss2 ...)
]
into:
@racketblock[
`(define ,rp (begin ,ss2 ...))
`(begin
   ,ss1 ...
   ,tail)
]

This transformation is part of the @racket[expose-basic-blocks] pass, which
lifts many inline blocks into top-level explicitly labelled blocks, and should
now do the same for return points.

The target language of the transformation is @deftech{Block-pred-lang v6},
defined below as a change over @ch5-tech{Block-pred-lang v5}.

@bettergrammar*-diff[block-pred-lang-v5 block-pred-lang-v6]

There are no major differences, since we are compiling a new abstraction into an
old one.
Note that only @block-pred-lang-v6[-] has been added to the
@block-pred-lang-v6[binop]s.

@nested[#:style 'inset
@defproc[(expose-basic-blocks (p nested-asm-lang-v6?))
         block-pred-lang-v6?]{
Compile the @tech{Nested-asm-lang v6} to @tech{Block-pred-lang v6}, eliminating
all nested expressions by generate fresh basic blocks and jumps.
}
]

@examples[#:eval sb
(parameterize ([current-parameter-registers '()])
  (pretty-display
   ((compose
     expose-basic-blocks
     implement-fvars
     replace-locations
     assign-frame-variables
     assign-registers
     allocate-frames
     assign-call-undead-variables
     conflict-analysis
     undead-analysis
     uncover-locals
     select-instructions
     canonicalize-bind
     impose-calling-conventions
     sequentialize-let)
    '(module
       (define L.swap.1
         (lambda (x.1 y.2)
           (if (< y.2 x.1)
               x.1
               (let ([z.3 (call L.swap.1 y.2 x.1)])
                 z.3))))
       (call L.swap.1 1 2)))))
]

@section{Final Passes}

The only two passes that should require changes are @racket[patch-instructions]
and @racket[generate-x64].

@racket[patch-instructions] should be updated to work over
@para-asm-lang-v6[addr]s instead of @nested-asm-lang-fvars-v6[fvars]s.
This can be done by changing a few predicates.

@defproc[(patch-instructions [p para-asm-lang-v6?]) paren-x64-v6?]{
Compile the @tech{Para-asm-lang v6} to @tech{Paren-x64 v6} by patching
instructions that have no @ch1-tech{x64} analogue into to a sequence of
instructions and an auxiliary register from
@racket[current-patch-instructions-registers].
}

@racket[generate-x64] needs to be updated to generate the new
@paren-x64-v6[binop].
Ideally, there is a separate helper for generating @paren-x64-v6[binop]s, so
this is only a minimal change.

@defproc[(generate-x64 [p paren-x64-v6?])
         (and/c string? x64-instructions?)]{
Compile the @tech{Paren-x64 v6} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v6-graph" "Overview of Compiler Version 6" v6-graph]

@section{Appendix: Languages}

@declare-exporting[cpsc411/langs/v6]

@deflangs[
values-lang-v6
values-unique-lang-v6
proc-imp-mf-lang-v6
imp-mf-lang-v6
imp-cmf-lang-v6
asm-pred-lang-v6
asm-pred-lang-v6/locals
asm-pred-lang-v6/undead
asm-pred-lang-v6/conflicts
asm-pred-lang-v6/pre-framed
asm-pred-lang-v6/framed
asm-pred-lang-v6/spilled
asm-pred-lang-v6/assignments
nested-asm-lang-fvars-v6
nested-asm-lang-v6
block-pred-lang-v6
block-asm-lang-v6
para-asm-lang-v6
paren-x64-v6
paren-x64-rt-v6
]
