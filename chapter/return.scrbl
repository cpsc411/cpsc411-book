#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label racket/match racket/function cpsc411/info-lib)
  (for-label cpsc411/reference/a6-solution)
  (for-label (prefix-in v5: cpsc411/reference/a5-solution))
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
    '(require racket/function racket/match racket/pretty
              cpsc411/info-lib
              cpsc411/reference/a6-solution cpsc411/compiler-lib)))

@define[v6-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

L0 [label="Values-lang v6"];
L1 [label="Values-unique-lang v6"];
L3 [label="Imp-mf-lang v6"];
L4 [label="Imp-cmf-lang v6"];
L2 [label="Proc-imp-cmf-lang v6"];
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
L1 -> L3 [label=" sequentialize-let"];
L3 -> L2 [label=" normalize-bind"];
L2 -> L4 [label=" impose-calling-conventions"]
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
Thankfully, we already have the low-level abstractions required to implement
this: labels, jumps, and a calling convention.
All we need to do is slightly generalize the calling convention to introduce a
new label at the point to which a @ch5-tech{procedure} @ch5-tech{call} must
@tech{return} after performing its effect or computing a value, store that label
somewhere, and arrange for the caller to jump back to that label.

@digression{
We might think we could pre-process @tech{Values-lang v6} to lift all non-tail
calls into new procedures and rewrite the program to have only tail calls.
This is possible, but would correspond to a CPS transformation.
It would also be difficult to implement without the @ch9-tech{closure}
abstraction, which we do not have yet, and difficult to optimize, particularly
at this level of abstraction.
Each non-tail call would introduce an entire procedure call setup to the "rest"
of the body, as a continuation.
Any parameters live across the non-tail call would need to be packaged and
explicitly passed as arguments to the continuation.
By creating a @tech{return point} abstraction, later in the compiler when we
have access to lower-level abstraction (basic blocks), we'll instead be able to
generate a single jump back to this @tech{return point}, instead of a whole
@ch5-tech{procedure} @ch5-tech{call}.
}

@section{Designing a source language with return}
Below, we define @deftech{Values-lang v6} by extending @ch5-tech{Values-lang-v5}
with a @tech{return}, and with @ch5-tech{calls} in arbitrary contexts.

@bettergrammar*-ndiff[
#:labels ("Diff" "Values-lang v6")
(#:exclude (pred triv relop int64 x) values-lang-v5 values-lang-v6)
(values-lang-v6)
]

The major syntactic change is the addition of @values-lang-v6[(call triv triv
...)] in @values-lang-v6[value] context.
The implementation of this requires a semantic change to call, to ensure it
@tech{returns}.
Otherwise, an expression such as @values-lang-v6[(let ([x (call f 5)]) (+ 1 x))]
would return the value @values-lang-v6[x], and not @values-lang-v6[(+ 1 x)] as
intended.

Note that @values-lang-v6[tail] and @values-lang-v6[value] context now coincide.
We do not collapse them yet, as imposing a distinction will let us
systematically derive a compiler design that transforms @ch5-tech{tail calls}
(which need not @tech{return}) separately from @deftech{non-tail calls}, @ie
@ch5-tech{calls} in any context other than @values-lang-v6[tail] context.
This will let us maintain the performance characteristics of @ch5-tech{tail
calls}, namely that they use a constant amount of stack space, and do not need
to jump after their computation is complete.

We also add subtraction as a @values-lang-v6[binop].
This week, we start to need subtraction a lot more and it's tiring to encode it
when @ch1-tech{x64} supports it.
This requires almost no changes to the compiler if it parameterized by the set
of @values-lang-v6[binop]s.


@section{The Stack (of Frames)}
The key challenge in implementing @tech{return} has little to do with
implementing control flow---tweaking the the calling convention add labels and
jumps and a convention on how use them is pretty easy.
The hard part is how to keep track of all the values of variables that are live
after a call.

For example, consider the following @tech{Values-lang v6} program:
@values-lang-v6-block[
#:datum-literals (f z y x)
(module
  ....
  (define f (lambda (x) ....))
  (let ([z 6]
        [y (call f 5)])
    (+ z y)))
]
The value of @values-lang-v6[#:datum-literals (z) z] is needed after the
@tech{non-tail call} to @values-lang-v6[#:datum-literals (f) f].
To which @ch2-tech{physical location} can we assign
@values-lang-v6[#:datum-literals (z) z] to ensure its value is not overwritten
after the call?

Locally, we have no way of knowing.
@values-lang-v6[#:datum-literals (f) f] could overwrite any register and any
@ch2-tech{frame variable}.
Recall that our allocator assumes by the end of a block, nothing is live.
It may overwrite any register not otherwise reserved.
It also starts counting from @ch2-tech{frame variable}
@imp-cmf-lang-v6[#:datum-literals (fv0) fv0] when spilling locations, and could,
in principle, use any number of @ch2-tech{frame variables}, so the caller has no
way of picking a @ch2-tech{frame variable} with a high enough index that is
safe.
To make matters worse, the callee itself could execute many @tech{non-tail
calls}, and have its own values it needs to retain until they @tech{return}, and
so on recursively.

We do have one such location where values are safe, although we have not used or
even consider it: negatively-indexed locations on the stack, relative to the
callee.
The allocator always starts indexing new @ch2-tech{frame variables} from 0 at
@paren-x64-v6[(rbp - 0)]; if we hide the values that are live across a
@tech{non-tail calls} at, say, @paren-x64-v6[(rbp + 8)], @paren-x64-v6[(rbp +
16)], and so on, they would be safe.
(Recall the stack grows downwards, decrementing from the base pointer, so
accessing prior values is implementing by incrementing from the base pointer.)

@tabular[
#:style 'boxed
#:row-properties '(border)
`((,(paren-x64-v6 (rbp - n*8)) "↑")
  ("..." "↑")
  (,(paren-x64-v6 (rbp - 8)) "↑")
  (,(paren-x64-v6 (rbp - 0)) "Might be overwritten")
  (,(paren-x64-v6 (rbp + 8)) "Allocator cannot overwrite")
  (,(paren-x64-v6 (rbp + 16)) "↓")
  ("..." "↓")
  (,(paren-x64-v6 (rbp + m*8)) "↓"))
]

We cannot directly express these backwards offsets on the stack since
our @ch2-tech{frame variables} use natural indexes, and even if we could, this
alone does not handle the general case, where the callee has further
@tech{non-tail calls}.
But it gives us the core of the idea: we want to hide our values on the stack,
but at a location before the 0-index from which the callee will start
counting.

We do this by @emph{pushing} the callers's @tech{frame} onto the stack prior to
a @tech{non-tail call}.
A @deftech{frame} is a @ch5-tech{procedure}'s set of @ch2-tech{frame variables}
needed after a @tech{non-tail call}.
We arrange that all values live after a @tech{non-tail call} are stored in
@ch2-tech{frame variables}, so they are automatically saved by pushing the
@tech{frame} onto the stack.
We push the @tech{frame} by incrementing the frame base pointer past the last
@ch2-tech{frame variable}.
This resets the 0-index for the caller, hiding the caller's @ch2-tech{frame
variables} from the callee, so from the callee's perspective, all
@ch2-tech{frame variables} starting at @asm-pred-lang-v6[#:datum-literals (fv0)
fv0] are unused.
After returning from a call, we @emph{pop} the caller's @tech{frame} from the
stack by decrementing the frame base pointer back to its original value.
This handles the general case: every @tech{non-tail call} pushes a @tech{frame},
create a new one for the callee and saving the caller's values on the stack, and
each @ch5-tech{procedure} accesses its own @tech{frame} starting from
@ch2-tech{frame variable} @asm-pred-lang-v6[#:datum-literals (fv0) fv0], the
same as before.
This means our compiler users the stack not to push and pop values, but to push
and pop @tech{frames}: it is a stack of @tech{frames}.

It's only when we push a @tech{frame} that stack space is allocated.
Until then, the stack space is essentially treated as temporary, reclaimed at
the end of a @ch5-tech{procedure}---a @ch5-tech{tail call} is free to overwrite
everything starting from the frame base pointer.

Implementing @tech{frame} allocation is our core challenge in adding
@tech{non-tail calls}.
The caller in a @tech{non-tail call} will need to access both its own and the
callee's @tech{frame}, since it may need to pass some arguments on the stack as
part of the callee's @tech{frame}.
We will also need to determine how large the caller's @tech{frame} is at a
@tech{non-tail call}, so we know how many words to increment the frame base
pointer in order to implement pushing and popping a @tech{frame} to and from the
stack.

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
When generating code for a procedure, we cannot know in general (Rice's Theorem)
whether it will need to return or not, @ie whether it will be called via a
@ch5-tech{tail call} or @tech{non-tail call}.
We therefore design our calling convention to enable any procedure to
@tech{return}.

We modify our @ch5-tech{calling convention}, designating a register
@racket[current-return-address-register] in which to pass the
@deftech{return address}, the label for the instruction following a
@tech{non-tail call}.
By default, we use @racket[(unsyntax (current-return-address-register))]
as the @racket[current-return-address-register].
Every @ch5-tech{procedure} will jump to this @tech{return address} after it is
finish executing.
On entry to any @ch5-tech{procedure}, we load the
@racket[current-return-address-register] into a fresh @ch2-tech{abstract
location}, which we call @racket[tmp-ra].
This is necessary since the @racket[current-return-address-register] needs to be
available immediately for any new calls, but we will not need the
@tech{return address} until the end of the procedure.

To clarify this idea of @emph{on entry}, we introduce a contextual distinction
between the first @proc-imp-cmf-lang-v6[tail], and other
@proc-imp-cmf-lang-v6[tail]s in the syntax of the source language.
An @deftech{entry point} is the top-level @proc-imp-cmf-lang-v6[tail] expression
that begins execution of code, and each @tech{entry point} must load the
@racket[current-return-address-register], preserve its value, and @tech{return}
to it when finished.
This happens either as the body of a @ch5-tech{procedure}, or as the initial
@proc-imp-cmf-lang-v6[tail] in a module @proc-imp-cmf-lang-v6[(module tail)].
@bettergrammar*-ndiff[
#:labels ("Proc-imp-mf-lang v6 (excerpt)")
(#:include (p tail entry) proc-imp-cmf-lang-v6)
]

@digression{
This generalization lets us treat all @tech{returns}, including returning the
final value to the run-time system, uniformly, and allows us to eliminate the
@asm-pred-lang-v5[halt] instruction.
This isn't necessary; we could continue to support
@asm-pred-lang-v5[halt] and treat the module-level @tech{entry point} separate
from @ch5-tech{procedures}.
However, there is no benefit to doing so, and it clutters the compiler with
special cases.
The only benefit of keeping @asm-pred-lang-v5[halt] would be saving a single jump
instruction, but this has almost no cost, will probably be predicted by a CPU's
branch predictor, and could easily be optimized away by a small pass nearly
anywhere in the compiler pipeline.
}

Second, we need to a way to introduce a @tech{return address} at a
@tech{non-tail call}, so we can actually return to the middle of a computation.
Since we do not have access to labels @ch5-tech{Imp-mf-lang v5}, we need to
introduce an abstraction for creating a @tech{return address} in our new
intermediate language.
We introduce a @deftech{return point}, which creates a @imp-cmf-lang-v6[label]
for a @imp-cmf-lang-v6[tail] in @imp-cmf-lang-v6[effect] context, so the
@imp-cmf-lang-v6[effect] position for this language is:
@bettergrammar*-ndiff[
#:labels ("Imp-cmf-lang v6 (excerpt)")
(#:include (effect) imp-cmf-lang-v6)
]
A @tech{return point} contains a @imp-cmf-lang-v6[tail], since that computation
"ends" locally, jumping elsewhere, but is an @imp-cmf-lang-v6[effect], since it
updates the state of the machine to return to control this location.
After returning, the code at the @tech{return address} will read from a
designated register and continue the rest of the computation.
We reuse the @racket[current-return-value-register] as this designated register.

Third, we need to explicitly return a value in @imp-cmf-lang-v6[tail] position.
Previously, the final value in @imp-cmf-lang-v6[tail] position was also the final
value of the program.
This value was implicitly returned to the run-time system.
However, now, a value in @imp-cmf-lang-v6[tail] position may either be returned
to the run-time system, or may be returned to some other computation from a
non-tail call.
We transform a value in @imp-cmf-lang-v6[tail] position by moving it into the
@racket[current-return-value-register], and jumping to the @tech{return
address} stored in @racket[tmp-ra].

Finally, when setting up a @tech{non-tail call}, we must ensure that arguments
are placed on the callee's @tech{frame} instead of the caller's @tech{frame}.
Recall that our @ch5-tech{calling convention} passes @ch5-tech{parameters} in
@ch2-tech{frame variables} when they don't fit in registers.

Making this all concrete, we implement our new calling convention with the
following transformations:

@itemlist[
@item{When transforming an @tech{entry point} @proc-imp-cmf-lang-v6[entry], we
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

@item{When transforming a @tech{return}, @ie, a base @imp-mf-lang-v6[value]
(either a @imp-mf-lang-v6[triv] or @imp-mf-lang-v6[(binop opand opand)])
@imp-mf-lang-v6[value] in @imp-mf-lang-v6[tail] position we generate:
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

@; Shouldn't be needed now since normalize-bind happens before
@;Note that our non-tail calls can appear in value position, for example, as the
@;right-hand side of a @imp-mf-lang-v6[(set! aloc value)] instruction.
@;Recall that after the non-tail call, the return value of the procedure
@;will be stored in @racket[(current-return-value-register)].
@;When normalizing the @imp-mf-lang-v6[set!] instructions
@;@racket[normalize-bind], we can translate this instruction as:
@;@racketblock[
@;`(begin
@;   ,translation-of-non-tail-call
@;   (set! ,aloc ,rv))
@;]
}
@; TODO: Need to introduce these in the discussion above, instead of here.
@item{@racket[nfv_0 _... nfv_k-1] should be the first @racket[k] @ch2-tech{frame
variables} on the @emph{callee's} @tech{frame}.

However, we can't actually use @ch2-tech{frame variables} yet.
These variables are assigned in the caller, but must be assigned to the callee's
@tech{frame}, and we don't yet know how large the caller's @tech{frame} is.

Consider the following example:
@imp-cmf-lang-v6-block[
#:datum-literals (L.rp.2 L.label.1 x.1 nfv.2)
(begin
  (set! fv0 1)
  (set! x.1 42)
  (return-point L.rp.2
    (set! nfv.2 x.1)
    (set! r15 L.rp.2)
    (jump L.label.1 rbp r15 fv0))
  (set! r9 fv0)
  (set! rax (+ rax r9)))
]
For this example, suppose we've exhausted our
@racket[(current-parameter-registers)], which we simulate by making it the empty
set, so the argument @imp-cmf-lang-v6[x.1] must be passed on the stack.

For a @ch5-tech{tail call}, we would have generated
@imp-cmf-lang-v6[#:datum-literals (x.1) (set! fv0 x.1)].
But @imp-cmf-lang-v6[fv0] is already in use, on the caller's @tech{frame}, and
is live across the call.
We would need to at least use @imp-cmf-lang-v6[fv1], which we know will be
@imp-cmf-lang-v6[fv0] for the callee.

Figuring how exactly where the end of the caller's @tech{frame} is, and thus
which index to start using for passing arguments on the callee's @tech{frame},
is tricky.
Thus we leave it for a separate pass.
For now, we simply record the fact that @imp-cmf-lang-v6[nfv.2] is a
@deftech{new frame variable}, which must be allocated not on the current
caller's @tech{frame}, but on the new @tech{frame} created for the callee.
Some later pass will be responsible for computing @tech{frame} sizes and
allocating all the @tech{new frame variables} on the appropriate @tech{frame}.

We record the @tech{new frame variables} in a @imp-cmf-lang-v6[info]
field, @imp-cmf-lang-v6[(new-frames (frame ...))].
The field is a list of @tech{frames} created for each @tech{non-tail call}, and
each @imp-cmf-lang-v6[frame] is a list of @tech{new frame variables} that the
caller will put, in order, on the callee's @tech{frame}.

Note that this is only a problem in a @tech{non-tail call}.
In a tail call, we can reuse the caller's @tech{frame} as the callee's
@tech{frame}, since we never @tech{return}.

@;{
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
}

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
@values-lang-v6[binop] and @values-lang-v6[relop] are never used on procedures.
}
}

Next, we extend @racket[uniquify].
First, of course, we design the updated @deftech{Values-unique-lang v6}.
We typeset the differences with respect to @ch5-tech{Values-unique-lang v5}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5 (excerpts)" "Diff vs Source" "Values-unique-lang v6")
(#:exclude (pred tail opand triv relop aloc label int64) values-unique-lang-v5 values-unique-lang-v6)
(values-lang-v6 values-unique-lang-v6)
(values-unique-lang-v6)
]

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
Below we define the target language, @deftech{Imp-mf-lang v6}, where we
transform lexical binding into sequential imperative assignments.

@bettergrammar*-ndiff[
#:labels ("v5 Diff (excerpts)" "Diff vs Source" "Imp-mf-lang v6")
(#:exclude (int64 triv opand relop int64 aloc label) imp-mf-lang-v5 imp-mf-lang-v6)
(values-unique-lang-v6 imp-mf-lang-v6)
(imp-mf-lang-v6)
]

Note that this language contains a definition @proc-imp-cmf-lang-v6[entry]
designating the top-level tail used as the @tech{entry point} for each
@ch5-tech{procedure} and for the module as a whole.
There is no syntactic distinction, but making a semantic distinction will
simplify our implementation of the @ch5-tech{calling convention} to support
@tech{return}.

@todo{Kent adds non-tail calls in effect context in this assignment. We could
add it here for future-proofing, but probably not important to do so. It's not
exposed in the surface yet.}

@nested[#:style 'inset
@defproc[(sequentialize-let (p values-unique-lang-v6?))
         imp-mf-lang-v6?]{
Compiles @tech{Values-unique-lang v6} to @tech{Imp-mf-lang v6} by picking a
particular order to implement @values-unique-lang-v6[let] expressions using
@imp-mf-lang-v6[set!].
}
]

Finally, we update @racket[normalize-bind].
Below we design the target language @deftech{Proc-imp-cmf-lang v6},
typeset with differences compared to @ch5-tech{Proc-imp-cmf-lang v5}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Proc-imp-cmf-lang v6")
(proc-imp-cmf-lang-v5 proc-imp-cmf-lang-v6)
(imp-mf-lang-v5 proc-imp-cmf-lang-v6)
(imp-mf-lang-v6)
]

We simply extend @ch5-tech{Proc-imp-cmf-lang v5} with our new abstractions, including
@tech{non-tail calls}.

@nested[#:style 'inset
@defproc[(normalize-bind (p imp-mf-lang-v6?))
          proc-imp-cmf-lang-v6?]{
Compiles @tech{Imp-mf-lang v6} to @tech{Proc-imp-cmf-lang v6}, pushing
@imp-mf-lang-v6[set!] under @imp-mf-lang-v6[begin] so that the right-hand-side
of each @imp-mf-lang-v6[set!] is base value-producing operation.

This normalizes @tech{Imp-mf-lang v6} with respect to the equations:
@tabular[
#:sep @hspace[3]
(list
 (list
  @imp-mf-lang-v6-block0[(set! aloc
                               (begin effect_1 ...
                                      value))]
  "="
  @imp-mf-lang-v6-block0[(begin effect_1 ...
                                (set! aloc value))])
 (list
  @imp-mf-lang-v6-block0[(set! aloc
                               (if pred
                                   value_1
                                   value_2))]
  "="
  @imp-mf-lang-v6-block0[(if pred
                             (set! aloc value_1)
                             (set! aloc value_2))]))]}]

@section{Extending Calling Convention}
Now we design @deftech{Imp-cmf-lang-v6}, the target language of our calling
convention translation.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Imp-cmf-lang v6")
(#:exclude (relop trg loc triv opand aloc) imp-cmf-lang-v5 imp-cmf-lang-v6)
(#:exclude (relop binop label aloc) proc-imp-cmf-lang-v6 imp-cmf-lang-v6)
(imp-cmf-lang-v6)
]

@todo{Jump disappears from value context, transformed into effect context.
Should point that out.. and double check.}

Compared to @ch5-tech{Imp-cmf-lang v5}, the main difference is the
@imp-cmf-lang-v6[return-point] form in effect context.
This instruction introduces a new, non-top-level @imp-cmf-lang-v6[label] in the
middle of an instruction sequence, which is expected to be exclusively used by
the @ch5-tech{calling convention} to implement @tech{return}.
We further assume that a @imp-cmf-lang-v6[return-point] cannot appear inside
another @imp-cmf-lang-v6[return-point], @ie there are no nested
@imp-cmf-lang-v6[return-point]s.
Our compiler can never generate this code, and there is no reason to support it.

The implicit return value, @imp-cmf-lang-v6[value] in @imp-cmf-lang-v6[tail]
position, is no longer valid.
Instead, the run-time system will set the first return address, and the final
result of any computation is explicitly returned to the run-time system using
the @ch5-tech{calling conventions}.
The run-time system initializes the @racket[current-return-address-register] to
be the address of the exit procedure.

To implement @tech{return}, we modify every @tech{entry point} to store the
@racket[current-return-address-register] as described by our calling convention.
Then we explicitly @tech{return} the base expressions in @imp-cmf-lang-v6[value]
context by jumping to that return address.

@; TODO why is this here?
@;{
To implement @imp-cmf-lang-v6[fvar]s later, we require that
@racket[current-frame-base-pointer-register] is assigned only by
incrementing or decrementing it by an integer literal.
Other uses @racket[current-frame-base-pointer-register] are @emph{invalid
programs}.
Later passes will assume this in order to compute frame variable locations.
}

The @imp-cmf-lang-v6[new-frames] @imp-cmf-lang-v6[info] field records all the
new @tech{frames} created in the block, and will be used later to push new
@tech{frames} on to the stack, and assign @tech{new frame variables} to actual
@ch2-tech{frame variables}.
There should be one frame for each @tech{non-tail call}, even if that
@tech{frame} is empty.
The @tech{new frame variables} should be in order.
Each @tech{new frame variable} must only appear in one @tech{frame} in the
@imp-cmf-lang-v6[new-frames] field.
Recall that @imp-cmf-lang-v6[aloc]s are unique to a scope, and the
@imp-cmf-lang-v6[new-frames] field represents newly defined
@imp-cmf-lang-v6[aloc]s.
It would not make sense for the same @imp-cmf-lang-v6[aloc] to appear in two
@tech{frames}.

@nested[#:style 'inset
@defproc[(impose-calling-conventions [p proc-imp-cmf-lang-v6?]) imp-cmf-lang-v6?]{
Compiles @tech{Proc-imp-cmf-lang v6} to @tech{Imp-cmf-lang v6} by imposing calling
conventions on all calls (both tail and non-tail calls), and @tech{entry
points}.
The registers used to passing parameters are defined by
@racket[current-parameter-registers], and the registers used for returning are
defined by @racket[current-return-address-register] and
@racket[current-return-value-register].
}
]

After implementing the @ch5-tech{calling conventions}, we have two abstractions that
we need to implement.

First, we must allocate @tech{frames}.
We've already collected the @tech{new frame variables}, so we need to modify the
allocator to determine the size of each @tech{frame} and allocate them on the
stack.
This requires explicitly manipulating the frame base pointer, which also changes
how @ch2-tech{frame variables} are implemented.

Second, we need to implement @imp-mf-lang-v6[return-point]s.
These will be compiled to raw labels, so we essentially preserve them until a
low-level language with access to raw labels.

@section{Implementing A Stack of Frames}

@;TODO This comment belongs somewhere
@;{
This usage of @tech{stack} differs from what @ch1-tech{x64} provides.
We now require the @tech{stack} to be frame-aligned, and any accesses outside
the current frame boundaries results in undefined behaviour.
}

The first new abstraction we must implement is @tech{frame} allocation.
We have two needs to allocate frames.
First, we need to know how large the @tech{frame} is, so we can actually
allocate that much space.
To compute the size of each caller's @tech{frame}, we need to know how many
variables might be live across a @tech{non-tail call}, so this must wait until
after @racket[undead-analysis].
Second, to minimize the space required, we want to make sure we wait to allocate
until @racket[conflict-analysis].
Otherwise, we might have to assume that all live @ch2-tech{abstract locations}
are unique @ch2-tech{frame variables}, increasing the size of the @tech{frame}.
By waiting until after @racket[conflict-analysis], so we can try to assign
non-conflicting variables to the same @ch2-tech{frame variable}.

@subsection{Updating intermediate passes}
We begin by updating the passes through @racket[conflict-analysis].

The next pass in the sequence is @racket[select-instructions].
We define @deftech{Asm-pred-lang v6} below, with changes typeset with respect to
@ch5-tech{Asm-pred-lang v5}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Asm-pred-lang v6")
(#:exclude (aloc label rloc relop trg loc triv opand int64) asm-pred-lang-v5 asm-pred-lang-v6)
(#:exclude (aloc label rloc relop trg triv int64 binop) imp-cmf-lang-v6 asm-pred-lang-v6)
(asm-pred-lang-v6)
]

There are no new restrictions for @asm-pred-lang-v6[return-point], so we just
add support for @asm-pred-lang-v6[return-point], and drop @asm-pred-lang-v6[halt].

@nested[#:style 'inset
@defproc[(select-instructions (p imp-cmf-lang-v6?))
         asm-pred-lang-v6?]{
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

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Asm-pred-lang-v6/locals")
(#:exclude (pred opand triv loc trg relop aloc label rloc) asm-pred-lang-v5/locals asm-pred-lang-v6/locals)
(#:exclude (pred opand triv loc trg relop aloc label rloc) asm-pred-lang-v6 asm-pred-lang-v6/locals)
(asm-pred-lang-v6/locals)
]

Updating this analysis is not complicated.
We simply add a case to handle @asm-pred-lang-v6[return-point]s.
Remember that the "arguments" to @asm-pred-lang-v6[jump] are only used for later
analyses and not considered locals for this analysis.

The @tech{new frame variables} are considered locals, so will be part of the
analysis.
The locals set is the set of unassigned variables, so we add variables to it
that are unassigned, and remove variables from the set as they are assigned.
So @tech{new frame variables} are listed in the locals set for the enclosing
block, until they are assigend @ch2-tech{frame variables}.

@nested[#:style 'inset
@defproc[(uncover-locals (p asm-pred-lang-v6?))
          asm-pred-lang-v6/locals?]{
Compiles @tech{Asm-pred-lang v6} to @tech{Asm-pred-lang v6/locals}, analysing
which @ch2-tech{abstract locations} are used in each block, and each block and
the module with the set of variables in an @racket[info?] fields.
}
]

Next we extend @racket[undead-analysis].
We design @deftech{Asm-pred-lang-v6/undead} below, typeset with respect to
@ch5-tech{Asm-pred-lang v5/undead}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Asm-pred-lang v6/undead")
(#:exclude (pred opand triv loc trg relop aloc label rloc int64) asm-pred-lang-v5/undead asm-pred-lang-v6/undead)
(#:exclude (pred opand triv loc trg relop aloc label rloc int64) asm-pred-lang-v6/locals asm-pred-lang-v6/undead)
(asm-pred-lang-v6/undead)
]

We add two new @asm-pred-lang-v6[info] fields:
@asm-pred-lang-v6/undead[undead-out] and @asm-pred-lang-v6/undead[call-undead].
The @asm-pred-lang-v6/undead[undead-out] field continues to store the
@ch-ra-tech{undead-set tree}, which we must update to follow the structure of
@asm-pred-lang-v6[return-point]s.
We also record information about what is undead across a @tech{non-tail call},
the @asm-pred-lang-v6/undead[call-undead] set.
This is the set of all variables that are undead after @emph{any} @tech{non-tail
call} in a block; there is a single set for the entire block, even when there
are multiple @tech{non-tail calls}.

The @asm-pred-lang-v6/undead[call-undead] field stores @emph{every}
@ch2-tech{abstract location} or @ch2-tech{frame variable} that is in the
undead-out set of a @asm-pred-lang-v6[return-point].
The @ch2-tech{abstract locations} must be allocated to the @tech{frame}, as
discussed above, and both these @ch2-tech{abstract locations} and the
@ch2-tech{frame variables} are requried to compute the size of the @tech{frame}.

First, we update the definition of @racket[undead-set-tree?]
to handle the @asm-pred-lang-v6[return-point] instruction, which includes a
nested @asm-pred-lang-v6[tail].
@todo{Since the predicate changes, probably need to add multiple definitions to
cpsc411-lib.}

Our new @deftech{undead-set tree} is one of:
@itemlist[
@item{an @ch-ra-tech{undead-out set} @asm-pred-lang-v6[(aloc ...)], corresponding to the
@ch-ra-tech{undead-out set} for a single instruction such as
@asm-pred-lang-v6[(set! aloc triv)].}

@item{a list of @tech{undead-set tree}s, @asm-pred-lang-v6[(undead-set-tree?_1
... undead-set-tree?_2)], corresponding to a @asm-pred-lang-v6[begin] statement
@asm-pred-lang-v6[(begin effect_1 ... effect_2)] or @asm-pred-lang-v6[(begin
effect_1 ... tail)].
The first element of the list represents @tech{undead-set tree} for the first
@asm-pred-lang-v6[effect], the second element represents the @tech{undead-set tree}
for the second @asm-pred-lang-v6[effect], and so on.
}

@item{a list of exactly three @tech{undead-set tree}s,
@asm-pred-lang-v6[(undead-set-tree?_p undead-set-tree?_1 undead-set-tree?_2)],
corresponding to a @asm-pred-lang-v6[if] statement @asm-pred-lang-v6[(if pred
tail_1 tail_2)].
@asm-pred-lang-v6[undead-set-tree?_p] corresponds to the @tech{undead-set tree}
of @asm-pred-lang-v6[pred], while @asm-pred-lang-v6[undead-set-tree?_1]
corresponds to @asm-pred-lang-v6[tail_1] and
@asm-pred-lang-v6[undead-set-tree?_2] corresponds to @asm-pred-lang-v6[tail_2].
}

@item{a list containing an @ch-ra-tech{undead-out set} and one @tech{undead-set tree},
@asm-pred-lang-v6[(undead-set undead-set-tree?)], corresponding to a
@asm-pred-lang-v6[return-point] statement @asm-pred-lang-v6[(return-point label
tail)].
The @asm-pred-lang-v6[undead-set] is the @ch-ra-tech{undead-out set} for the
@asm-pred-lang-v6[return-point], and the @asm-pred-lang-v6[undead-set-tree?]
in the list is the @tech{undead-set tree} for the @asm-pred-lang-v6[tail] of the
@asm-pred-lang-v6[return-point].
}
]

@;Analyzing non-tail jumps is no different from other jumps; we reuse the
@;"arguments" annotated on the jump as the undead-out set, and discard the
@;annotation.

Analyzing @asm-pred-lang-v6[return-point] requires making explicit a fact from
our calling convention.
After returning, we expect @racket[current-return-value-register] to be live.
We model this as treating a @asm-pred-lang-v6[return-point] as defining the
@racket[current-return-value-register].

@nested[#:style 'inset
@defproc[(undead-analysis (p asm-pred-lang-v6/locals?))
          asm-pred-lang-v6/undead?]{
Performs undead analysis, compiling @tech{Asm-pred-lang v6/locals} to
@tech{Asm-pred-lang v6/undead} by decorating programs with their
@ch-ra-tech{undead-set trees}.
}
]

Below are examples of compiling a program that swaps its inputs, one using the
normal allocator and one using an empty set of assignable registers.
@examples[#:eval sb
((compose
  undead-analysis
  uncover-locals
  select-instructions
  impose-calling-conventions
  normalize-bind
  sequentialize-let)
 '(module
    (define L.swap.1
      (lambda (x.1 y.2)
        (if (< y.2 x.1)
            x.1
            (let ([z.3 (call L.swap.1 y.2 x.1)])
              z.3))))
    (call L.swap.1 1 2)))

(parameterize ([current-parameter-registers '()])
  ((compose
    undead-analysis
    uncover-locals
    select-instructions
    impose-calling-conventions
    normalize-bind
    sequentialize-let)
   '(module
      (define L.swap.1
        (lambda (x.1 y.2)
          (if (< y.2 x.1)
              x.1
              (let ([z.3 (call L.swap.1 y.2 x.1)])
                z.3))))
      (call L.swap.1 1 2))))

]

The following example shows the output on an intermediate representation of
non-tail recursive factorial, compiled with @racket[current-parameter-registers]
set to @racket['()] to force the compiler to generate @ch2-tech{frame variables}
and test edge cases.
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

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs v6" "Asm-pred-lang v6/conflicts")
(#:exclude (pred opand triv loc trg relop aloc label rloc int64) asm-pred-lang-v5/conflicts asm-pred-lang-v6/conflicts)
(#:exclude (pred opand triv loc trg relop aloc label rloc int64) asm-pred-lang-v6/undead asm-pred-lang-v6/conflicts)
(asm-pred-lang-v6/conflicts)
]

Recall that @racket[current-return-value-register] is assigned by a non-tail call.
Also note that @racket[current-frame-base-pointer-register] and
@racket[current-return-value-register] are likely to end up in conflict with
everything, even though we have removed them from the
@racket[current-assignable-registers] set.

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

The size of a frame @racket[n] (in slots) for a given @tech{non-tail call} is
the maximum of:
@itemlist[
@item{the number of locations in the @asm-pred-lang-v6[call-undead], or}

@item{one more than the index of the largest frame location in the
@asm-pred-lang-v6[call-undead] set.}
]
So far, it's unlikely that the current compiler implementation will ever create
a program with a @ch2-tech{frame variable} live after a @tech{non-tail call},
since register allocation hasn't happened yet.
However, we will see shortly an example of a @asm-pred-lang-v6[call-undead]
variables assigned a @ch2-tech{frame variable} with an index greater than the
size of the @asm-pred-lang-v6[call-undead] set.

@margin-note{In practice, calling conventions distinguish between two sets of
registers: callee-saved and caller-saved, to allow some registers to be live
across a call.
We ignore this for simplicity and assume all registers are caller-saved.}

To allocate a frame, intuitively, we want to transform @racket[`(return-point
,rp ,tail)] into:
@racketblock[
`(begin
   (set! ,fv_0 ,x_0)
   ...
   (set! ,fv_n-1 ,x_n-1)

   (set! ,fbp (- ,fbp ,nb))
   (return-point ,rp ,tail)
   (set! ,fbp (+ ,fbp ,nb))

   (set! ,x_0 ,fv_0 )
   ...
   (set! ,x_n-1 ,fv_n-1))
]
where:
@itemlist[

@item{@racket[nb] is the number of bytes required to save @racket[n] slots on
the frame, @ie @racket[(* n (current-word-size-bytes))].}

@item{@racket[fbp] is the value of the parameter
@racket[current-frame-base-pointer-register].}

@item{@racket[x_0], ... @racket[x_n] are the locations in the
live after the @tech{non-tail call}, which we approximate with the
@asm-pred-lang-v6[call-undead] set.}

@item{@racket[fv_0], ... @racket[fv_n-1] are @racket[n] free @ch2-tech{frame variables}.}
]

This pushes all call-live variables onto the frame, pushes the @tech{frame},
performs the call, pops the @tech{frame}, and reads back the variables.

@;Unfortunately, there are two problems with this desired transformation.
@;
@;... conflict analysis

Unfortunately, we can't implement this transformation as is.
We want to avoid producing new @asm-pred-lang-v6[set!]s.
First, they will invalidate the undead analysis we've just performed.
Second, some or all the moves might be unnecessary.
We don't know whether those variables @racket[x_0 ... x_n-1] need to be in
registers, so it would be potentially more efficient to assign them to
@ch2-tech{frame variables} in the first place and leave some other pass to move
them into registers when necessary, rather than @emph{definitely} generate a
ton of memory accesses.

Instead, we perform this transformation in two steps.
First, a new pass @racket[assign-call-undead-variables] assigns each variable
that is live across a call to a @ch2-tech{frame variable}, instead of producing
new moves.
This produces a partial assignment of @ch2-tech{abstract locations} to
@ch2-tech{frame variables}, which the register allocator will work from.
Second, a new pass, @racket[allocate-frames] does the work of updating the frame
base pointer, effectively allocating the @tech{frame} for each @tech{non-tail call}.

We define @deftech{Asm-pred-lang-v6/pre-framed} below, typeset with changes with
respect to @tech{Asm-pred-lang-v6/conflicts}.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Asm-pred-lang v6/pre-framed")
(#:include (info) asm-pred-lang-v6/conflicts asm-pred-lang-v6/pre-framed)
(asm-pred-lang-v6/pre-framed)
]

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

An easy way to find a compatible frame variable is to find the set of
@ch2-tech{frame variables} to which @racket[x] cannot be assigned.
Then, starting from @asm-pred-lang-v6[fv0], assign the first @ch2-tech{frame
variable} that is not in the incompatible set.

Finally, add the assignment for the @racket[x] to the result of the recursive
call.
}
]

The default @asm-pred-lang-v6[assignment] for this pass is the empty
@asm-pred-lang-v6[assignment], since nothing has been assigned yet.

Since many @ch2-tech{frame variables} will be assigned prior to register allocation,
you will need to modify @racket[assign-registers] to use a similar algorithm for
spilling, instead of a naive algorithm that starts spilling at @ch2-tech{frame
variable} @asm-pred-lang-v6[fv0].

@nested[#:style 'inset
@defproc[(assign-call-undead-variables (p asm-pred-lang-v6/conflicts?))
         asm-pred-lang-v6/pre-framed?]{
Compiles @tech{Asm-pred-lang-v6/conflicts} to @tech{Asm-pred-lang-v6/pre-framed}
by pre-assigning all variables in the
@asm-pred-lang-v6/conflicts[call-undead] sets to
@ch2-tech{frame variables}.
}]

@todo{Change locals to homeless? Then when introducing the fixed point
algorithm, could name the break condition "homelessness-eliminated".}
@examples[#:eval sb
(parameterize ([current-parameter-registers '()])
  (pretty-display
   ((compose
     assign-call-undead-variables
     conflict-analysis
     undead-analysis
     uncover-locals
     select-instructions
     impose-calling-conventions
     normalize-bind
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

Notice in this example that @asm-pred-lang-v6[#:datum-literals (tmp-ra.7)
tmp-ra.7] gets assigned the @ch2-tech{frame variable} @asm-pred-lang-v6[fv2],
since it is live across the call, but in conflict with the
@asm-pred-lang-v6[fv0] and @asm-pred-lang-v6[fv1] due to the calling convention.
This means the frame will have size 3, to preserve @asm-pred-lang-v6[fv2],
despite only 1 variable being live across the call.

@margin-note{
Note that our language is flexible enough to allow us to inline
@asm-pred-lang-v6[fv2] in the @asm-pred-lang-v6[call-undead] set, or not and
allow a @racket[allocate-frames] to resolve @asm-pred-lang-v6[call-undead]
variables using the @asm-pred-lang-v6[assignment].
}

Now we can allocate @tech{frames} for each @tech{non-tail call}.
For each block, we compute the size of the @tech{frames} for @emph{all}
@tech{non-tail call}, as described earlier.

To allocate the caller's @tech{frame}, we transform @racket[`(return-point ,rp
,tail)] into
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

With the caller's @tech{frame} allocated, we also know where the callee's
@tech{frame} begins, so we also assign each of the @tech{new-frame variables}
from the @asm-pred-lang-v6/pre-framed[new-frame] lists.
In order, each @tech{new-frame variable} is assigned to a @ch2-tech{frame
variable} starting with @racket[(make-fvar n)].
These assignments are added to the @asm-pred-lang-v6/pre-framed[assignment]
field the enclosing block.

The output is @deftech{Asm-pred-lang-v6/framed}, which only changes in its
info fields compared to @tech{Asm-pred-lang-v6/pre-framed}.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Asm-pred-lang v6")
(#:include (info) asm-pred-lang-v6/pre-framed asm-pred-lang-v6/framed)
(asm-pred-lang-v6/framed)
]

The only differences are in the info field.
The call-undead sets and new-frame fields are removed.

Note that if the undead-outs were being preserved for debugging, they must be
removed after @racket[allocate-frames], since this pass invalidates the
undead-out sets.

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
     impose-calling-conventions
     normalize-bind
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
     impose-calling-conventions
     normalize-bind
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
@tech{Frames} are now implemented and all @tech{new-frame variables} and
variables live across a call are assigned to @ch2-tech{frame variables}.
We need to adjust our register allocator so that it does not try to spill
variables into @ch2-tech{frame variables} that are already taken.

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

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Asm-pred-lang v6/spilled")
(#:include (info) asm-pred-lang-v6/framed asm-pred-lang-v6/spilled)
(asm-pred-lang-v6/spilled)
]

@nested[#:style 'inset
@defproc[(assign-registers (p asm-pred-lang-v6/framed?))
          asm-pred-lang-v6/spilled?]{
Performs @ch-ra-tech{graph-colouring register allocation}, compiling
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
@ch2-tech{frame variables}.

The new language, @deftech{Asm-pred-lang-v6/assignments}, is the familiar output
of our register allocation process, which has all abstract locations assigned to
physical locations.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Asm-pred-lang v6/assignments")
(#:include (info) asm-pred-lang-v6/spilled asm-pred-lang-v6/assignments)
(asm-pred-lang-v6/assignments)
]

After assigning @ch2-tech{frame variables}, we can discard all the assorted info
fields and keep only the @asm-pred-lang-v6/assignments[assignment].

@nested[#:style 'inset
@defproc[(assign-frame-variables (p asm-pred-lang-v6/spilled?))
         asm-pred-lang-v6/assignments?]{
Compiles @tech{Asm-pred-lang-v6/spilled} to @tech{Asm-pred-lang-v6/assignments}
by allocating all abstract locations in the locals set to free @ch2-tech{frame variables}.
}]

Finally, we actually replace @ch2-tech{abstract locations} with
@ch2-tech{physical locations}.
Below we define @deftech{Nested-asm-lang-fvars v6}, typeset with differences compared
to @ch5-tech{Nested-asm-lang v5}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5 (excerpts)" "Diff vs Source (excerpts)" "Nested-asm-lang-fvars v6")
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
     impose-calling-conventions
     normalize-bind
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
@racket[current-frame-base-pointer-register], when added the stack of
@tech{frames}.
We now allow it to be incremented and decremented by an integer literal.
This affects how we implement @ch2-tech{frame variables}.

Previously, the @ch2-tech{frame variables} @nested-asm-lang-fvars-v6[fv1]
represented the address @paren-x64-v6[(fbp - 8)] in all contexts.
However, now the compilation is non-trivial, as it must be aware of
increments and decrements to the @nested-asm-lang-fvars-v6[fbp].

Consider the example snippet
@nested-asm-lang-fvars-v6-block[
#:datum-literals (L.rp.8 L.f.1)
(begin
  (set! rbp (- rbp 8))
  (return-point L.rp.8
    (begin
      (set! rdi fv3)
      (jump L.f.1)))
  (set! rbp (+ rbp 8)))
]

In this example, the frame variable @nested-asm-lang-v6[fv3] is being passed to
the procedure @nested-asm-lang-v6[#:datum-literals (L.f.1) L.f.1] in a
@tech{non-tail call}.
@nested-asm-lang-v6[fv3] does not refer to 3rd @ch2-tech{frame variable} on
callee's, but the 3rd @ch2-tech{frame variable} on the caller's @tech{frame}.
Since the @tech{frame} is allocated prior to the @tech{return point}, we need to
fix-up this index by translating @ch2-tech{frame variables} relative to frame
allocations introduced around @tech{return points}.

To do this, we change @racket[implement-fvars] to be aware of the current
@nested-asm-lang-v6[fbp] offset.
We can do this more easily by relocating @racket[implement-fvars] in the
compiler pipeline, to before @racket[expose-basic-blocks].
This allows the compiler to make use of the nesting structure of the program
while tracking changes to @nested-asm-lang-v6[fbp].

To update @racket[implement-fvars], we need to keep an accumulator of the
current offset from the base of the @tech{frame}.
On entry to a block, @ch2-tech{frame variables} start indexing from the base of
the @tech{frame}, so the offset is 0.
So, @nested-asm-lang-v6[fv3] corresponds to @paren-x64-v6[(fbp - 24)]
(@racket[(- (* 3 (current-word-size-bytes)) 0)]).
After pushing/allocating a @tech{frame}, such as @nested-asm-lang-v6[(set! fbp (- fbp
8))], @nested-asm-lang-fvars-v6[fv3] corresponds to @paren-x64-v6[(fbp - 16)]
(@racket[(+ (* 3 (current-word-size-bytes)) -8)]).
After popping/deallocating a @tech{frame}, such as @paren-x64-v6[(set! fbp (+
fbp 8))] @nested-asm-lang-fvars-v6[fv3] corresponds to @paren-x64-v6[(fbp - 24)]
(@racket[(+ (* 3 (current-word-size-bytes)) 0)]) again.

@todo{Should create an example to use here and in allocate-frames.}

Recall that @nested-asm-lang-v6[fbp] is only incremented or decremented by
integer literal values, like those generated by @racket[allocate-frames].
Other assignments to @nested-asm-lang-v6[fbp] are invalid programs.
This means we don't have to consider complicated data flows into
@nested-asm-lang-v6[fbp].

The source language for @racket[implement-fvars], @deftech{Nested-asm-lang-fvars v6},
is defined below typeset with respect to @deftech{Nested-asm-lang v5}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Nested-asm-lang-fvars v6")
(#:exclude (reg pred loc opand triv trg int64 aloc fvar label relop)
 nested-asm-lang-v5 nested-asm-lang-fvars-v6)
(nested-asm-lang-fvars-v6)
]

The language does not change much, only adding a new @nested-asm-lang-v6[binop].

The target language simply changes @nested-asm-lang-fvars-v6[fvar]s to
@nested-asm-lang-v6[addr]s.
We define @deftech{Nested-asm-lang-v6} below.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Nested-asm-lang v6")
(#:exclude (reg pred loc opand triv trg int64 aloc label relop binop)
 nested-asm-lang-fvars-v6 nested-asm-lang-v6)
(nested-asm-lang-v6)
]

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
Finally, to accommodate @tech{non-tail calls}, we introduced a new abstraction:
@tech{return points}.
We must now implement this abstraction.

To implement @tech{return points}, we need to compile all the instructions
following the @tech{return points} into labelled blocks, since that is our
low-level implementation of labels.
We lift all the instructions following the @tech{return point} into a new
block, and merge the tail implementing the call into the
@block-asm-lang-v6[begin] of the caller.
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
now do the same for @tech{return points}.

The target language of the transformation is @deftech{Block-pred-lang v6},
defined below as a change over @ch5-tech{Block-pred-lang v5}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5 (excerpts)" "Diff vs Source (excerpts)" "Block-pred-lang v6")
(#:exclude (triv opand trg int64 relop reg) block-pred-lang-v5 block-pred-lang-v6)
(#:exclude (triv opand trg int64 relop reg) nested-asm-lang-v6 block-pred-lang-v6)
(block-pred-lang-v6)
]

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
     impose-calling-conventions
     normalize-bind
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

@racket[resolve-predicates] and @racket[flatten-program] should not need any
real changes, since they don't inspect binary operations and didn't inspect at
@ch2-tech{frame variables}

We define @deftech{Block-asm-lang v6} below.
@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Block-asm-lang v6")
(#:exclude (triv opand trg reg int64 relop label) block-asm-lang-v5 block-asm-lang-v6)
(#:exclude (triv opand trg reg int64 relop label binop) block-pred-lang-v6 block-asm-lang-v6)
(block-asm-lang-v6)
]

@defproc[(resolve-predicates [p block-pred-lang-v6?])
         block-asm-lang-v6?]{
Compile the @tech{Block-pred-lang v6} to @tech{Block-asm-lang v6} by
manipulating the branches of @block-pred-lang-v6[if] statements to resolve
branches.
}

We define @deftech{Para-asm-lang v6} below.
@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Para-asm-lang v6")
(#:exclude (triv opand trg reg relop int64) para-asm-lang-v5 para-asm-lang-v6)
(#:exclude (triv opand trg loc reg int64 relop) block-asm-lang-v6 para-asm-lang-v6)
(para-asm-lang-v6)
]

@defproc[(flatten-program [p block-asm-lang-v6?])
         para-asm-lang-v6?]{
Compile @tech{Block-asm-lang v6} to @tech{Para-asm-lang v6} by flattening basic
blocks into labeled instructions.
}

The only two passes that should require changes are @racket[patch-instructions]
and @racket[generate-x64].
The languages change only in minor ways.

@deftech{Para-asm-lang v6} is defined below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Para-asm-lang v6")
(#:exclude (triv opand trg reg relop int64) para-asm-lang-v5 para-asm-lang-v6)
(#:exclude (triv opand trg loc relop int64 binop) block-asm-lang-v6 para-asm-lang-v6)
(para-asm-lang-v6)
]

@racket[patch-instructions] should be updated to work over
@para-asm-lang-v6[addr]s instead of @nested-asm-lang-fvars-v6[fvars]s.
This can be done by changing a few predicates, depending on the design.

@deftech{Paren-x64 v6} is defined below.
@bettergrammar*-ndiff[
#:labels ("Diff vs v5" "Diff vs Source" "Paren-x64 v6")
(#:exclude (trg triv opand loc reg relop) paren-x64-v5 paren-x64-v6)
(#:exclude (relop binop int64 reg) para-asm-lang-v6 paren-x64-v6)
(paren-x64-v6)
]

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

The new binary operation, @paren-x64-v6[-], corresponds to the x64 instruction
@tt{sub}, and has the same restrictions as the @tt{add} and @tt{mul} instructions.

@defproc[(generate-x64 [p paren-x64-v6?])
         (and/c string? x64-instructions?)]{
Compile the @tech{Paren-x64 v6} program into a valid sequence of @ch1-tech{x64}
instructions, represented as a string.
}

@section[#:tag "sec:overview"]{Appendix: Overview}

@figure["fig:v6-graph" "Overview of Compiler Version 6" v6-graph]
