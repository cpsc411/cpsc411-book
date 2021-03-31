#lang scribble/base

@(require
"../assignment/assignment-mlang.rkt"
scriblib/figure
(for-label cpsc411/reference/a9-solution)
(for-label (except-in cpsc411/compiler-lib compile))
cpsc411/langs/v8
cpsc411/langs/v9
(for-label cpsc411/langs/v9))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a9-solution]
@(define sb
   (make-cached-eval
    "ch9-eval"
    '(require racket/pretty cpsc411/reference/a9-solution cpsc411/compiler-lib)))

@define[v9-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

Lx [label="Exprs-lang v8"];
Ly [label="Exprs-unique-lang v8"];
Lz [label="Exprs-unsafe-data-lang v8"];
L0 [label="Exprs-bits-lang v8"];
L1 [label="Values-bits-lang v8"];
L2 [label="Proc-imp-mf-lang v8"];
L3 [label="Imp-mf-lang v8"];
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
L1 -> L2 [label=" sequentialize-let"];
L2 -> L3 [label=" impose-calling-conventions"]
L3 -> L4 [label=" canonicalize-bind"];
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

@title[#:tag "top" #:tag-prefix "chp-closures:"]{Closures: Code is Data}
@(define (v8-tech . rest)
  (apply tech #:tag-prefixes '("book:" "chp-structured-data:") rest))


@section{Preface: What's wrong with Exprs-Lang v8?}
Actually, not much.
With structured data types, @v8-tech{Exprs-lang v8} is a pretty good language
now.

@v8-tech{Exprs-bits-lang v8} is sufficiently expressive to act as a reasonable
compiler backend for many languages.
It's roughly equivalent to C, although with more curvy parens.

@v8-tech{Exprs-lang v8} adds safety on top of that language, although this
safety does come at a cost.
The main limitation in @v8-tech{Exprs-lang v8} is the lack of abstractions of computation.
We have lots of abstraction over data, but it's quite common to want to abstract
over computation---first class functions, objects, function pointers, etc.
Currently, @v8-tech{Exprs-lang v8} prevents any such abstraction for safety.
This week, we add the necessary features to enable this safely.


We're going to add the ability to easily abstract over computations at any point
via first-class functions.
Many languages provide some version of this---Python, JavaScript, Ruby, Racket,
Scheme, Java, and many more.
They enable the programmer to create a suspended computation, and pass it around
as a value.
The procedure closes over the environment in which it was created, essentially
creating an object with private fields.
They can be used as the foundations for object systems, and provide a safe,
lexically scoped alternative to function pointers.

This week, we'll add first-class functions as values in @deftech{Exprs-lang v9}:
@racketgrammar*[
[p     (module b ... e)]
[b     (define x (lambda (x ...) e))]
[e     v
       (apply e e ...)
       (let ([x e]) e)
       (if e e e)]
[v     fixnum prim-f x #t #f () (void) (error uint8) ascii-char-literal (unsyntax @bnf:add{(lambda (x ...) e)})]
[prim-f * + - eq? < <= > >=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        car
        cdr

        make-vector
        vector-length
        vector-set!
        vector-ref

        procedure-arity]
]

Now, @object-code{lambda} can appear in any expression.
We can still define procedures at the top-level using @object-code{define},
although the semantics will change slightly.

This is a syntactically small change, but it has massive implications.

Every instance of @object-code{lambda} will compile to a procedure.
The procedure now has three pieces of information: it's arity, the label to its
@deftech{code}, the computation it executes when invoked, and its
@deftech{environment}, the values of the free variables used in the definition
of the procedure.
We compile each application of a procedure to dereference and apply the label of
the procedure, but also to pass a reference to the procedure itself as a parameter.
Essentially, the procedure is an object, and receives itself as an argument.
Each "free variable" @tt{x} is a field of that object, and are compiled to
references to @tt{self.x}.

We already have the low-level abstractions in place to deal with closures, so we
design this assignment top-down.

@section{Administrative Passes}
Allowing procedures to be bound in two different ways is great for programmer
convenience, but annoying for a compiler writer.
Before we get to implementing procedures, we simplify and
regularize how procedures appear in our language.

@subsection{uniquify}
As usual with @racket[uniquify], the only change is that all names
@object-code{x} are replaced by abstract locations @object-code{aloc}.

Unlike normal, there are no @object-code{label}s.
All of our functions are procedures, not merely code, and cannot easily be
lifted to the top level, so it is now the job of a later pass to introduce
labels.

Below we define @deftech{Impure-Exprs-safe-lang v9}.
We typeset the changes with respect to @tech{Exprs-lang v9}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define (unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) (lambda ((unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) ...) e))]
[e     v
       (apply e e ...)
       (let ([(unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) e]) e)
       (if e e e)]
[v     fixnum prim-f x #t #f () (void) (error uint8) ascii-char-literal
       (lambda ((unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) ...) e)]
[prim-f * + - eq? < <= > >=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        car
        cdr

        make-vector
        vector-length
        vector-set!
        vector-ref

        procedure-arity]
]

@exercise{Redesign and extend the implementation of @racket[uniquify].
The source language is @tech{Exprs-lang v9} and the target language is
@tech{Impure-Exprs-safe-lang v9}.}

@subsection{define->letrec}
Some procedures now appear in local expressions, and some appear defined at the
top-level.
This presents two problems.
First, we have to look for procedures in two different places to transform them:
that's annoying.
Second, our compiler later assumes that all @emph{data} (as opposed to code) is
@emph{locally} defined---we have no way to define top-level, labeled data.
Since procedures are data, we need to transform top-level bindings of procedures
into local bindings, so the rest of the compiler will "just work".

To do this, we elaborate @object-code{define} into a local binding form
@object-code{letrec}, which will be used to bind all procedures.

@object-code{letrec}, unlike @object-code{let}, supports multiple bindings in a
single form, and each bound expression can refer to any variable in the set of
bindings for the @object-code{letrec}.
This is important to capture mutually-recursive functions, and has the same
binding structure as our top-level @object-code{define}s.

@digression{
A real language would impose additional semantics on @object-code{define}, such
as allowing @object-code{define}d data to be exported and imported at module
boundaries.
This would require additional handling of @object-code{define}, and the ability
to generate labeled data in the back-end of the compiler.
We continue to ignore separate compilation and linking, so we treat
@object-code{define} as syntactic sugar for @object-code{letrec}.
}

Below we define @deftech{Just-Exprs-lang v9}.
We typeset the changes with respect to @tech{Impure-Exprs-safe-lang v9}.

@racketgrammar*[
[p     (module (unsyntax @bnf:sub{b}) (unsyntax @bnf:sub{...}) e)]
[(unsyntax @bnf:sub{b})     (define aloc (lambda (x ...) e))]
[e     v
       (apply e e ...)
       (unsyntax @bnf:add{(letrec ([aloc (lambda (aloc ...) e)] ...) e)})
       (let ([aloc e]) e)
       (if e e e)]
[v     fixnum prim-f aloc #t #f ()
       (void) (error uint8) ascii-char-literal
       (lambda (aloc ...) e)]
[prim-f * + - eq? < <= > >=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        car
        cdr

        make-vector
        vector-length
        vector-set!
        vector-ref

        procedure-arity]
]

@exercise{Design and implement @racket[define->letrec].
The source language is @tech{Impure-Exprs-safe-lang v9} and the target language
is @tech{Just-Exprs-lang v9}.}

@subsection{optimize-direct-calls}
Before we start compiling @object-code{lambda}s, we should try to get rid of
them.
@emph{Direct calls} to @object-code{lambda}s, such as @racket[(apply (lambda (x)
x) 1)], are simple to rewrite to a @object-code{let} binding, such a
@racket[(let ([x 1]) x)].
A human programmer may not write this kind of code much, but most programs are
not written by humans---compilers write far more programs.
This optimization will speed-up compile time and run time for such simple
programs.

@challenge{Design and implement the function @racket[optimize-direct-calls].
The source and target language are @tech{Just-Exprs-lang v9}.
}

@subsection{dox-lambdas}
The source language supports anonymous procedures, that is, first-class
procedure values that are not necessarily bound to names.
For example, we can write the following in Racket, creating and using procedures
without ever binding them to names in a @object-code{letrec} or
@object-code{let} form.
@examples[
((lambda (x f) (f x x)) 1 (lambda (x y) (+ x y)))
]

The equivalent in @tech{Exprs-lang v9} is:
@racketblock[
(apply (lambda (x f) (apply f x x)) 1 (lambda (x y) (apply + x y)))
]

This is great for functional programmers, who value freedom, but bad for
compilers who feel it is their job to keep track of everything so they can make
good decisions.

Before we closure convert, we want to bind all procedures to names.
This will simplify lifting code to the top-level and assigning labels later.

We transform each @racket[`(lambda (,alocs ...) ,e)] into @racket[`(letrec
([,tmp (lambda (,alocs ...) ,e)]) ,tmp)], where @racket[tmp] is a fresh
@object-code{aloc}.

We define @deftech{Lam-opticon-lang v9}, in which we know the name of every
procedure.
We typeset the changes with respect to @tech{Just-Exprs-lang v9}.
@racketgrammar*[
[p     (module e)]
[e     v
       (apply e e ...)
       (letrec ([aloc (lambda (aloc ...) e)] ...) e)
       (let ([aloc e]) e)
       (if e e e)]
[v     fixnum prim-f aloc #t #f ()
       (void) (error uint8) ascii-char-literal
       (unsyntax @bnf:sub{(lambda (aloc ...) e)})]
[prim-f * + - eq? < <= > >=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        car
        cdr

        make-vector
        vector-length
        vector-set!
        vector-ref

        procedure-arity]
]

@exercise{Design and implement @racket[dox-lambdas].
The source language @tech{Just-Exprs-lang v9} and the target language is
@tech{Lam-opticon-lang v9}.
}

@subsection{implement-safe-primops}
Not much changes in @racket[implement-safe-primops].
We need to adjust its language definition to remove @object-code{define} and
support @object-code{letrec}.
We change the pass a little since it generates procedures definitions, which
should now be bound using @object-code{letrec}.

The target language of the pass, @deftech{Safe-apply-lang v9}, is defined below.
We typeset the differences compared to @a8-tech{Impure-Exprs-data-lang v8}

@racketgrammar*[
[p     (module (unsyntax @bnf:sub{b ...}) e)]
[(unsyntax @bnf:sub{b})     (define label (lambda (aloc ...) e))]
[c     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       (apply e e ...)
       (let ([aloc e]) e)
       (unsyntax @bnf:add{(letrec ([aloc (lambda (aloc ...) e)] ...) e)})
       (if e e e)
       (begin c ... e)]
[v     fixnum aloc label #t #f () (void) (error uint8) ascii-char-literal]
[primop unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
        unsafe-fx>=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        unsafe-car
        unsafe-cdr

        unsafe-make-vector
        unsafe-vector-length
        unsafe-vector-set!
        unsafe-vector-ref

        (unsyntax @bnf:sub{make-procedure})
        unsafe-procedure-arity
        (unsyntax @bnf:sub{unsafe-procedure-label})]
]

Other than the difference with @object-code{letrec} and @object-code{define}, we
also remove @object-code{make-procedure} and
@object-code{unsafe-procedure-label}.
These are introduced by a later pass that is responsible for implementing
procedures safely.
This means that @object-code{apply} can be safely applied to arbitrary data---a
later pass will implement dynamic checking for application.

@exercise{Redesign and extend the implementation of
@racket[implement-safe-primops].
The source language is @tech{Lam-opticon-lang v9} and the target language is
@tech{Safe-apply-lang v9}.

You might reduce the changes to your code by reusing @racket[define->letrec].
}

@section{Closure Conversion}
The rest of our compiler expects procedures to be little more than labeled
blocks of code.

If you completed the challenge exercise last week,
@racket[implement-safe-apply], they're only slightly more complicated.
Procedures are essentially tagged pointers to a simple data structures
that contain a label as the procedure code, and its arity for error checking.
If you completed the challenge exercise last assignment,
@racket[implement-safe-apply], our procedures this week are only slightly more
complicated.

Unfortunately, now our procedures can contain references to free-variables
in their lexical scope.
This means we cannot simply lift procedure definitions to the top-level, stick
on a label, and generate a procedure.
Before we can generate procedures, we collect the free-variable information and
create an explicit closure data structure.

We do this in two steps.

@subsection{uncover-free}
First, we uncover the free variables in each @object-code{lambda}.
We add these as an annotation on the @object-code{lambda}, which the next pass
will use to generate closures.

Below we define @deftech{Lambda-free-lang v9}.
We typeset the differences compared to @tech{Safe-apply-lang v9}.
We elide the definitions of @object-code{v} and @object-code{primop}, which are
unchanged.

@racketgrammar*[
[p     (module e)]
[c     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       (apply e e ...)
       (let ([aloc e]) e)
       (letrec ([aloc (lambda (aloc ...) (unsyntax @bnf:add{(free (aloc ...))}) e)] ...) e)
       (if e e e)
       (begin c ... e)]
[v     _...]
[primop _...]
]

To find the free abstract locations, we traverse the body of each
@object-code{lambda} remembering any abstract locations that have been bound
(by @object-code{let}, @object-code{lambda}, or @object-code{letrec}), and
return the set of abstract locations that have been used but were not in the
defined set.
On entry to the @object-code{(lambda (aloc ...) e)}, only the formal parameters
@object-code{aloc ...} are considered bound, initially.

The only complicated case is for @object-code{letrec}.
Even a variable bound in a @object-code{letrec} is considered free in the body
of a @object-code{lambda}.
@examples[#:eval eg
(uncover-free
 `(module
    (letrec ([x.1 (lambda () (apply x.1))])
      x.1)))
]
However, the @object-code{letrec} does bind those variables, so they do not
contribute to the free variable set for the context surrounding the
@object-code{letrec}.
@examples[#:eval eg
(uncover-free
 `(module
    (letrec ([f.1 (lambda ()
                    (letrec ([x.1 (lambda () (apply x.1))])
                      x.1))])
      f.1)))
]

@exercise{Design and implement of @racket[uncover-free].
The source language is @tech{Safe-apply-lang v9} and the target language is
@tech{Lambda-free-lang v9}.

You may find the @racket[map2] function from the @share{a9-skeleton.rkt}
helpful, depending on how you write your code.
You shouldn't feel like you must use it, nor try to use it if it doesn't occur
to you that you want such a function.}

@subsection{convert-closures}
Now, we convert closures.
Strictly speaking, all the previous languages had
@deftech{closures}---procedures that (implicitly) close over their lexical
environment.
Closure conversion makes these explicit as a new data type.

Below, we define @deftech{Closure-lang v9}.
We typeset changes with respect to @tech{Lambda-free-lang v9}.

@racketgrammar*[
[p     (module e)]
[c     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       ((unsyntax @bnf:sub{apply}) (unsyntax @bnf:add{unsafe-apply}) e e ...)
       (let ([aloc e] (unsyntax @bnf:add{...})) e)
       (letrec ([(unsyntax @bnf:sub{aloc}) (unsyntax @bnf:add{label}) (lambda (aloc ...) (unsyntax @bnf:sub{(free (aloc ...))}) e)] ...) e)
       (unsyntax @bnf:add{(cletrec ([aloc (make-closure label e ...)] ...) e)})
       (if e e e)
       (begin c ... e)]
[v     _...]
[primop _...
       (unsyntax @bnf:add{closure-ref})
       (unsyntax @bnf:add{closure-apply})]
]

Closure conversion changes @object-code{letrec} to bind labels to procedure
code.
After this pass, the body of @object-code{lambda} will not contain any free
variables, and will not be a procedure data type---it is just like a function
from @a6-tech{Values-lang v6}.

To encode closures, we temporarily add a new data type for closures.
We add a new form, @object-code{cletrec}, which only binds closures.
Closures can, in general, have recursive self-references, so this is a variant
of the @object-code{letrec} form.
We also add a new primop form for dereferencing the value of lexical variables
from the closure @object-code{(closure-ref e e)}.
The next pass implements closures using the procedure data type.

We assume that the @object-code{cletrec} form only ever appears as the body of a
@object-code{letrec} form, but we do not make this explicit in the syntax for
readability.
This assumption is not necessary for correctness, but simplifies an
optimization presented later as a challenge exercise.

We represent a @tech{closure} essentially as a vector containing a label to the
@tech{code} and the values of each free variable in its @tech{environment}.
@tech{Closures} support two operations.
First, you can apply a @tech{closure} with @object-code{closure-apply}, which
essentially extracts the label from the @tech{closure} and calls the procedure
at that label.
Second, you can dereference an @tech{environment} variable from the
@tech{closure} with @object-code{closure-ref}, extracting the value of a closure
at an index.

Because we want to implement safe procedure application, we add a third field to
the @tech{closure}: it's @deftech{arity}, the number of arguments expected by
the @tech{code} of the @tech{closure}.

The closure interface is described below:
@itemlist[
@item{@object-code{(make-closure e_label e_arity e_i ...)}

Creates a @tech{closure} whose @tech{code} is at label @object-code{e_label},
which expects @object-code{e_arity} number of arguments, and has the values
@object-code{e_i} in its @tech{environment}.
}
@item{@object-code{(closure-apply e_c es ...)}

Safely apply the @tech{closure} @object-code{e_c}, invoking its @tech{code}, to
the arguments @object-code{es ...}.}
@item{@object-code{(closure-ref e_c e_i)}

Deference the value at index @object-code{e_i} in the @tech{environment} of the
@tech{closure} @object-code{e_c}.
Since this dereference is only generated by the compiler, it always succeeds and
performs no dynamic checks.
The environment is 0-indexed.
}
]

There are two parts to closure conversion:
@itemlist[
@item{Transform each @object-code{lambda}.
Each @object-code{lambda} is transformed to take a new formal parameter, which
is its closure, and to be bound to a @object-code{label} in its enclosing
@object-code{letrec}.

The abstract location to which the the @object-code{lambda} was previously bound
must now be bound to a closure.
The closure has @emph{n + 1} fields, where @racket[n] is the number of free
variables in the @object-code{lambda}.
The first field is the label to which the closure's @tech{code} is bound.
The remaining fields are references to the lexical variables in the
@tech{environment} of the closure.

In essence, we transform
@racketblock[
`(letrec ([,x (lambda (,xs ...) (free (,ys ...)) ,es)] ...)
   ,e)
_=>
`(letrec ([,l (lambda (,c ,xs ...)
                (let ([,ys (closure-ref ,c ,i)] ...)
                  ,es))] ...)
    (cletrec ([,x (make-closure ,l ,(length xs) ,ys ...)] ...)
      ,e))
]
where @racket[l] is a fresh label and @racket[c] is a fresh abstract location.
To support the translation, we allow @object-code{let} to bind multiple abstract
locations at once.
We add the number of arguments as a field in the closure to implement safe
application later.
}
@item{Transform each @object-code{apply}.
Every procedure now takes an extra argument, its closure, so we have to expand
each apply.
The essence of the translation is:
@racketblock[
`(apply ,e ,es ...)
_=>
`(let ([,x ,e])
   (closure-apply ,e ,e ,es ...))
]
We use @object-code{closure-apply} to apply the (label of the) closure to
the closure itself and its usual arguments.
We need to bind the operator to avoid duplicating code.

However, if the operator is already a @object-code{aloc}, we should instead
avoid introducing an extra @object-code{let}:
@racketblock[
`(apply ,aloc ,es ...)
_=>
`(closure-apply ,aloc ,aloc ,es ...)
]
This also simplifies the optimization @racket[optimize-known-calls].

We add @object-code{unsafe-apply} to the language to enable optimizing closures,
an important optimization in functional languages.
This @object-code{unsafe-apply} directly applies a label to arguments, without
performing any checks.
@object-code{closure-apply} will get translated into the safe, dynamically
checked apply.
}
]

@exercise{Design and implement @racket[convert-closures].
The source language is @tech{Lambda-free-lang v9} and the target language is
@tech{Closure-lang v9}.

You may find the @racket[map-n] function from the @share{a9-skeleton.rkt}
helpful, depending on how you write your code.
You shouldn't feel like you must use it, nor try to use it if it doesn't occur
to you that you want such a function.
}

@subsection{Challenge: optimize-known-call}
Closures can cause a lot of indirection, and thus performance penalty, in a
functional language.
We essentially transform all call into @emph{indirect calls}.
This causes an extra memory dereference and indirect jump, both of which can
have performance penalties.

Many calls, particularly to named functions, can be optimized to direct calls.
We essentially perform the following transformation on all calls where we can
determine the label of the operator:
@racketblock[
`(closure-apply ,e ,es ...)
_=>
`(unsafe-apply ,l ,es ...)
]
where @racket[l] is known to be the label of the closure @racket[e].
Because @racket[e] is already an @object-code{aloc}, we can safely discard it;
we do not need to force evaluation to preserve any side-effects.

Because this transforms into an @object-code{unsafe-apply}, we need to inline
the arity check that @racket[implement-safe-apply] would insert.
Something like:
@racketblock[
`(closure-apply ,e ,es ...)
_=>
`(if (eq? (procedure-arity e) ,(sub1 (length es)))
     (unsafe-apply ,l ,es)
     ,bad-arity-error)
]
Remember the the @object-code{procedure-arity} will be one more than the closure
arguments, since the closure takes itself as a hidden argument.
@margin-note{We could further optimize this, since we should know the arity
statically when this optimization would apply.}

We do this by recognizing @object-code{letrec} and @object-code{cletrec} as a
single composite form:
@racketblock[
`(letrec ([,label_l ,lam])
   (cletrec ([,aloc_c (make-closure ,label_c ,es ...)])
     ,e))
]
All references uses of @object-code{(closure-apply ,aloc_c ,es ...)} in
@racket[e] and @racket[lam] can be transformed into @object-code{(unsafe-apply
,label_c ,es ...)}.
We have to recognize these as a single composite form to optimize recursive
calls inside @racket[lam], which will benefit the most from the optimization.
This relies on the name @racket[aloc_c] being bound in two places: once to
define the closure, and once when dereferenced in a recursive closure.

@challenge{Design and implement the function @racket[optimize-known-calls].
The source and target language are @tech{Closure-lang v9}.
}

@subsection{hoist-lambdas}
Now that all @object-code{lambda}s are closed and labeled, we can lift them to
top-level @object-code{define}s.

We define @deftech{Hoisted-lang v9} below.
We typeset differences with respect to @tech{Closure-lang v9}.

@racketgrammar*[
[p     (module (unsyntax @bnf:add{b ...}) e)]
[(unsyntax @bnf:add{b}) (define label (lambda (aloc ...) e))]
[c     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       (unsafe-apply e e ...)
       (let ([aloc e] ...) e)
       (unsyntax @bnf:sub{(letrec ([label (lambda (aloc ...)  e)] ...) e)})
       (cletrec ([aloc (make-closure label e e ...)] ...) e)
       (if e e e)
       (begin c ... e)]
[v     _...]
[primop _...]
]

The only difference is the @object-code{letrec} is remove and
@object-code{define} blocks are re-added.

@exercise{Design and implement the function @racket[hoist-lambdas].
The source language is @tech{Closure-lang v9} and the target language is
@tech{Hoisted-lang v9}.}

@subsection{implement-closures}
Now we implement closures as procedures.

@todo{Need to add proceudres now; removed from prior milestone}
@;@item{A @deftech{procedure} is a data structure representing a value that can be
@;called as a function.
@;Essentially, it is a wrapper around labels so we can check applications.
@;Starting in this language, application must use a procedure instead of
@;referencing a label directly.
@;We construct a procedure using @exprs-unsafe-data-lang-v8[(make-procedure e_1 e_2)], where
@;@exprs-unsafe-data-lang-v8[e_1] must evaluate to a label and @exprs-unsafe-data-lang-v8[e_2] is the number
@;of expected arguments.
@;The predicate @exprs-unsafe-data-lang-v8[procedure?] should return @exprs-unsafe-data-lang-v8[#t] for any
@;value constructed this way, and #f for any other value---@exprs-unsafee-data-lang-v8[(eq?
@;(procedure? (make-procedure e_1 e_2)) #t)].
@;We extract the label of a procedure with @exprs-unsafe-data-lang-v8[(unsafe-procedure-label
@;e_1)], where @exprs-unsafe-data-lang-v8[e_1] is a procedure.
@;We get the arity of a procedure with @exprs-unsafe-data-lang-v8[(unsafe-procedure-arity e_1)],
@;where @exprs-unsafe-data-lang-v8[e_1] is a procedure.
@;}
@;We remove @object-code{make-procedure} and @object-code{procedure-label}, which
@;are used internally.
@;The surface programmer will only be able to define safe procedures using
@;@object-code{lambda}.
@;However, we do allow the user to dynamically test whether a value is a procedure
@;and how many arguments it takes.


Our procedure object is going to be extended compared to last assignment.
Previously, we only had a label and an arity as part of a procedure.
All procedures were defined at the top-level and could not have lexical
variables.

Now, a procedure will look like an extension of a vector.
It will have at least three fields: the label, the arity, and a size.
The size indicates how large the environment of the procedure is.
The environment will be uninitialized after @object-code{make-procedure}, and
instead the environment will be initialized manually using
@object-code{unsafe-procedure-set!}, similar to vector initialization.
As before, @object-code{unsafe-procedure-label} and
@object-code{unsafe-procedure-arity} dereference the label and arity of a
procedure.
However, we now also have @object-code{unsafe-procedure-ref} which dereferences
a value from the procedure's environment, given an index into the environment,
similar to @object-code{unsafe-vector-ref}.
We still have a safe version of apply, @object-code{procedure-apply}.

The language @deftech{Proc-apply-lang v9} is defined below.
The changes are typeset with respect to @tech{Hoisted-lang v9}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[c     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       (unsafe-apply e e ...)
       (let ([aloc e] ...) e)
       (unsyntax @bnf:sub{(cletrec ([aloc (make-closure label e ...)] ...) e)})
       (if e e e)
       (begin c ... e)]
[v     _...]
[primop _...
        (unsyntax @bnf:add{make-procedure})
        (unsyntax @bnf:add{unsafe-procedure-ref})
        (unsyntax @bnf:add{unsafe-procedure-set!})
        (unsyntax @bnf:add{procedure-apply})
        (unsyntax @bnf:sub{closure-ref})
        (unsyntax @bnf:sub{closure-apply})]
]

For reference, the procedure interface is described below:
@itemlist[
@item{@object-code{(make-procedure e_label e_arity e_size)}

Creates a procedure whose label is @object-code{e_label}, which expects
@object-code{e_arity} number of arguments, and has an environment of size
@object-code{e_size}.

@object-code{make-procedure} does not perform any error checking; it must be
applied to a label and two fixnum @a7-tech{ptrs}.
This is safe because no user can access @object-code{make-procedure}
directly.
Only the compiler generates uses of this operator, and surely our compiler uses
it correctly.
}
@item{@object-code{(unsafe-procedure-ref e_proc e_index)}

Return the value at index @object-code{e_index} in the environment of the
procedure @object-code{e_proc}.

As with all unsafe operators, this does not perform any checking.
}
@item{@object-code{(unsafe-procedure-set! e_proc e_index e_val)}

Set the value at index @object-code{e_index} in the environment of the
procedure @object-code{e_proc} to be @object-code{e_val}.
}
@item{@object-code{(procedure-apply e_proc es ...)}

Safely apply the procedure @object-code{e_proc} to its arguments
@object-code{es}.
Some later pass will implement this primop to check that @object-code{e_proc} is
a procedure that expects exactly @racket[(length es)] arguments.
}
]

To transform closures into procedures, we do a three simple translations:
@itemlist[
@item{Transform @object-code{make-closure}
@racketblock[
`(cletrec ([,aloc (make-closure ,label ,arity ,es ...)] ...)
   ,e)
_=>
`(let ([,aloc (make-procedure ,label ,arity ,n)] ...)
   (begin
     (unsafe-procedure-set! ,aloc 0 ,(list-ref es 0))
     ...
     (unsafe-procedure-set! ,aloc ,n ,(list-ref es n))
     ,e))
]
where @racket[n] is @racket[(length es)], the number of values in the
environment.
}
@item{Transform @object-code{closure-ref}.
@racketblock[
`(closure-ref ,c ,i)
_=>
`(unsafe-procedure-ref ,c ,i)
]
We can use @object-code{unsafe-procedure-ref} since we generate all uses of
@object-code{closure-ref}.
}
@item{Transform @object-code{closure-apply}.
@racketblock[
`(closure-apply ,c ,es ...)
_=>
`(procedure-apply ,c ,es ...)
]
@object-code{procedure-apply} must still be dynamically checked, since procedure
applications came from user programs.
}
]

@exercise{Design and implement the function @racket[implement-closures].
The source language is @tech{Hoisted-lang v9} and the target language is
@tech{Proc-apply-lang v9}.
}

@subsection{sequentialize-let}
Next we simplify the language once more by sequentializing @object-code{let}, so
each @object-code{let} binds exactly one abstract location.
It's not terribly important when we do this, but the rest of our compiler
assumes unary @object-code{let}, so we might as well do it now.

We define @deftech{Unary-let-lang v9}, typesetting the differences with respect
to @tech{Proc-apply-lang v9}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[c     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       (unsafe-apply e e ...)
       (let ([aloc e] (unsyntax @bnf:sub{...})) e)
       (if e e e)
       (begin c ... e)]
[v     _...]
[primop _...
        make-procedure
        unsafe-procedure-ref
        unsafe-procedure-set!
        procedure-apply]
]

The translation is straightforward.

@exercise{Design and implement the function @racket[sequentialize-let].
The source language is @tech{Proc-apply-lang v9} and the target language is
@tech{Unary-let-lang v9}.
}

@subsection{implement-safe-apply}
Now we implement @object-code{procedure-apply} in terms of
@object-code{unsafe-apply}
@object-code{unsafe-procedure-label}, and
@object-code{unsafe-procedure-arity}.

Below we define @deftech{Exprs-data-lang v9}.
We typeset changes with respect to @tech{Unary-let-lang v9}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[c     (begin c ...) (primop e ...)]
[e     v
       (primop e ...)
       (unsyntax @bnf:sub{(unsafe-apply e e ...)})
       (unsyntax @bnf:add{(apply e e ...)})
       (let ([aloc e]) e)
       (if e e e)
       (begin c ... e)]
[v     _...]
[primop unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx>
        unsafe-fx>=
        fixnum? boolean? empty? void? ascii-char? error? not
        pair?
        procedure?
        vector?

        cons
        unsafe-car
        unsafe-cdr

        unsafe-make-vector
        unsafe-vector-length
        unsafe-vector-set!
        unsafe-vector-ref

        make-procedure
        unsafe-procedure-arity
        unsafe-procedure-label
        unsafe-procedure-ref
        unsafe-procedure-set!
        (unsyntax @bnf:sub{procedure-apply})]
]

We implement @object-code{procedure-apply} in terms of @object-code{procedure?},
@object-code{unsafe-procedure-label}, and @object-code{unsafe-procedure-arity}.
The essence of the transformation is:
@racketblock[
`(procedure-apply ,e ,es ...)
_=>
`(if (procedure? ,e)
     (if (eq? (unsafe-procedure-arity ,e) ,(sub1 (length es)))
         (apply (unsafe-procedure-label ,e) ,es ...)
         ,bad-arity-error)
     ,bad-proc-error)
]
We subtract one from the length of the parameter list to account for the closure
parameter.
We could equivalently add one to the procedure arity, but since the length of
the parameter list is known at compile-time, this saves us at least one run-time
instruction.

@digression{
This pass assumes the closure argument must always be there.
This design prevents us from optimizing away the closure parameter easily, a
slight annoyance that is due to your professor missing this design mistake
before releasing the assignment.
A better design would place this pass before closure conversion, exposing
@object-code{unsafe-apply} earlier.
Then we would have access to the correct arity count without the closure
argument, and closure conversion modify the @object-code{unsafe-apply} form
without disrupting @object-code{procedure-arity}.

We do not want to capture the closure parameter in the
@object-code{procedure-arity} value, since this value is exposed to a user, and
we do not want the user to know about the internal closure parameter.
This internal parameter is not part of their code, so we should not burden them
with it.
}

We change the name of @object-code{unsafe-apply} to @object-code{apply}, since
that's what the rest of the compiler uses.

Note that we cannot simply define @object-code{procedure-apply} as a procedure,
like we did with other safe wrappers, since it must count its arguments, and we
must support a variable number of arguments to the procedure.

@exercise{Design and implement the function @racket[implement-safe-apply].
The source language is @tech{Unary-let-lang v9} and the target language is
@tech{Exprs-data-lang v9}.
}

@subsection{specify-representation}
Finally, we need to modify the procedure data type slightly.
It was intentionally designed to be similar to the vector data type.

We define @deftech{Impure-Exprs-bits-lang v9} below.
There are no differences with respect to @a8-tech{Impure-Exprs-bits-lang v8}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define label (lambda (aloc ...) e))]
[c     (begin c ...) (mset! e e e)]
[e     v
       (let ([aloc e]) e)
       (if (cmp e e) e e)
       (begin c ... e)
       (binop e e) (apply e e ...)
       (alloc e)
       (mref e e)]
[v     int64 label aloc]
[binop * + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right]
[cmp   neq? eq? < <= > >=]
]

When implementing @object-code{make-procedure}, you may assume the size of the
environment is a fixnum constant.

@exercise{Redesign and extend the implementation of the function
@racket[specify-representation].
The source language is @tech{Exprs-data-lang v9} and the target language is
@tech{Impure-Exprs-bits-lang v9}.
}

No other passes should need to be updated.
