#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a10-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v9
  (for-label cpsc411/langs/v9)
  #;cpsc411/langs/v10
  #;(for-label cpsc411/langs/v10))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a10-solution]

@(define sb
   (make-cached-eval
    "ch10-eval"
    '(require racket/pretty cpsc411/reference/a10-solution cpsc411/compiler-lib)))

@title[#:tag "top" #:tag-prefix "chp-macros:"]{Syntactic Sugar}

@section{Preface: What's wrong with Racketish?}
@tech{Racketish} is a good start at a core language.
You could start building standard libraries and get to work.
But it's missing a handful of nice features.
Most languages support various complex data literals, such as Racket's
@racket[quote] form for lists.
They also support features you cannot encode as functions, like short-circuiting
boolean @racket[and] and @racket[or].

These get elaborated very early.
In Racket, these are mostly implemented as @emph{macros}, a user-extensible
system for expressing syntactic rewrites.
We'll design a compiler pass that isn't quite a macro system, but should
give you an idea of how a macro system works, and give you the ability to add
new syntactic sugar to the language.

Below, we define @deftech{Racketish-Surface}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define x e)]
[e     v
       (unsyntax @bnf:add{'s-expr})
       (unsyntax @bnf:add{#(e ...)})

       (unsyntax @bnf:sub{(apply e e ...)})
       (unsyntax @bnf:add{(e e ...)})
       (unsyntax @bnf:add{(macro-id e ...)})
       (let ([x e]) e)
       (letrec ([x e] ...) e)
       (if e e e)]
[v     fixnum prim-f x #t #f () (void) (error uint8) ascii-char-literal
       (lambda (x ...) e)]
[(unsyntax @bnf:add{s-expr}) #t #f fixnum ascii-char-literal () (s-expr ...)]
[(unsyntax @bnf:add{macro-id}) and or quote vector begin]
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

We add two special syntaxes, @object-code{'s-expr} for quoted lists, and
@object-code{#(e ...)} for vector literals.
Note that our language does not have symbols, so our @object-code{s-expr}
differs from Racket's.
@object-code{'s-expr} is a reader macro in Racket and is implicitly elaborated
to @object-code{(quote s-expr)}.
@object-code{#(e ...)} is the notation for vector literals, and is equivalent to
@object-code{(vector e ...)}, but not syntactically identical in the same way
@object-code{'s-expr} and @object-code{(quote s-expr)} are.

Because Racket's reader supports them, we do not have to do anything to
add them!
We avoid implementing a parser once more (huzzah!).
This is one interesting feature of @deftech{bootstrapping}---defining a language
in itself.
@margin-note{Parsing is often described as a solved problem, but it is not, and
is easy to get wrong, and doing a bad job can
@hyperlink["https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags"]{summon
the dark gods to our realm}.
We do not want to summon dark gods, so we worked directly with data in the host
language, and it was fine.
}

You can see the two are equivalent in Racket by trying typing the following in
to the REPL or in the interactions frame in DrRacket:
@codeblock{
'(1 2 3)

(quote (1 2 3))

#(1 2 3)

(vector 1 2 3)
}
@margin-note{I can't typeset these examples using my normal REPL typesetting
feature since Racket implicitly renders them identically.}

Using @racket[match] on this notation requires a little care, since our program
is itself a quoted datum in Racket, and @racket[match] treats them slightly
differently.
@codeblock{
(match '(quote (1 2 3))
  [`(quote ,es) es])

(match ''(1 2 3)
  [`(quote ,es) es])

(match '#(1 2 3)
  [`#(,es ...) es])

(match '#(1 2 3)
  [(vector es ...) es])

;This one will fail to match
 (match '#(1 2 3)
   [`(vector ,es ...) es])
}

We also introduce a new form for applying macros, and add a few macros.
We also make function application implicit; essentially, anything that looks
like an application form is either a macro application, or, is implicitly a
function application.

@section[#:tag "uniquify-2"]{uniquify}
Macros can often introduce new variable binding, so it's still important to have
some mechanism for handling fresh variables.
Before we expand, we once more @racket[uniquify].

Below we define @deftech{Racketish-Surface-Unique}, updating it to include the
new forms and macros.
We typeset changes with respect to @tech{Racketish-Surface}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define (unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) e)]
[e     v
       (unsyntax @racketvarfont{'s-expr})
       #(e ...)
       (e e ...)
       (macro-id e ...)
       (let ([(unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) e]) e)
       (letrec ([(unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) e] ...) e)
       (if e e e)]
[v     fixnum prim-f (unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) #t #f ()
       (void) (error uint8) ascii-char-literal
       (lambda ((unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) ...) e)]
[s-expr #t #f fixnum ascii-char-literal () (s-expr ...)]
[macro-id and or quote vector begin]
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
The source language is @tech{Racketish-Surface} and the target language is
@tech{Racketish-Surface-Unique}.

To support implicit application, you'll probably want predicates for determining
when something is a macro id, and when something is a value keyword.

To handle @object-code{'s-expr}, remember that it is implicitly elaborated to
@object-code{(quote s-expr)} by Racket.}

@section{expand-macros}
Now we can expand macros.
Ideally, this is designed to be extensible, so we can add new macros at will, or
even allow the user to define macros.
We don't force you to design your compiler this way, though.

Our target language is the (uniquified) core language:
@tech{Racketish-Unique}.
All macros will be elaborated to existing features.
We also elaborate implicit function application into explicit
@object-code{apply}.

The transformation we want to define are given below:
@itemlist[
@item{@object-code{(and v ...)}
@racketblock[
`(and) _=> #t
`(and ,e) _=> e
`(and ,e ,es ...) _=> `(if ,e (and ,es ...) #f)
]

When @object-code{and} is given no arguments, we expand to #t, since #t is the
multiplicative identity element on booleans (like 1 is for integers and
multiplication).
When given a single argument, we return it, since Racket is falsey.
Otherwise, we elaborate to branch on the first argument, and recursively expand
@object-code{and} applied to the rest of its arguments.
}
@item{@object-code{(or v ...)}
@racketblock[
`(or) _=> #f
`(or ,e) _=> e
`(or ,e ,es ...) _=> `(let ([,tmp ,e]) (if ,tmp ,tmp (or ,es ...)))
]

When @object-code{or} is given no arguments, we expand to #f, since #f is the
additive identity element on booleans (like 0 is for integers and addition).
When given a single argument, we return it, since Racket is falsey.
Otherwise, we bind the first argument to an auxiliary variable, return it if its
true, and otherwise return the @object-code{or} of the rest of the arguments.

Because we introduce an auxiliary variable, we see why it's important for macros
to work with unique variables.
Otherwise, this could accidentally capture any use of a free variable in
@object-code{es ...}, just like any compiler pass that introduces auxiliary
variables.
}
@item{@object-code{(vector e ...)} and @object-code{#(e ...)}
@racketblock[
`(vector ,es ...)
_=>
`(let ([,tmp (make-vector ,(length es))])
   (begin
     (vector-set! ,tmp 0 ,(list-ref es 0))
     ...
     (vector-set! ,tmp ,(sub1 (length es)) ,(list-ref es (sub1 (length es))))
     tmp))
]
}
@item{@object-code{(quote s-expr)}
@racketblock[
`(quote ,v) _=> v
`(quote (,s-expr ,s-exprs ...))
_=>
`(cons (quote ,s-expr) (quote (,s-exprs ...)))
]
}
@item{@object-code{(begin e ...)}
@racketblock[
`(begin) _=> '(void)
`(begin ,e) _=> e
`(begin ,e ,es ...)
_=>
`(let ([,tmp ,e])
   (if (error? ,tmp)
       ,tmp
       `(begin ,es ...)))
]

We add a surface-level begin form that handles propagating errors correctly.
This is essentially an implementation of @tt{do} for the monad.
}
]

These rewrites are essentially @racket[syntax-case] macros, recursive
pattern-based macros that can perform computation at compile time.

Notice that each macro can generate more uses of macros.
As a result, the recursive structure of this elaborate pass is going to be
slightly different from other passes.

@;There are (at least) two ways to design the @racket[expand-macros] function:
@;either as a compiler (easy way), or as a macro expander (hard way).
@;As a compiler, you design the function as you have designed all your other
@;compiler transformations, more or less.
@;
@;As a macro expander, your should have an environment of macro identifiers bound
@;to @deftech{macro transformers}---functions from syntax to syntax, which the
@;compiler will execute.
@;The @tech{macro transformer} receives the syntax of the expression that invoked
@;it, as an s-expression, and should return a new s-expression.
@;To implement this, you need to add a notion of symbols to the language of your
@;macro transformers, and an interpreter to run macro transformers at
@;compile-time.
@;The symbols do not need to remain in the language after macro-expansion, since
@;they will only be used by the macro-expander.

@exercise{Design and implement the function @racket[expand-macros].
The source language is @tech{Racketish-Surface-Unique} and the target language
is @tech{Racketish-Unique}.

Try designing the pass so that each of the above macros is defined in its own
helper function, and make @racket[expand-macros] handle macros completely
generically.
You'll be part of the way to an implementation of a macro system that supports
user-defined macros.
If you exposed an interface in @tech{Racketish-Surface} and used
@racket[eval]...
}

@;@challenge{Design and implement an extension to
@;@racket{Racketish-Surface-Unique} that enables user-defined macros.
@;}

@section{)}
The end.

@exercise{Good luck. Have fun.}

@section[#:tag "sec:overview"]{Appendix: Overview}

@define[v10-graph
@dot->svg{
digraph {

node [ shape="box", fontsize=12 ]


/* The Languages */

Lx [label="Exprs-lang v9"];
Ly [label="Exprs-unique-lang v9"];
Lz [label="Exprs-unsafe-data-lang v9"];
Lz1 [label="Exprs-unsafe-lang v9"];
Lz2 [label="Just-exprs-lang v9"];
Lz3 [label="Lam-opticon-lang v9"];
Lz4 [label="Lam-free-lang v9"];
Lz5 [label="Closure-lang v9"];
Lz6 [label="Hoisted-lang v9"];
Lz7 [label="Proc-exposed-lang v9"];
L0 [label="Exprs-bits-lang v8"];
L1 [label="Values-bits-lang v8"];
L3 [label="Imp-mf-lang v8"];
L2 [label="Proc-imp-cmf-lang v8"];
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
Lz -> Lz1 [label=" implement-safe-call"];
Lz1 -> Lz2 [label=" define->letrec"];
Lz2 -> Lz2 [label=" optimize-direct-calls"];
Lz2 -> Lz3 [label=" dox-lambdas"];
Lz3 -> Lz4 [label=" uncover-free"];
Lz4 -> Lz5 [label=" convert-closures"];
Lz5 -> Lz5 [label=" optimize-known-calls"];
Lz5 -> Lz6 [label=" hoist-lambdas"];
Lz6 -> Lz7 [label=" implement-closures"];
Lz7 -> L0 [label=" specify-representation"];
L0 -> L1 [label=" remove-complex-opera*"];
L1 -> L3 [label=" sequentialize-let"];
L3 -> L2 [label=" normalize-bind"];
L2 -> L4 [label=" impose-calling-conventions"]
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

@figure["fig:v10-graph" "Overview of Compiler Version 10" v10-graph]
