#lang reader "assignment-lang.rkt"
@(require cpsc411/deprecated/a10-compiler-lib)

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@;TODO abstractions for each of these; no more copy/paste/modify
@(define eg
   (make-cached-eval
    "a10-eval"
    '(require
      cpsc411/v1-reference/a10-solution
      cpsc411/deprecated/a10-compiler-lib
      racket/pretty)
    '(current-stack-size 512)))

@title[#:tag "top" #:tag-prefix "a10:"]{Compiler 10: Recursive Data and
Syntactic Sugar}

@section{Assignment Summary}
The goal of this assignment is to add recursive data to our language.
This allows us to create streams or cyclic graphs, which are useful in a variety
of algorithms (such as writing control-flow graph algorithms for compilers).
With this feature, we'll finally be able to completely collapse distinctions in
our surface syntax.

We'll also add a little syntactic sugar to the surface language.
This isn't really related to recursive data, but the assignment is small if we
don't merge the two separate goals into a single assignment.

This assignment is due Friday, April 10, 2020 at 11:59pm.

You can use the reference solution here:
@url{https://www.students.cs.ubc.ca/~cs-411/2019w2/a10-interrogator.cgi}

@subsubsub*section{Assignment Checklist}
You should find a new repository in your
@url{https://github.students.cs.ubc.ca} account named @tt{a10_<team ids>} with a
code skeleton and a support library.
You should complete the assignment in that git repository and push it to the
GitHub Students instance.

You should first merge your solution to @secref[#:tag-prefixes '("a9:")]{top}
with the starter code provided.
The new starter code has the correct provides and includes a submodule to help
you run your compiler on the command line if you want.
The name of the skeleton is @share{a10-skeleton.rkt} to avoid accidentally
overwriting your files, but your file in the Git repository should be named
@tt{a10.rkt}.

@section{Language Diagram}

@;@dot->svg{...}

@section{Preface: What's wrong with Exprs-Lang v9}
@a9-tech{Exprs-lang v9} gave us the ability to write first-class lexically
scoped local functions, a powerful tool for abstracting over computation.
However, without the ability to write recursive data, we could only write simple
non-recursive local functions.
This makes implementing local loops annoying; we had to go through top-level
definitions instead.
It also means we can't directly implement other cyclic data structures, like
streams or control-flow graphs!
By adding recursive data, we can implement arbitrary cyclic data structures,
and get first-class lexically-scoped @emph{recursive} functions (generalized
loops) as a happy side effect.

@margin-note{Many languages support cyclic data through imperative mutation, but
as we'll see in this assignment, that's a low-level implementation technique
that the user need not be required to use.
}

This week, we'll add @object-code{letrec} to the surface language, defining
@deftech{Racketish}, a tiny Racket-like core language.
It's missing a few key features from Racket, but it's a substantial start.
@margin-note{Most of Racket is implemented using its macro system, thereby
extending Racket in Racket, which all elaborates to a simple core language.}
@racketgrammar*[
[p     (module b ... e)]
[b     (define x e)]
[e     v
       (apply e e ...)
       (let ([x e]) e)
       (unsyntax @bnf:add{(letrec ([x e] ...) e)})
       (if e e e)]
[v     fixnum prim-f x #t #f () (void) (error uint8) ascii-char-literal (lambda (x ...) e)]
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

We add @object-code{letrec}, which we saw in the last assignment, to the surface
language.
Unlike last week, it is not restricted to bind variables only to procedures.
This apparently simple change has deep consequences.

Further, since we have names instead of abstract locations, @object-code{letrec}
is restricted to only bind each name once in a single @object-code{letrec}.

@section{Preliminary Passes}
@subsection{uniquify}
First we modify @racket[uniquify].
Below we define @deftech{Racketish-Unique}.

@racketgrammar*[
[p     (module b ... e)]
[b     (define (unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) e)]
[e     v
       (apply e e ...)
       (let ([(unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) e]) e)
       (letrec ([(unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) e] ...) e)
       (if e e e)]
[v     fixnum prim-f (unsyntax @bnf:sub{x}) (unsyntax @bnf:add{aloc}) #t #f ()
       (void) (error uint8) ascii-char-literal
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

As usual with @racket[uniquify], the only change is that all names
@object-code{x} are replaced by abstract locations @object-code{aloc}.

@exercise{Redesign and extend the implementation of @racket[uniquify].
The source language is @tech{Racketish} and the target language is
@tech{Racketish-Unique}.

Be careful with the binding for @object-code{letrec}.}

@subsection{define->letrec}
Next we elaborate @object-code{define} into @object-code{letrec}, as before.
Below we define @deftech{Just-Exprs-lang v10}.

@racketgrammar*[
[p     (module (unsyntax @bnf:sub{b ...}) e)]
[(unsyntax @bnf:sub{b}) (define aloc e)]
[e     v
       (apply e e ...)
       (letrec ([aloc e] ...) e)
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

@exercise{Redesign and extend the implementation of @racket[define->letrec].
The source language is @tech{Racketish-Unique} and the target language
is @tech{Just-Exprs-lang v10}.
}

@section{Resolving Recursive Binding}
The rest of our compiler assumes that @object-code{letrec} only binds
procedures.
We need to purify all @object-code{letrec}s so this is true.
This is non-trivial, because we could have bound a recursive data structure.
Consider following Racket example:
@examples[
(letrec ([x.1 (cons 1 (lambda () x.1))])
  (+ (car x.1) (car ((cdr x.1)))))
]
@racket[x.1] is an encoding of an infinite stream of 1s.

To translate a recursive data structure into a @object-code{let}-bound data
structure, we need mutable variables.
We add mutable variables to the target language, and transform the above example
into:
@examples[
(let ([x.1 (void)])
  (let ([x.2 (cons 1 (lambda () x.1))])
    (begin
      (set! x.1 x.2)
      (+ (car x.1) (car ((cdr x.1)))))))
]
We don't want to do this transformation for every @object-code{letrec}, because
@object-code{set!} will be more expensive to compile than what we can do for
@object-code{letrec}-bound procedures.

We've actually already seen this transformation; we did it when implementing
closures, although its details were somewhat obscured.
Closures are a form of recursive data---they have a binding to themselves in
their environment.
When we implemented closures as procedures, we translated the
@object-code{cletrec} form into a @object-code{let} followed by several
@object-code{unsafe-procedure-set!} calls to tie the knot.

@subsection{purify-letrec}
We define @deftech{Landin-knot-lang v10}.

@racketgrammar*[
[p     (module e)]
[c     (set! aloc e)]
[e     v
       (apply e e ...)
       (letrec ([aloc e] ...) e)
       (let (unsyntax @bnf:add{(info ((assigned (aloc ...))))}) ([aloc e] ...) e)
       (if e e e)
       (begin c ... e)]
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

Now @object-code{letrec} only binds procedures.
We also allow n-ary binding in @object-code{let}, to simplify the compiler pass.
We add a single computation, @object-code{(set! aloc e)}.
Each @object-code{let} form is annotated with an @object-code{info} structure
containing the list of @object-code{aloc}s that are assigned in its body, and is
used to keep track of mutable @object-code{aloc}s.

To purify @object-code{letrec} to only bind procedures, we transform
@racket[`(letrec ([,xs ,es] ...) ,e)] into the following, partitioning the
original bindings @racket[`([,xs ,es] ...)].
@racketblock[
`(let (info ((assigned ())))
   ([,xs_s ,es_s] ...)
   (let (info ((assigned (,xs_c ...))))
     ([,xs_c (void)] ...)
       (letrec ([,xs_l ,es_l] ...)
          (let (info ((assigned ())))
            ([,xs_t ,es_c] ...)
              (begin
                (set! ,xs_c ,xs_t) ...
                 ,e)))))
]
where
@itemlist[
@item{@racket[`([,xs_s ,es_s] ...)] are all @tech{simple} bindings}
@item{@racket[`([,xs_l ,es_l] ...)] are all @tech{lambda} bindings}
@item{@racket[`([,xs_c ,es_c] ...)] are all @tech{complex} bindings}
@item{@racket[`(,xs_t ...)] are all fresh auxiliary @object-code{aloc}s.}
]

A binding @racket[`[,x ,e]] is @deftech{simple} binding if the bound expression
@racket[e] contains no occurrences of the variables @racket[xs] bound by the
@object-code{letrec} expression and no applications unless the application is a
@object-code{prim-f} with @tech{simple} operands.
We can also reduce compile time by considering @object-code{letrec}
non-@tech{simple}; otherwise, the @tech{simple} check may be non-linear in the
number of let-bindings.
We may get better run-time performance by trying to check if a nested
@object-code{letrec} is really @tech{simple}, though.
@margin-note{The restriction on applications is to avoid problems if we ever
add control-flow constructs, such as @racket[call/cc], to our language.}

@; In Kent's a14, there are references to simple lambdas, or lambdas in simple
@; expressions. I can't make sense of this, so for now, I've removed them.
@; (1) they are nested within @object-code{lambda} expressions or (2) unless

A binding @racket[`[,x ,e]] is @deftech{lambda} if @racket[e] is literally a
@object-code{lambda} value.

Otherwise, the binding is @deftech{complex}.

@tech{Complex} bindings @racket[`[,x ,e]] are compiled as follows.
First, we set up an initial let-binding to @racket[x] to @object-code{(void)}.
Then, we introduce an auxiliary binding @racket[`[,xt ,e]].
In it, @racket[e] now has the correct binding, referring to @racket[x].
But @racket[x] does not have the right value.
So we finally mutate @racket[x] to have the value of @racket[xt], so now both
@racket[xt] and @racket[x] have the same value, and the right recursive binding
structure.
This implements recursion using back-patching mutation, a technique called
@deftech{Landin's Knot}.

@exercise{Design and implement @racket[purify-letrec].
The source language is @tech{Just-Exprs-lang v10} and the target language is
@tech{Landin-knot-lang v10}.

The reference implementation takes an optional second parameter specifying an
optimization level.
@racket[(curryr purify-letrec 0)] (the default) does not treat nested
@object-code{letrec} as @tech{simple}.
@racket[(curryr purify-letrec 1)] tries to detect @tech{simple} nested
@object-code{letrec}s.
}

@subsection{convert-assigned}
Previously, we did not expose @object-code{set!} beyond the backend of the
compiler, so we must implement @object-code{set!}.
We cannot simply expose @object-code{set!} from the backend of the compiler,
because this version of @object-code{set!} behaves differently.
It acts much more like Racket's @racket[set-box!].

This @object-code{set!} is forcing the variable to act like a heap-allocated
location that is implicitly dereferenced.
In the backend of our compiler, @object-code{set!} simply moved a value from one
location to another.
That can't be what happens here.
Consider the Racket example from earlier:
@racketblock[
(let ([x.1 (void)])
  (let ([x.2 (cons 1 (lambda () x.1))])
    (begin
      (set! x.1 x.2)
      (+ (car x.1) (car ((cdr x.1)))))))
]
If this @racket[set!] had a move semantics, @ie it simply moved values
around, then first the value @racket[(void)] would be moved into @racket[x.1],
and next the value @racket[(cons 1 (lambda () (void)))] would be moved into
@racket[x.2], so we would never get a recursive data structure.

Instead, this @racket[set!] is forcing the variable @racket[x.1] to act as a
@racket[box] (essentially, as a pointer), and any reference to @racket[x.1] acts
as an implicit dereference of the @racket[box].
The example would make a great deal more sense if written as:
@racketblock[
(let ([x.1 (box (void))])
  (let ([x.2 (cons 1 (lambda () (unbox x.1)))])
    (begin
      (set-box! x.1 x.2)
      (+ (car (unbox x.1)) (car ((cdr (unbox x.1))))))))
]

So now, we need to implement @object-code{set!} and the assigned locations to
give them a box-like semantics, rather than a move semantics.

We do this using vectors.
@margin-note{Vectors use more memory than we need.
We could reduce memory usage by adding a primitive @racket[box] data type.
However, this requires adding a new tag, modifying complex passes, and has
little pedagogical value.}

Below we define @deftech{Landin-vec-lang v10}.
We no longer have @object-code{set!} or impure computation; everything must be
let-bound once more.

@racketgrammar*[
[p     (module e)]
[(unsyntax @bnf:sub{c})     (set! aloc e)]
[e     v
       (apply e e ...)
       (letrec ([aloc (lambda (aloc ...) e)] ...) e)
       (let (unsyntax @bnf:sub{(info ((assigned (aloc ...))))}) ([aloc e] ...) e)
       (if e e e)
       (unsyntax @bnf:sub{(begin c ... e)})]
[v     fixnum prim-f aloc #t #f ()
       (void) (error uint8) ascii-char-literal
       (lambda (aloc ...) e)]
[prim-f _...]
]
Remember that this pass happens before @racket[implement-safe-primops], and the
safe version of @object-code{vector-set!} must be let-bound since it might
return an error value.
You might think about how to redesign the compiler to avoid the unnecessary
dynamic checks we're about to introduce.

We translate each @deftech{assigned} abstract location into a vector, and each
reference to an @tech{assigned} location to a dereference from the vector, and
each @object-code{set!} to a @object-code{vector-set!}.
For example:
@racketblock[
`(let (info ((assigned (x.1))))
      ([x.1 (void)])
   (let (info ((assigned ())))
     ([tmp.2 (apply cons 1 (lambda () x.1))])
     (begin
       (set! x.1 tmp.2)
       x.1)))
_=>
`(let ([x.1 (apply make-vector 1)])
   (let ([tmp.2 (apply cons 1 (lambda () (apply vector-ref x.1 0)))])
     (let ([tmp.3 (apply vector-set! x.1 0 tmp.2)])
       (apply vector-ref x.1 0))))
]

@exercise{Design and implement the function @racket[convert-assigned].
The source language is @tech{Landin-knot-lang v10} and the target language is
@tech{Landin-vec-lang v10}.}

@subsection{dox-lambdas, implement-safe-primops, uncover-free, convert-closures}
Now we just need to make interfaces match.
We've moved multi-arity @object-code{let} up a few passes, so we need to adapt
the languages between here and @racket[sequentialize-let].

@exercise{Redesign and extend the implementation of
@itemlist[
@item{@racket[dox-lambdas], whose source language is @tech{Landin-vec-lang v10}.
You need to add support for multi-arity @object-code{let}.}
@item{@racket[implement-safe-primops], you need to add support for multi-arity
@object-code{let} in the source language.}
@item{@racket[uncover-free], you need to add support for multi-arity
@object-code{let} in the source language.}
@item{@racket[convert-closures], you need to add support for multi-arity
@object-code{let} in the source language.}
]}

@section{Syntactic Sugar}

@subsection{Preface: What's wrong with Racketish?}
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

@subsection[#:tag "uniquify-2"]{uniquify}
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

@subsection{expand-macros}
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

@;  LocalWords:  lang rkt TODO eg eval pm url subsubsub tt ids GitHub secref
@;  LocalWords:  Exprs emph letrec deftech Racketish racketgrammar unsyntax eq
@;  LocalWords:  bnf fixnum uint ascii cdr ref arity uniquify aloc cletrec ary
@;  LocalWords:  Landin xs es racketblock itemlist cc xt Landin's curryr ie
@;  LocalWords:  unbox vec primops tmp dox expr huzzah REPL DrRacket codeblock
@;  LocalWords:  racketvarfont uniquified falsey exprs sexpr
