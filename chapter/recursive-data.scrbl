#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  scriblib/figure
  (for-label cpsc411/reference/a10-solution)
  (for-label (except-in cpsc411/compiler-lib compile))
  cpsc411/langs/v9
  (for-label cpsc411/langs/v9)
  cpsc411/langs/v10
  (for-label cpsc411/langs/v10))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a10-solution]

@(define sb
   (make-cached-eval
    "ch10-eval"
    '(require racket/pretty cpsc411/reference/a10-solution cpsc411/compiler-lib)))

@(define todo margin-note)

@title[#:tag "top" #:tag-prefix "chp-letrec:"]{Recursive Data}

@section{Preface: What's wrong with Exprs-Lang v10}

@todo{This Chapter still needs too much work. Contains too many hacks.}

@ch9-tech{Exprs-lang v9} gave us the ability to write first-class lexically
scoped local procedures, a powerful tool for abstracting over computation.
However, without the ability to write recursive data, we could only write simple
non-recursive local procedures.
This makes implementing local loops annoying; we had to go through top-level
definitions instead.
By adding recursive data, we can implement first-class lexically-scoped
@emph{recursive} procedures (generalized loops), but also enacode other cyclic
data such as stream.

@margin-note{Many languages support cyclic data through imperative mutation, but
as we'll see in this assignment, that's a low-level implementation technique
that the user need not be required to use.
Instead, we can express cyclic data more abstractly through recursion and
suspended computation.
Unfortunately, the only suspended computation primitive we have is
@exprs-lang-v9[lambda], and our surface language is call-by-value, so this
still requires a little encoding.
}

This week, we'll add @racketish-core-v10[letrec] to the surface language, defining
@deftech{Racketish Core v10}, a tiny Racket-like core language.
It's missing a few key features from Racket, but it's a substantial start.
@margin-note{Most of Racket is implemented using its macro system, thereby
extending Racket in Racket, which all elaborates to a simple core language.}

@bettergrammar*-ndiff[
(exprs-lang-v9 racketish-core-v10)
]

We add @racketish-core-v10[letrec], which we saw in the previous chapter, to the
surface language.
Unlike the previous version, this @racketish-core-v10[letrec] is not restricted to
bind only procedures.
This apparently simple change has deep consequences.

In the surface language, @racketish-core-v10[letrec] is restricted to only bind each
name once in a single @racketish-core-v10[letrec].
A @racketish-core-v10[letrec] binds all names in a single mutually recursive scope,
so later bindings are accessible to earlier bindings, and thus it wouldn't make
sense to have the same name bound multiple times in the same scope.

@section{Compiling Recursion to Back-Patching Mutation}
The rest of our compiler assumes that @racketish-core-v10[letrec] only binds
procedures.
Compiling first-class procedures is complicated for reasons others than
recursion, and keeping them separated is useful.
To maintain this separation and avoid extending the closure conversion passes
unnnecessarily, we can purify @racketish-core-v10[letrec], by compiling all
non-procedure recursive bindings into a lower-level implementation of recursion.

Consider following Racket example:
@examples[
(letrec ([x.1 (cons 1 (lambda () x.1))])
  (+ (car x.1) (car ((cdr x.1)))))
]
@racket[x.1] is an encoding of an infinite stream of 1s.
The encoding binds @racket[x.1] not to a procedure, but to a pair that has a
delayed self-reference to @racket[x.1], where suspended computation is
implemented using a first-class procedure.

To translate a recursive data structure into a @racketish-core-v10[let]-bound data
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

This implements recursion through back-patching; we create an uninitialized
variable, @racket[x.1], and reference this in a suspended computation before it
is initialized.
Then we initialize @racket[x.1] to the value that contains a pointer
@racket[x.1].
After that, later uses of @racket[x.1] can call that suspended computation,
which will get the right value.

We don't want to do this transformation for every @racketish-core-v10[letrec], because
@object-code{set!} will be more expensive to compile than what we can do for
@object-code{letrec}-bound procedures.

We've actually already seen this transformation; we did it when implementing
closures, although its details were somewhat obscured.
Closures are a form of recursive data---they have a binding to themselves in
their environment.
When we implemented closures as procedures, we translated the
@object-code{cletrec} form into a @object-code{let} followed by several
@object-code{unsafe-procedure-set!} calls to tie the knot.

Unfortunately, since we haven't exposed many imperative commands high in the
compiler yet, we'll have to use a bad abstraction to implement back-patching.
We'll compile our uninitialized values to vectors, and compile
@object-code{set!} to @object-code{vector-set!}.
@digression{
This is a hack. I just haven't gotten around to writing a chapter on how to
extend the surface language with @object-code{set!}.

But, you'll sometimes find you're forced to use such hacks when working with
abstraction boundaries you have no control over.
}

@section{Preliminary Passes}
As usual, we don't want to manipulate any bindings before we've uniquified, so
first we modify @racket[uniquify].

Below we define @deftech{Racketish-Unique}.

@bettergrammar*-ndiff[
(racketish-core-v10 racketish-unique-v10)
]

As usual with @racket[uniquify], the only change is that all names
@racketish-core-v10[x] are replaced by abstract locations
@racketish-unique-v10[aloc].
But we need to pay attention to the mutually recursive scope of
@racketish-unique-v10[letrec].

@nested[#:style 'inset
@defproc[(uniquify [p racketish-core?]) racketish-unique?]{
Resolves all @ch3-tech{lexical identifiers} into unique @ch2-tech{abstract
locations}.
}
]

Next, we need to move @racket[define->letrec] up, as the pass just after
@racket[uniquify].
This enables @racketish-core-v10[define] to bind arbitrary expressions, but also
simplifies our implementation of recursive binding.

@nested[#:style 'inset
@defproc[(define->letrec [p racketish-unique-v10?])
         just-exprs-lang-v10?]{
Transform all top-level bindings into local bindings.
}]

@todo{This complicates implement-safe-primop since they introduce defines...

Want implement-safe* to be just after uniquify.
They should get to take advantage of as many surface features as possible.}

@section{Resolving Recursive Binding}
Our first goal is ensure that @assigned-lang-v10[letrec] only binds procedures.

We define @deftech{Assigned-lang v10}.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Assigned-lang v10")
(racketish-unique-v10 assigned-lang-v10)
(assigned-lang-v10)
]

In this language @object-code{letrec} only binds procedures.
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

A binding @racket[`[,x ,e]] is a @deftech{lambda} binding if @racket[e] is
literally a @object-code{lambda} value.

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

@nested[#:style 'inset
@defproc[(purify-letrec [p racketish-unique?]) assigned-lang-v10?]{
Compiles @tech{Racketish-Unique} to @tech{Assigned-lang v10}, transforming all
@assigned-lang-v10[letrec] expressions to bind only procedures, and implementing
recursive data structures through back-patching.
}
]

@;@exercise{Design and implement @racket[purify-letrec].
@;The source language is @tech{Just-Exprs-lang v10} and the target language is
@;@tech{Landin-knot-lang v10}.
@;
@;The reference implementation takes an optional second parameter specifying an
@;optimization level.
@;@racket[(curryr purify-letrec 0)] (the default) does not treat nested
@;@object-code{letrec} as @tech{simple}.
@;@racket[(curryr purify-letrec 1)] tries to detect @tech{simple} nested
@;@object-code{letrec}s.
@;}

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

Below we define @deftech{Exprs-safe-lang v10}.
We no longer have @object-code{set!} or impure computation; everything must be
let-bound once more.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source" "Exprs-safe-lang v10")
(assigned-lang-v10 exprs-safe-lang-v10)
(exprs-safe-lang-v10)
]

Remember that this pass happens before @racket[implement-safe-primops], and the
safe version of @object-code{vector-set!} must be let-bound since it might
return an error value.

We translate each @deftech{assigned} abstract location into a vector, and each
reference to an @tech{assigned} location to a dereference from the vector, and
each @object-code{set!} to a @object-code{vector-set!}.
For example:
@racketblock[
`(let (info ((assigned (x.1))))
      ([x.1 (void)])
   (let (info ((assigned ())))
     ([tmp.2 (call cons 1 (lambda () x.1))])
     (begin
       (set! x.1 tmp.2)
       x.1)))
_=>
`(let ([x.1 (call make-vector 1)])
   (let ([tmp.2 (call cons 1 (lambda () (call vector-ref x.1 0)))])
     (let ([tmp.3 (call vector-set! x.1 0 tmp.2)])
       (call vector-ref x.1 0))))
]

@nested[#:style 'inset
@defproc[(convert-assigned [p assigned-lang-v10?]) exprs-safe-lang-v10?]{
Implement assigned variables using lower-level heap-allocated mutable data.
}
]
