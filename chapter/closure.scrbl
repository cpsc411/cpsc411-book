#lang scribble/base

@(require
"../assignment/assignment-mlang.rkt"
scriblib/figure
(for-label cpsc411/reference/a9-solution)
(for-label (except-in cpsc411/compiler-lib compile))
cpsc411/langs/v8
(for-label cpsc411/langs/v8)
cpsc411/langs/v9
(for-label cpsc411/langs/v9))

@(provide (all-defined-out))

@declare-exporting[cpsc411/reference/a9-solution]
@(define sb
   (make-cached-eval
    "ch9-eval"
    '(require racket/pretty cpsc411/reference/a9-solution cpsc411/compiler-lib)))

@title[#:tag "top" #:tag-prefix "chp-closures:"]{First-class Procedures: Code is Data}

@section{Preface: What's wrong with Exprs-Lang v8?}
Actually, not much.
With structured data types, @ch8-tech{Exprs-lang v8} is a pretty good language
now.

@ch8-tech{Exprs-bits-lang v8} is sufficiently expressive to act as a reasonable
compiler backend for many languages.
It's roughly at the abstraction level of C.
It provides raw access to memory, pointers, labels (function pointers) and
bitwise operations, but also non-tail calls and some nice syntactic properties
like algebraic expressions.

@ch8-tech{Exprs-lang v8} adds safety on top of that language, although this
safety does come at a cost.
One major limitation in @ch8-tech{Exprs-lang v8} is the lack of abstractions
over computation.
We have lots of abstraction over data, but it's common to want to abstract
over computation---first-class procedures, objects, function pointers, etc.
@ch8-tech{Exprs-lang v8} prevents even passing function pointers to ensure safety.

In this chapter, we add the ability to easily abstract over computations at any point
via first-class procedures.
Many languages provide some version of this---Python, JavaScript, Ruby, Racket,
Scheme, Java, and many more.
They enable the programmer to create a suspended computation, and pass it around
as a value.
The procedure closes over the environment in which it was created, capturing any
free variables and essentially creating an object with private fields.
They can be used as the foundations for object systems, to represent call backs
or continuations, and provide a safe, lexically scoped alternative to function
pointers.

In @deftech{Exprs-lang v9}, we add first-class procedures as values:
@bettergrammar*-ndiff[
#:labels ("Diff vs v8 (excerpts)" "Exprs-lang v9")
(#:exclude (fixnum uint8 ascii-char-literal) exprs-lang-v8 exprs-lang-v9)
(exprs-lang-v9)
]

Now, @exprs-lang-v9[lambda] can appear in any expression.
We can still define procedures at the top-level using @exprs-lang-v9[define],
although the semantics will change.

We add a new data structure to the language as well: @exprs-lang-v9[procedure]s.
These are implicitly constructed by @exprs-lang-v9[lambda].
They support two operations, in addition to @exprs-lang-v9[call]: the
tag-checking predicate @exprs-lang-v9[procedure?] and
@exprs-lang-v9[procedure-arity], for inspecting how many parameters the
procedure expects take.

This is a syntactically small change, but it has massive implications.

@section{Procedures, Closures and Closure Conversion}
So far, procedures in our language have been compiled directly to labeled
@deftech{code}---a suspended computation that is closed except for its declared
parameters.
We have not treated procedures as values, nor considered what happens if a
procedure appears in value context.
The closest representation of the value of a procedure we had was the label to
its @tech{code}.
In earlier source languages, we disallowed passing procedures as values, to
ensure safety.

@subsection{The First-class Procedure}
To support first-class procedures, we need to compile procedures to a
data structure.
This data structure allows us to construct a procedure value, pass it around and
return it, call it (safety), and captures both the procedure's @tech{code}, but
also any information we need about the procedure.

To add a new data structure, we need a new @ch7-tech{primary tag} and a new
collection of primitive operations.
We use the tag @code{#b010} for procedures.

Here is our updated list of tags:
@itemlist[
@item{@code{#b000}, @deftech{fixnums}, fixed-sized integers}
@item{@code{#b001}, @ch8-tech{pairs}}
@item{@code{#b010}, (first-class) @tech{procedures}}
@item{@code{#b011}, @ch8-tech{vectors}}
@item{@code{#b100}, @emph{unused}}
@item{@code{#b101}, @emph{unused}}
@item{@code{#b110}, non-fixnum immediates (booleans, etc)}
@item{@code{#b111}, @emph{unused}}
]

In the source language, we expose the primitive operations @exprs-lang-v9[procedure?] and
@exprs-lang-v9[procedure-arity].
However, the compiler intermediate languages expose a few more operations
that the compiler needs to make use of to implement procedure calls.

Every instance of @exprs-lang-v9[lambda] compiles to a procedure.
The procedure now has three pieces of information: its arity for dynamic
checking; the label to its @deftech{code}, the computation it executes when
invoked; and its @deftech{environment}, the values of the free variables used in
the definition of the procedure.
We compile each application of a procedure to dereference and call the label of
the procedure, but also to pass a reference to the procedure itself as a parameter.
Essentially, the procedure is an object, and receives itself as an argument.
Each "free variable" @tt{x} is a field of that object, and are compiled to
references to @tt{self.x}.

The low-level first-class @deftech{procedure} interface is described below:
@itemlist[
@item{@proc-exposed-lang-v9[(make-procedure value_label value_arity value_size)]

Creates a procedure whose label is @proc-exposed-lang-v9[value_label], which expects
@proc-exposed-lang-v9[value_arity] number of arguments, and has an environment of size
@proc-exposed-lang-v9[value_size].

@proc-exposed-lang-v9[make-procedure] does not perform any error checking; it must be
applied to a label and two fixnum @ch7-tech{ptrs}.
This is safe because no user can access @proc-exposed-lang-v9[make-procedure]
directly.
Only the compiler generates uses of this operator, and surely our compiler uses
it correctly.

In the source language, @proc-exposed-lang-v9[make-procedure] is not exposed
directly; instead, @exprs-lang-v9[lambda] is compiled down to this primitive.
}
@item{@proc-exposed-lang-v9[(unsafe-procedure-ref value_proc value_index)]

Return the value at index @proc-exposed-lang-v9[value_index] in the environment of the
procedure @proc-exposed-lang-v9[value_proc].

As with all unsafe operators, this does not perform any checking.

In the source language, @proc-exposed-lang-v9[unsafe-procedure-ref] is not
exposed directly.
This is used to access variables outside of the procedure's scope, but in scope
at the time the procedure is created.
We use this to implement @tech{closures}, which we describe shortly.
}
@item{@proc-exposed-lang-v9[(unsafe-procedure-set! e_proc e_index e_val)]

Set the value at index @proc-exposed-lang-v9[value_index] in the environment of the
procedure @proc-exposed-lang-v9[value_proc] to be @proc-exposed-lang-v9[value_val].

In the source language, @proc-exposed-lang-v9[unsafe-procedure-set!] is not
exposed directly.
}
@item{@proc-exposed-lang-v9[(unsafe-procedure-label value_proc)]

Returns the label to the @tech{code} for the procedure @proc-exposed-lang-v9[value_proc].
}
@item{@proc-exposed-lang-v9[(call value_label value ...)]

Call the @tech{code} whose label is @proc-exposed-lang-v9[value_label] with the
arguments @proc-exposed-lang-v9[value].

This is essentially the same as the @exprs-lang-v9[call] primitive in previous
chapters, although we now allow labels to be computes and passed as values.
It is unsafe and with no dynamic checks, so some earlier pass must insert
dynamic checks to ensure safety.
}
]

Our procedure data structure is essentially a vector containing a label to the
@tech{code} and the values of each free variable in its @tech{environment}.

The challenge in implementing procedures is primarily in compiling
@exprs-lang-v9[lambda] down to the procedure primitives, then specifying the
representation of these procedure primitives in terms of calls to labelled
@tech{code}.
All compiler passes below @racket[specify-representation] remain unchanged.

Until now, all procedures were bound at the top-level in a set of
mutually-recursive definitions.
To work with first-class procedures in intermediate languages, we need to be
able to represent sets of mutually recursive definitions that appear @emph{as
expressions}.
We introduce the @closure-lang-v9[letrec] construct to aid with this.
@closure-lang-v9[(letrec ([aloc e] ...) e_2)] binds each @closure-lang-v9[aloc]
in each @closure-lang-v9[e], including its own right-hand-side, as well as
binding all @closure-lang-v9[aloc]s in @closure-lang-v9[e_2].
For now, we only consider a restricted form of @closure-lang-v9[letrec] that
only binds procedures.

@subsection{The Closure}
Our procedure data structure implements a @deftech{closure}, a
procedure's @tech{code} paired with the values of free variables from the
environment in which the procedure was created.
This allows us to create procedures that refer to variables outside of their own
scope, but still retain references to those variables even when the procedure is
passed to a different scope.

As an intermediate step in compiling first-class procedures, we introduce
explicit @tech{closure} primitives which compile to the procedure primitives.
There is no primary tag for this data strucure, since it will be implemented by
the lower-level procedure data type.

@tech{Closures} support two operations.
First, you can call a @tech{closure} with @closure-lang-v9[(closure-call e es ...)], which
essentially extracts the label from the @tech{closure} @closure-lang-v9[e]
and calls the procedure at that label with the argument @closure-lang-v9[(es ...)].
Second, you can dereference an @tech{environment} variable from the
@tech{closure} with @closure-lang-v9[(closure-ref e value_i)], extracting the value
at index @closure-lang-v9[value_i] from the @tech{environment} of the @tech{closure}
@closure-lang-v9[e].

Because we want to implement safe procedure application, we add a third field to
the @tech{closure}: its @deftech{arity}, the number of arguments expected by
the @tech{code} of the @tech{closure}.

The @tech{closure} interface is described below:
@itemlist[
@item{@closure-lang-v9[(make-closure value_label value_arity value_i ...)]

Creates a @tech{closure} whose @tech{code} is at label @closure-lang-v9[value_label],
which expects @closure-lang-v9[value_arity] number of arguments, and has the values
@closure-lang-v9[value_i] in its @tech{environment}.
}
@item{@closure-lang-v9[(closure-call value_c value ...)]

Safely call the @tech{closure} @closure-lang-v9[value_c], invoking its @tech{code}, with
the arguments @closure-lang-v9[(value ...)].}
@item{@closure-lang-v9[(closure-ref value_c value_i)]

Deference the value at index @closure-lang-v9[value_i] in the @tech{environment} of the
@tech{closure} @closure-lang-v9[value_c].
Since this dereference is only generated by the compiler, it always succeeds and
performs no dynamic checks.
The environment is 0-indexed.
}
]
Each of these primitives compile down to the analogous procedure primitives.

@subsection{Closure Conversion}
The main problem with compiling first-class procedure is that we need to lift
their code to the top-level, but they have references to free variables which go
out of scope if we move the procedure definition.
We deal with this by converting all procedures to @tech{closures}, rebinding the
free variables in the @tech{code} as explicit dereferences from the
@tech{closure}'s @tech{environment}, then lifting the now-closed @tech{code}
definitions to the top-level.
This process is called @deftech{closure conversion}.

@margin-note{@tech{Closure conversion} is not the only way to implement
first-class procedures.
An alternative that can avoid some of the allocation cost of @tech{closures} is
@emph{defunctionalization}, but this does not work well with separate
compilation.}

Before we can perform @tech{closure conversion}, we must discover which variables in a
@closure-lang-v9[lambda] are @tech{free} with respect to the procedure's scope.
We first annotation all @closure-lang-v9[lambda] with their free variable sets.
@racketblock[
`(lambda (,alocs ...) ,value)
=>
`(lambda ,(info-set '() 'free (set-subtract (free-var value) alocs))
         (,alocs ...) ,value)
]
We add a pass to perform this just prior to @tech{closure conversion}.

A variable is considered @deftech{free} in a scope if it is not in the set
of variables bound by that scope, if it is referenced in any expression in
which the scope binds variables, and if the reference is not @tech{bound}.
A variable is @deftech{bound} if it is refernced inside a scope for which it is
declared in the set of variables bound by that scope.

In our languages, @closure-lang-v9[lambda], @closure-lang-v9[let], and
@closure-lang-v9[letrec] introduce new scopes.
Calcuating the free variables of an expression is relatively straightforward,
but we have to be careful with the binding structures of
@closure-lang-v9[letrec] and @closure-lang-v9[let].
@margin-note{Note that all variables are @tech{bound}, which is enforced by
@racket[check-exprs-lang], but they can be @tech{free} relative to a particular
scope.}

There are two parts to @tech{closure conversion}:
@itemlist[
@item{Transform each @closure-lang-v9[lambda].
Each @closure-lang-v9[lambda] is transformed to take a new formal parameter, which
is its closure, and to be bound to a @closure-lang-v9[label] in its enclosing
@closure-lang-v9[letrec].
We can think of this as adding a @tt{this} or @tt{self} argument to each procedure.

The @ch2-tech{abstract location} to which the the @closure-lang-v9[lambda] was
previously bound must now be bound to a closure.
The closure has @tt{n + 2} fields, where @racket[n] is the number of free
variables in the @closure-lang-v9[lambda].
The first field is the label to which the closure's @tech{code} is bound.
The final @tt{n} fields are references to the lexical variables in the
@tech{environment} of the closure.

In essence, we transform
@racketblock[
`(letrec ([,x (lambda ((free (,ys ...))) (,xs ...)  ,values)] ...)
   ,value)
_=>
`(letrec ([,l (lambda (,c ,xs ...)
                (let ([,ys (closure-ref ,c ,i)] ...)
                  ,value))] ...)
    (cletrec ([,x (make-closure ,l ,(length xs) ,ys ...)] ...)
      ,value))
]
where @racket[l] is a fresh label and @racket[c] is a fresh abstract location.
The @closure-lang-v9[cletrec] form is like @closure-lang-v9[letrec]
but restricted to bind @tech{closures}.
We add the number of arguments as a field in the @tech{closure} to implement
safe application later.
}
@item{Transform each @closure-lang-v9[call].
Every procedure now takes an extra argument, its closure, so we have to expand
each @closure-lang-v9[call].
The essence of the translation is:
@racketblock[
`(call ,value ,values ...)
_=>
`(let ([,x ,value])
   (closure-call ,x ,x ,values ...))
]
We use @closure-lang-v9[closure-call] to call the (label of the) @tech{closure}
to the @tech{closure} itself and its usual arguments.
We need to bind the operator to avoid duplicating code.
}
]

@section{Administrative Passes}
Allowing procedures to be bound in two different ways is great for programmer
convenience, but annoying for a compiler writer.
Before we get to implementing procedures, we simplify and
regularize how procedures appear in our language.

The first big benefit to the programmer comes in @racket[check-exprs-lang].
Since we finally have a procedure data type, and procedure primitives to enable
dynamic checking, we can finally stop type checking programs.
Now, it is valid and does not cause undefined behaviour to pass procedures are
arguments, return procedures, or call an arbitrary variables with an arbitrary
number of arguments.
The language will dynamically check whether any of those expressions is safe
before attempting to execute them

@nested[#:style 'inset
@defproc[(check-exprs-lang [p any]) exprs-lang-v9]{
Validates that input is a well-bound @tech{Exprs-lang v9} program.
There are no other static restrictions.
}
]

As usual with @racket[uniquify], the only change is that all names
@exprs-lang-v9[x] are replaced by abstract locations
@exprs-unique-lang-v9[aloc].

Unlike in previous versions, there are no @exprs-lang-v9[label]s after
@racket[uniquify].
All of our procedures are data, not merely code, and cannot easily be lifted to
the top level yet, so it is now the job of a later pass to introduce labels.

Below we define @deftech{Exprs-unique-lang v9}.
We typeset the changes with respect to @tech{Exprs-lang v9}.

@bettergrammar*-ndiff[
#:labels ("Diff vs v8 (excerpts)" "Diff vs Source Diff (excerpts)" "Exprs-unique-lang v9")
(#:exclude (prim-f aloc label fixnum uint8 ascii-char-literal)
  exprs-unique-lang-v8 exprs-unique-lang-v9)
(#:exclude (prim-f aloc label fixnum uint8 ascii-char-literal)
 exprs-lang-v9 exprs-unique-lang-v9)
(exprs-unique-lang-v9)
]

@nested[#:style 'inset
@defproc[(uniquify [p exprs-lang-v9]) exprs-unique-lang-v9]{
Resolves all @ch3-tech{lexical identifiers} into unique @ch2-tech{abstract
locations}.
}]

Not much changes in @racket[implement-safe-primops].
The target language of the pass, @deftech{Exprs-unsafe-data-lang v9}, is defined
below.

@bettergrammar*-ndiff[
#:labels ("Diff vs v8 (excerpts)" "Diff vs Source (excerpts)" "Exprs-unsafe-data-lang v9")
(#:exclude (aloc label fixnum uint8 ascii-char-literal)
  exprs-unsafe-data-lang-v8 exprs-unsafe-data-lang-v9)
(#:exclude (aloc label fixnum uint8 ascii-char-literal)
  exprs-unique-lang-v9 exprs-unsafe-data-lang-v9)
(exprs-unsafe-data-lang-v9)
]

Note that this pass does not implement safe @exprs-unsafe-data-lang-v9[call],
but can be safely applied to arbitrary data---a later pass will implement
dynamic checking for application.

@nested[#:style 'inset
@defproc[(implement-safe-primops [p exprs-unique-lang-v9?])
         exprs-unsafe-data-lang-v9?]{
Implement safe primitive procedures by inserting procedure definitions for each
primitive operation which perform dynamic tag checking, to ensure type and
memory safety.
}]

Now we implement @exprs-unsafe-data-lang-v9[call] in terms of
@exprs-unsafe-lang-v9[unsafe-procedure-call] and
@exprs-unsafe-lang-v9[unsafe-procedure-arity].
Note that we cannot simply define @exprs-unsafe-data-lang-v9[call] as a procedure,
like we did with other safe wrappers, since it must count its arguments, and we
must support a variable number of arguments to the procedure.

Next, we implement safe @exprs-unsafe-data-lang-v9[call].
Below we define @deftech{Exprs-unsafe-lang v9}.
@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Exprs-unsafe-lang v9")
(#:exclude (primop triv pred aloc fixnum uint8 ascii-char-literal)
 exprs-unsafe-data-lang-v9 exprs-unsafe-lang-v9)
(exprs-unsafe-lang-v9)
]

We implement @exprs-unsafe-data-lang-v9[call] in terms of @exprs-unsafe-lang-v9[procedure?],
@exprs-unsafe-lang-v9[unsafe-procedure-call] and @exprs-unsafe-lang-v9[unsafe-procedure-arity].
The essence of the transformation is:
@racketblock[
`(call ,e ,es ...)
_=>
`(if (procedure? ,e)
     (if (eq? (unsafe-procedure-arity ,e) ,(length es))
         (unsafe-procedure-call ,e ,es ...)
         ,bad-arity-error)
     ,bad-proc-error)
]

@margin-note{If we statically track the arity of @tech{procedures}, we can
optimize this transformation in some cases.}

@;We subtract one from the length of the parameter list to account for the closure
@;parameter.
@;We could equivalently add one to the procedure arity, but since the length of
@;the parameter list is known at compile-time, this saves us at least one run-time
@;instruction.
@;
@;@digression{
@;This pass assumes the closure argument must always be there.
@;This design prevents us from optimizing away the closure parameter easily, a
@;slight annoyance that is due to your professor missing this design mistake
@;before releasing the assignment.
@;A better design would place this pass before closure conversion, exposing
@;@object-code{unsafe-apply} earlier.
@;Then we would have access to the correct arity count without the closure
@;argument, and closure conversion modify the @object-code{unsafe-apply} form
@;without disrupting @object-code{procedure-arity}.
@;
@;We do not want to capture the closure parameter in the
@;@object-code{procedure-arity} value, since this value is exposed to a user, and
@;we do not want the user to know about the internal closure parameter.
@;This internal parameter is not part of their code, so we should not burden them
@;with it.
@;}

@nested[#:style 'inset
@defproc[(implement-safe-call [p exprs-unsafe-data-lang-v9?])
         exprs-unsafe-lang-v9?]{
Implement @exprs-unsafe-data-lang-v9[call] as an unsafe procedure call with
dynamic checks.
}
]

Some procedures now appear in local expressions, and some appear defined at the
top-level.
This presents two problems.
First, our compiler later assumes that all @emph{data} (as opposed to code) is
@emph{locally} defined---we have no way to define top-level, labelled data.
Since procedures are data, we need to transform top-level bindings of procedures
into local bindings, so the rest of the compiler will "just work".
Second, our compiler later assumes that all @emph{code} (as opposed to data) is
@emph{globally} defined---we expect code to appear only at the top-level.
That's not true when @exprs-lang-v9[lambda] is used in a local expression.

To deal with these, we introduce two small administrative passes:
@racket[define->letrec] and @racket[dox-lambdas].

First, in @racket[define->letrec], we elaborate @just-exprs-lang-v9[define] into
a local binding form @just-exprs-lang-v9[letrec], which will be used to bind all
procedures.

@just-exprs-lang-v9[letrec], unlike @just-exprs-lang-v9[let], supports multiple bindings in a
single form, and each bound expression can refer to any variable in the set of
bindings for the @just-exprs-lang-v9[letrec].
This is important to capture mutually-recursive functions, and has the same
binding structure as our top-level @just-exprs-lang-v9[define]s.

@digression{
In general, a language might impose additional semantics on
@just-exprs-lang-v9[define], such as allowing @just-exprs-lang-v9[define]d data
to be exported and imported at module boundaries.
This would require additional handling of @just-exprs-lang-v9[define], and the ability
to generate labelled data in the back-end of the compiler.
We continue to ignore separate compilation and linking, so we treat
@just-exprs-lang-v9[define] as syntactic sugar for @just-exprs-lang-v9[letrec].
}

Below we define @deftech{Just-Exprs-lang v9}.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Just-exprs-lang v9")
(#:exclude (primop aloc label fixnum uint8 ascii-char-literal)
 exprs-unsafe-lang-v9 just-exprs-lang-v9)
(just-exprs-lang-v9)
]

@nested[#:style 'inset
@defproc[(define->letrec [p exprs-unsafe-lang-v9?])
         just-exprs-lang-v9?]{
Transform all top-level bindings into local bindings.
}]

Before we start compiling @just-exprs-lang-v9[lambda]s, we should try to get rid of
them.
@emph{Direct calls} to @just-exprs-lang-v9[lambda]s, such as @racket[(call (lambda (x)
x) 1)], are simple to rewrite to a @just-exprs-lang-v9[let] binding, such as
@racket[(let ([x 1]) x)].
A human programmer may not write this kind of code much, but most programs are
not written by humans---compilers write far more programs.
This optimization will speed-up compile time and run time for such simple
programs.

@digression{
This optimization is a special case of procedure inlining.
A direct call to a @tech{procedure} guarantees the @tech{procedure} occurs
exactly once, and is therefore completely safe to inline.
This pass could be replaced by a more general inlining optimization.
}

@nested[#:style 'inset
@defproc[(optimize-direct-calls [p just-exprs-lang-v9?])
         just-exprs-lang-v9?]{
Inline all direct calls to first-class procedures.
}
]

Next, we explicitly name all lambdas in @racket[dox-lambdas].
The source language supports anonymous procedures, that is, first-class
procedure values that are not necessarily bound to names.
For example, we can write the following in Racket, creating and using procedures
without ever binding them to names in a @just-exprs-lang-v9[letrec] or
@just-exprs-lang-v9[let] form.
@examples[
((lambda (x f) (f x x)) 1 (lambda (x y) (+ x y)))
]

The equivalent in @tech{Exprs-lang v9} is:
@exprs-lang-v9-block[
(call (lambda (x f) (call f x x)) 1 (lambda (x y) (call + x y)))
]

This is great for functional programmers, who value freedom, but bad for
compilers who want to keep track of everything.

We bind all procedures to names to simplify lifting code to the top-level and
assigning labels later.

We transform each @racket[`(lambda (,alocs ...) ,e)] into @racket[`(letrec
([,tmp (lambda (,alocs ...) ,e)]) ,tmp)], where @racket[tmp] is a fresh
@object-code{aloc}.

We define @deftech{Lam-opticon-lang v9}, in which we know the name of every
procedure.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Full")
(#:exclude (primop pred aloc fixnum uint8 ascii-char-literal)
 just-exprs-lang-v9 lam-opticon-lang-v9)
(lam-opticon-lang-v9)
]

@nested[#:style 'inset
@defproc[(dox-lambdas [p just-exprs-lang-v9?])
         lam-opticon-lang-v9?]{
Explicitly binds all procedures to @ch2-tech{abstract locations}.
}]

@section{Implementing Closure Conversion}
The rest of our compiler expects procedures to be little more than labeled
blocks of code.
Unfortunately, now our first-class procedures can contain references to
@tech{free} variables in their lexical scope.
This means we cannot simply lift first-class @tech{procedure} definitions to the
top-level, stick on a label, and generate a labeled @ch6-tech{procedure}.

First, we uncover the @tech{free} variables in each @lam-free-lang-v9[lambda].
We add these as an annotation on the @lam-free-lang-v9[lambda], which the next
pass usees to generate @tech{closures}.

Below we define @deftech{Lambda-free-lang v9}.
@bettergrammar*-ndiff[
#:labels ("Source/Target Diff (excerpts)" "Full")
(#:exclude (pred effect primop aloc label fixnum uint8 ascii-char-literal)
 lam-opticon-lang-v9 lam-free-lang-v9)
(lam-free-lang-v9)
]

To find the @tech{free} @ch2-tech{abstract locations}, we traverse the body of each
@lam-free-lang-v9[lambda] remembering any @ch2-tech{abstract locations} that
have been @tech{bound} (by @lam-free-lang-v9[let], @lam-free-lang-v9[lambda], or
@lam-free-lang-v9[letrec]), and return the set of @ch2-tech{abstract locations}
that have been used but were not in the defined set.
On entry to the @lam-free-lang-v9[(lambda (aloc ...) e)], only the formal parameters
@lam-free-lang-v9[(aloc ...)] are considered @tech{bound}.

@nested[#:style 'inset
@defproc[(uncover-free [p lam-opticon-lang-v9?])
         lam-free-lang-v9?]{
Explicitly annotate procedures with their free variable sets.
}]

The only complicated case is for @lam-free-lang-v9[letrec].
Even a variable @tech{bound} in a @lam-free-lang-v9[letrec] is considered
@tech{free} in the body of a @lam-free-lang-v9[lambda].
@examples[#:eval sb
(uncover-free
 `(module
    (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
      x.1)))
]

However, the @closure-lang-v9[letrec] does bind those variables, so they do not
contribute to the free variable set for the context surrounding the
@closure-lang-v9[letrec].
@examples[#:eval sb
(uncover-free
 `(module
    (letrec ([f.1 (lambda ()
                    (letrec ([x.1 (lambda () (unsafe-procedure-call x.1))])
                      x.1))])
      f.1)))
]

After we know the @tech{free} variables, we make @tech{closures} explicit.

Strictly speaking, all the previous languages had
@deftech{closures}---procedures that (implicitly) close over their lexical
environment.
However, our earlier languages forbid us from ever creating procedures that had
a non-empty environment, so all our @tech{closures} were trivial to compile
to labelled code.
@tech{Closure conversion} is the process of compiling first-class procedures
into an explicit @tech{closure} data type.

Below, we define @deftech{Closure-lang v9}.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Closure-lang v9")
(#:exclude (primop pred effect aloc fixnum uint8 ascii-char-literal)
 lam-free-lang-v9 closure-lang-v9)
(closure-lang-v9)
]

@tech{Closure conversion} changes @closure-lang-v9[letrec] to bind labels to procedure
code.
After this pass, the body of @closure-lang-v9[lambda] does not contain any
@tech{free} variables, and will not be a procedure data type---it is just like a
@ch5-tech{procedure} from @ch6-tech{Values-lang v6}.

To encode @tech{closures}, we temporarily add a new data type for
@tech{closures}, which we compile to a lower-level data type.
We add a new form, @closure-lang-v9[cletrec], which only binds @tech{closures}.
@tech{Closures} can, in general, have mutually recursive references, so this is
a variant of the @closure-lang-v9[letrec] form.
We also add a new form for dereferencing the value of @tech{free} variables from
the @tech{closure} @closure-lang-v9[(closure-ref e e)].

We assume that the @closure-lang-v9[cletrec] form only ever appears as the body
of a @closure-lang-v9[letrec] form, but we do not make this explicit in the
syntax for readability.
This assumption is not necessary for correctness, but simplifies an
optimization presented later.

We add @closure-lang-v9[call], the primitive operation for calling a label
directly, to enable optimizing @tech{closures}, an important optimization.

@nested[#:style 'inset
@defproc[(convert-closures [p lam-free-lang-v9?])
         closure-lang-v9?]{
Performs @tech{closure conversion}, converting all procedures into explicit
@tech{closures}.
}]

@margin-note{
If the operator is already an @closure-lang-v9[aloc], we can avoid introducing
an extra @closure-lang-v9[let]:
@racketblock[
`(unsafe-procedure-call ,_aloc ,_values ...)
_=>
`(closure-call ,_aloc ,_aloc ,_values ...)
]
This also simplifies the optimization @racket[optimize-known-calls].
}

Closures can cause a lot of indirection, and thus performance penalty.
We essentially transform all calls into @emph{indirect calls}.
This causes an extra memory dereference and indirect jump, both of which can
have performance penalties.

Many calls, particularly to named functions, can be optimized to direct calls.
We essentially perform the following transformation on all calls where we can
determine the label of the operator:
@racketblock[
`(closure-call ,_value ,_values ...)
_=>
`(call ,_label ,_values ...)
]
where @closure-lang-v9[label] is known to be the label of the closure
@closure-lang-v9[value].
Because @closure-lang-v9[value] is already an @closure-lang-v9[aloc], we can
safely discard it; we do not need to force evaluation to preserve any side
effects.

We perform this optimization by recognizing @closure-lang-v9[letrec] and
@closure-lang-v9[cletrec] as a single composite form:
@racketblock[
`(letrec ([,_label_c ,_lam])
   (cletrec ([,_aloc_c (make-closure ,_label_c ,_values ...)])
     ,_value))
]
All uses of @closure-lang-v9[(closure-call aloc_c values ...)] in
@closure-lang-v9[value] and @closure-lang-v9[lam] can be transformed into
@closure-lang-v9[(call label_c values ...)].
We have to recognize these as a single composite form to optimize recursive
calls inside @closure-lang-v9[lam], which will benefit the most from the optimization.
This relies on the name @closure-lang-v9[aloc_c] binding in two places: once to
define the @tech{closure}, and once when dereferenced in a recursive closure.

@nested[#:style 'inset
@defproc[(optimize-known-calls [p closure-lang-v9?])
         closure-lang-v9?]{
Optimizes calls to known @tech{closures}.
}
]

Now that all @closure-lang-v9[lambda]s are closed and labelled, we can lift them to
top-level @hoisted-lang-v9[define]s.

We define @deftech{Hoisted-lang v9} below.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Hoisted-lang v9")
(#:exclude (pred effect primop v aloc label fixnum uint8 ascii-char-literal)
 closure-lang-v9 hoisted-lang-v9)
(hoisted-lang-v9)
]

The only difference is the @hoisted-lang-v9[letrec] is remove and
@hoisted-lang-v9[define] blocks are re-added.

@nested[#:style 'inset
@defproc[(hoist-lambdas [p closure-lang-v9?])
         hoisted-lang-v9?]{
Hoists @tech{code} to the top-level definitions.
}
]

Now we implement @tech{closures} as the procedure data structure described
earlier.
Our procedure data structure represents a value that can be called, and has some
associated information useful for dynamic type checking.

A procedure looks like an extension of a vector.
It has at least three fields: the label, the arity, and a size.
The size indicates how large the environment of the procedure is.
The environment will be uninitialized after @proc-exposed-lang-v9[make-procedure], and
instead the environment will be initialized manually using
@proc-exposed-lang-v9[unsafe-procedure-set!], similar to vector initialization.
As with @tech{closures}, @proc-exposed-lang-v9[unsafe-procedure-label]
dereferences the label and @proc-exposed-lang-v9[unsafe-procedure-ref] dereferences a
value from the procedure's environment, given an index into the environment.
However, we also have @proc-exposed-lang-v9[unsafe-procedure-arity] to dereference
the arity of a procedure.
@todo{Talk about Landin's Knot?}

The language @deftech{Proc-exposed-lang v9} is defined below.
@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Proc-exposed-lang v9")
(#:exclude (pred effect aloc fixnum uint8 ascii-char-literal)
 hoisted-lang-v9 proc-exposed-lang-v9)
(proc-exposed-lang-v9)
]

To transform closures into procedures, we do a three simple translations:
@itemlist[
@item{Transform @hoisted-lang-v9[make-closure]
@racketblock[
`(cletrec ([,_aloc (make-closure ,_label ,_arity ,_values ...)] ...)
   ,_value)
_=>
`(let ([,_aloc (make-procedure ,_label ,_arity ,_n)] ...)
   (begin
     (unsafe-procedure-set! ,_aloc 0 ,(list-ref _values 0))
     ...
     (unsafe-procedure-set! ,_aloc ,_n ,(list-ref _values n))
     ,_value))
]
where @racket[_n] is @racket[(length _values)], the number of values in the
environment.
}
@item{Transform @hoisted-lang-v9[closure-ref].
@racketblock[
`(closure-ref ,_c ,_i)
_=>
`(unsafe-procedure-ref ,c_ ,_i)
]
We can use @proc-exposed-lang-v9[unsafe-procedure-ref] since we generate all uses of
@hoisted-lang-v9[closure-ref].
}
@item{Transform @hoisted-lang-v9[closure-call].
@racketblock[
`(closure-call ,_c ,_values ...)
_=>
`(call (unsafe-procedure-label ,_c) ,_values ...)
]
Recall that @hoisted-lang-v9[closure-call] is generated from an
@lam-free-lang-v9[unsafe-procedure-call], so it is equally safe to call
@proc-exposed-lang-v9[unsafe-procedure-label].
}
]

@nested[#:style 'inset]{
@defproc[(implement-closures [p hoisted-lang-v9])
         proc-exposed-lang-v9]{
Implement @tech{closures} in terms of the procedure data structure.
}
}

Finally, we need to implement procedure data type.
It is intentionally designed to be similar to the vector data type.

The target language is @tech{Exprs-bits-lang v8}, which is unchanged
from the previous chapter.

@bettergrammar*-ndiff[
#:labels ("Diff vs Source (excerpts)" "Full")
(proc-exposed-lang-v9 exprs-bits-lang-v8)
(exprs-bits-lang-v8)
]

When implementing @proc-exposed-lang-v9[make-procedure], we assume the size of the
environment is a fixnum constant, since this is guaranteed by how our compiler
generates @proc-exposed-lang-v9[make-procedure]

@nested[#:style 'inset
@defproc[(specify-representation [p proc-exposed-lang-v9?])
         exprs-bits-lang-v8?]{
Compiles data types and primitive operations into their implementations as
@ch7-tech{ptrs} and primitive bitwise operations on @ch7-tech{ptrs}.
}
]

@section[#:tag "sec:overview"]{Appendix: Overview}

@define[v9-graph
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
@figure["fig:v9-graph" "Overview of Compiler Version 0" v9-graph]
