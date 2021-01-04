#lang scribble/base
@(require
  "../assignment/assignment-mlang.rkt"
  scribble/core
  scribble/examples
  (for-label rackunit))

@title[#:tag "top" #:tag-prefix "notes:match-tutorial"]{Match Tutorial}
In this course, we define all languages using s-expressions, and encode them
using Racket lists and symbols.
To pattern match on language forms, we use Racket's @racket[match].
We'll primarily use @racket[quasiquote], @racket[unquote], @racket[...],
@racket[_] patterns, and the @racket[#:when] clause to disambiguate.

Using lists and @racket[match] has some advantages over creating new data types
for each language.
We can write program abstract syntax trees (ASTs) directly using
@racket[quasiquote], without worrying about the constructors for each language's
data type.
The syntax is also more flexible, since we do not have fixed constructors.
We can easily print the output of compilers, and transparently use functions
over lists as functions on programs.

It has a few disadvantages though.
The primary disadvantage is that @racket[match] cannot check that we've covered
all cases of a nonterminal.
This means we must be extra careful to follow the design recipe and
disambiguate every match case, or we'll get annoying run-time errors or
mysterious failures for ambiguous cases in the language.

As a simple example of using @racket[match], the function below counts the nodes
in a tree of integers.
This uses all of the features you are expected to know when writing compilers
over s-expressions.

@examples[
(require racket/match)
(code:comment "Tree -> Natural Number")
(code:comment "Expects a tree t and returns the number of nodes in the tree.")
(code:comment "Behavior is undefined if not given a valid tree.")
(define (int-tree-nodes t)
  (match t
    [i #:when (integer? i) 1]
    [`(,x ...) (apply + (map int-tree-nodes x))]
    [_ (error "Invalid tree" t)]))

(eval:check (int-tree-nodes `((1 2) ((3 4 (5 1 0)) (1 8 9)))) 10)
(eval:error (int-tree-nodes "hello"))
]

See @secref["match" #:doc '(lib "scribblings/guide/guide.scrbl")] for a more
thorough tutorial on match.
