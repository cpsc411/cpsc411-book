#lang scribble/manual
@(require
  (for-label racket)
  scriblib/figure)

@(define htdp @hyperlink["https://htdp.org"]{@emph{How to Design Programs}})

@title[#:tag "sec:recipe"]{Appendix: Compiler Design Recipe}

Compilers are complex pieces of software, and it can be difficult to know where
to start.
Thankfully, most of you are already indoctrinated into the ways of systematic program design
from CPSC 110, so you know that we should start with the design recipe.  If you are not yet
familiar with systematic program design, don't worry:  you will be.
The weekly assignments in this course were written following the systematic
program design recipe taught in CPSC 110.
By following the design recipe, you will always know where to start.

In this document, we explain how the design recipe applies to compilers,
which pieces of the design recipe you are given, and which pieces you will need
to complete your weekly assignment.

@section{Systematic Program Design for Compilers}
@Figure-ref{fig:recipe} describes a design recipe from
@hyperlink["https://htdp.org/2019-02-24/part_preface.html#%28counter._%28figure._fig~3athe-design-recipe%29%29"]{@emph{How
to Design Programs}}, on which CPSC 110 is based.
@;Figure 1. The basic steps of the a function design recipe. Preface. How to
@;Design Programs, Edition 2019-02-24.

@figure[
"fig:recipe"
@elem{The Design Recipe}
#:style left
@itemlist[#:style 'ordered
@item{From Problem Analysis to Data Definitions}
@item{Signature, Purpose Statement, Header}
@item{Functional Examples}
@item{Function Template}
@item{Function Definition}
@item{Testing}
]
]

The hardest step in this recipe is Step 1: creating a data definition from a
problem analysis.
A compiler is a transformation between languages, so in this course, most data
definitions are language definitions.
The data are programs from a language.
Almost all of the creativity in the compilers we will design is in designing
the languages that the compilers manipulate.
In class, we will analyze some language design problem then discuss design
decisions that led to the source languages we present in the weekly assignment.
You are not yet experienced language developers, so we have done the work of
describing the languages, and will give you these data definitions each week.

We will also give you most of Step 2.
For compilers, Step 2 follows a regular pattern: every compiler pass takes
a source (input) language and produces a target (output) language.
The pass will have one of two purposes.
Either to implement an abstraction for the source in terms of the target, or
to parse some information implicit in the source into an explicit representation
in the target.

From the language definitions you provide, you should complete Step 3 by hand
compiling some programs.
These will also form your tests for Step 6.
The weekly assignments will include a few examples, but it is up to you to come
up with enough examples to test your compiler thoroughly.

Step 4, the template, will be uniform for each compiler you write.
Every compiler is defined over a source language, and each source language has a
common tree structure.
In the rest of this document, I'll explain the template pattern you should use.

By the time you have worked your way through the steps of the design recipe,
there is only one step, Step 5, that requires thinking about writing code.
You will be able to write most of your compiler by following the systematic
process.

@section{The Compiler Template Recipe}
@;A compiler is transformation between languages.
@;The hardest part of the design process is Step 1, creating the data definitions.
@;In this course, the data are languages, so creating data definitions is a
@;language design and specification process.
@;We will deal with at least 13 languages: one for each compiler, one for each week.
@;(In fact, we will have many more intermediate languages.)
@;This requires iteratively refining each language as we add features to the
@;source language to design a better language, and expose features from the target
@;to generate more efficient assembly.
@;In this course, we will design the source and target languages together in
@;class, and you will be given formal language definitions for each compiler you
@;write.

The data definition of each language is essentially a tree, given as a BNF
grammar, which satisfies some properties and can be evaluated to a value.
Each instance of a tree, @emph{i.e.}, each program in the language, satisfies
some common properties, such as having no references to unbound variables.
Each program has an interpretation as a value, given by the interpreter for the
language.
@;For some assignments, we will give you the interpreter, but for others you will
@;write the interpreter.

Since every data structure is a tree, every compiler pass will follow a
template for treelike data.
By following the design recipe, and using the template, systematic design will
write half of your code for you.

The structure of our languages is so regular that we have a recipe for writing a
template for each new language.
You can develop the template for each language using the process in
@Figure-ref{fig:template}.

@figure[
"fig:template"
"Compiler Template Recipe"
#:style left
@itemize[#:style 'ordered
@item{Design predicates for each data non-terminal of the grammar.}
@item{Design one function for each code non-terminal of the grammar.}
@item{Design each structural function by pattern matching on the input
      expression, creating one branch for each case of non-terminal.}
@item{Following the natural recursion: call the corresponding non-terminal
      function on each structural sub-expression.}
@item{Transform the leaves of the tree, @emph{i.e.}, the expressions with no
      complex sub-expressions.}
]
]

For example, consider implementing a compiler for the following two languages.
The compiler should add a subtraction operation to the source language by
compiling it into addition and multiplication.

@figure[
"fig:source"
@elem{@racket[Paren-asm-sub], the source language}
@racketgrammar*[
[p     (begin s ...)]
[s     (set! loc int64)
       (set! loc loc)
       (set! loc (binop loc loc))]
[binop * + -]
[loc   reg addr]
[reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
[addr  (rbp - integer) (rbp + integer)]
]
]

In @Figure-ref{fig:source}, @svar{p} (for @emph{p}rogram) and @svar{s} (for
@emph{s}tatement) are code non-terminals.
These give the structure of code in the language, and are what we recur
on and process when designing a compiler.

The rest, such as @svar{binop}, @svar{reg}, and @svar{int64}, are data
non-terminals.
These define data that appears in the structure of programs, but that the
compiler will typically not process in a complex way.

@figure[
"fig:target"
@elem{@racket[Paren-asm], the target language}
@racketgrammar*[
[p     (begin s ...)]
[s     (set! loc int64)
       (set! loc loc)
       (set! loc (binop loc loc))]
[binop * +]
[loc   reg addr]
[reg   rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
[addr  (rbp - integer) (rbp + integer)]
]
]

Note that @Figure-ref{fig:target} is essentially the same, except @svar{binop}
excludes the symbol @svar{-}.

We would design this compiler using the following template for
Paren-asm-sub.

@codeblock{
#lang racket
(define (TODO . rest) (error "Template not yet completed"))

(define (max-int word-size) (sub1 (expt 2 (sub1 word-size))))
(define (min-int word-size) (* -1 (expt 2 (sub1 word-size))))

(define (int-size? word-size i)
  (and (integer? i)
       (<= (min-int word-size) i (max-int word-size))))

(define (int32? i) (int-size? 32 i))
(define (int64? i) (int-size? 64 i))

(define (Paren-asm-sub-binop? op) (and (member op '(* + -)) #t))
(define (Paren-asm-binop? op) (and (member op '(* +)) #t))
(define (location? loc) (or (register? loc) (address? loc)))
(define (register? r)
  (and
    (member r '(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
    #t))

(define (address? a)
  (match a
    [`(rbp ,op ,int)
      #:when (and (member op '(- +))
                  (integer? int))
     #t]
    [_ #f]))

(define (compiler-sub p)
  (define (process-p p)
    (match p
      [`(begin ,s ...)
       (TODO process-s s)]))
  (define (process-s s)
    (match s
      [`(set! ,loc ,int64)
       #:when (and (location? loc) (int64? int64))
       (TODO)]
      [`(set! ,loc1 ,loc2)
       #:when (and (location? loc1) (location? loc2))
       (TODO)]
      [`(set! ,loc1 (,binop ,loc2 loc3))
       #:when (and (location? loc1)
                   (Paren-asm-sub-binop? '(+ * -))
                   (location? loc2)
                   (location? loc3))
       (TODO)]))
  (process-p p))
}

@margin-note{Racket list predicates, such as @racket[member], often return a
non-boolean result. It's a good idea to force predicates to return a boolean
result using the @racket[(and ... #t)] pattern, to avoid type confusion.}

None of this code required creativity; it is the template induced by the
definition of @racket[Paren-asm-sub].
We have two non-terminals: programs @racket[p] and statements @racket[s], so we
design two functions.
While the language definitions includes other non-terminals in the grammar,
these define atomic data.
Each function pattern matches on the expression using @racket[match].
Nothing interesting happens to programs, so the template for @racket[process-p]
simply calls @racket[process-s] on each statement.
In @racket[process-s], we pattern match, and use @racket[#:when] guards to
disambiguate between statements with similar structure but different atoms in
the expression.
While the last statement is unambigious in the current language definition, it's
a good idea to disambiguate it anyway, as the language definitions may change
throughout the course as we iteratively refine them.
