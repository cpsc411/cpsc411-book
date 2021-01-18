#lang scribble/base

@(require
  "../assignment/assignment-mlang.rkt"
  cpsc411/langs/v1
  (for-label cpsc411/langs/v1)
  (for-label cpsc411/reference/a1-solution)
  (for-label (except-in cpsc411/compiler-lib compile)))

@(provide
  (except-out (all-defined-out) eg))

@declare-exporting[cpsc411/reference/a1-solution]

@(define eg
   (make-cached-eval
    "a1-eval"
    '(require
      cpsc411/reference/a1-solution
      (except-in cpsc411/compiler-lib compile))))

@title[#:tag "top" #:tag-prefix "chp-boilerplate:"]{The First Compiler: Abstracting Boilerplate}
Once we have picked a @ch1-tech{target language}, a language from which to start
building, we have fixed some set of abstractions.
From this starting point, we ask a question:
@todo{Also talk about validators and undefined behaviour in this chapter. Should
that be separate from abstracting boilerplate?}

@nested[#:style 'inset]{
  What's wrong with this language?
}

Our goal in designing and implementing a language is to systematically design
and build new layers of abstractions.
These abstractions are meant to solve some problem, such as software development
being error-prone, software design being complex, or software produced in the
language being unsafe, unportable, or verbose.
Ideally, we solve these problems without introducing some cost, such as a high
learning curve, or introducing some performance cost.
To solve these problems, we must first concretely identify one, and then design a
layer of abstraction to address it.

The first limitation in @ch1-tech{x64} we identify is boilerplate.
Writing programs in @ch1-tech{x64} requires the programmer to insert repetitive
boilerplate, such as the declaration of the initial label, and some code to exit
the program and report the result to the user.
This boilerplate prevents the user from focusing on the program and requires
them to copy and paste the same snippets of code into their programs, an error-prone
process if that snippet ever needs to change.

To address this, we design a new @ch1-tech{source language} that is free of this
boilerplate, and design a compiler to transform the @ch1-tech{source language}
into the @ch1-tech{target language} by introducing the boilerplate.

@section{Designing an abstraction}
Our goal is to introduce the abstraction of @deftech{instruction sequences}:
lists of instructions that represent the code of an @ch1-tech{x64} program.
@tech{Instruction sequences} separate the code from the boilerplate.
As a result, we get a notion of program composition, allowing us to focus on the
program, and decompose a program into separate pieces that we can easily stitch
together.
Supposing @metavar{p_1} and @metavar{p_2} are both @tech{instruction sequences},
then there exists @racket[(p-append _p_1 _p_2)] (for some definition of
@racket[p-append]) which first executes the instructions in @metavar{p_1} and then
executes the instructions in @metavar{p_2}.

Since these are not valid @ch1-tech{x64} programs on their own, we need to define
the meaning of @tech{instruction sequences} to make clear whether we are compiling
them correctly.
We define their meaning by describing an interpreter for @tech{instruction
sequences}: executing the @tech{instruction sequence} begins at the first
instruction in the sequence, and ends with the last instruction.
We describe how to execute each instruction once we fix a specific set of
instructions.
The final result of the @tech{instruction sequence} is the value of some
designated register after the last instruction is executed.

Below, we select the subset of @ch1-tech{x64} instructions we plan to suppport.

@itemlist[

@item{@tt{mov @metavar{triv}, @metavar{triv}}

This string represents the move instruction, which moves a value from one
location to another, or moves a value into a locaction.

@ch1-tech{x64} imposes further restrictions on @tt{mov}
(remember---@ch1-tech{x64} @emph{is}, we must not ask why).
We can only move a value into a register, or a value in one register to another
register.
}

@item{@tt{add @metavar{triv}, @metavar{triv}}

This string represents the add instruction, which intuitively
adds two values.

In fact, we cannot add values in @ch1-tech{x64}: we can add a value (@ie an
integer), to a register @tt{add @metavar{reg}, @metavar{integer}}, or add the
values of two registers, @tt{add @metavar{reg_1}, @metavar{reg_2}}.

Furthermore, when using an integer directly in the add statement, it must be a
32-bit integer, in the range @tt{-2^31 <= i <= 2^31 - 1}.
Yes, that's right, 32-bit not 64-bit; we do not ask "why" of x64.
For example, @tt{add rax, 2147483647} is valid, but @tt{add rax, 2147483648} is
not.
Instead, we would first need to move @tt{2147483648} into a register to
add it to @tt{rax}.
For example:
@verbatim{
mov rbx, 2147483648
add rax, rbx
}
}
@item{@tt{imul @metavar{triv}, @metavar{triv}}

This string represents the multiply instruction, which intuitively
multiplies two values.

Multiplication is further restricted by @ch1-tech{x64}.
Again, we cannot multiply values directly.
We can only multiply the value of a register by a 32-bit integer @tt{imul
@metavar{reg_1}, @metavar{int32}}, or the value in a register by the value in
a register @tt{imul @metavar{reg_1}, @metavar{reg_2}}.
}
]


Below is an example of a valid sequence of instructions in our subset of
@ch1-tech{x64}.
@nested[#:style 'inset
@verbatim{
  mov rax, 170679
  mov rdi, rax
  add rdi, rdi
  mov rsp, rdi
  imul rsp, rsp
  mov rbx, 8991
}
]

Note that this does not correspond to a @ch1-tech{x64} @emph{program}, as it is
missing much of the structure: the starting label, the section declarations,
etc.
The goal of our first compiler is to capture this notion of instruction
sequences, and insert the necessary boilerplate to generate a valid
@ch1-tech{x64} program.

We represent @ch1-tech{x64} @tech{instruction sequences} as Racket strings, with
each instruction separated by newline characters.
For example, the following @ch1-tech{x64} instruction sequence @tt{mov rax, 42}
corresponds to the Racket string @racket{  mov rax, 42}.

Note that the representation of @ch1-tech{x64} @tech{instruction sequences} is
whitespace sensitive.
For example, the following program corresponds to the Racket string
@racket{  mov rbx, 2147483648\n  add rax, rbx}.

@nested[#:style 'inset
@verbatim{
  mov rbx, 2147483648
  add rax, rbx
}
]

@section{Defining a Source Language}
Above we defined @tech{instruction sequences} in terms of the concrete syntax of
@ch1-tech{x64} and strings, but this syntax is not convenient for a compiler to
manipulate.
Concrete syntax often contains irrelevant details that are useful for human
programmers, but irrelevant to a machine.
For example, strings are also difficult to work with, as they lack structure for
conveniently accessing substructures, and whitespace-sensitivity makes means
there is multiple ways to represent identical programs.

Therefore, we define an abstract syntax our new language, @deftech{Paren-x64
v1}, with its new @tech{instruction sequence} abstraction.
We first present a simple definition, then gradually refine the definition to
encode more constraints.

@bettergrammar*[
  #:literals (integer?)
  #:datum-literals (begin set! + * rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11
   r12 r13 r14 r15)
  [p (begin s ...)]
  [s (set! triv triv) (set! triv (binop triv triv))]
  [triv reg integer]
  [binop + *]
  [reg rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
  [integer integer?]
]

Every @tech{Paren-x64 v1} program, @paren-x64-v1[p], begins with
@paren-x64-v1[begin], followed by a sequence of statements (instructions).
The non-terminal @paren-x64-v1[p] defines the @tech{instruction sequence}
abstraction.
We can define the composition operation as follows:
@racketblock[
(define (p-append p1 p2)
  (match `(,p1 ,p2)
    [`((begin ,s1 ...) (begin ,s2 ...))
     `(begin ,@s1 ,@s2)]))
]

Each instruction @paren-x64-v1[s] corresponds to one of the @ch1-tech{x64}
instructions described earlier.
For example, the @ch1-tech{x64} instruction @tt{mov @metavar{reg},
@metavar{integer}} corresponds to the @tech{Paren-x64 v1} instruction
@paren-x64-v1[(set! reg integer)].
Our abstract syntax makes more clear that the arithmetic operations are actually
a combination of an arithmetic operation and an operation that changes state.
The instruction @tt{add @metavar{reg}, @metavar{integer}} is represented
@paren-x64-v1[(set! reg (+ reg integer))].
Note that this duplicates the register, and the two occurences must be the same
in order to be translated to a valid @ch1-tech{x64}.

Some of the restrictions from @ch1-tech{x64} are not apparent in the definition
of the grammar.
To simplify precisely defining languages as grammars without too much additional
English specification, we create two conventions.
@itemlist[

@item{Any time we suffix a non-terminal by an underscore and a number, such as
@paren-x64-v1[reg_1], this is a reference to a particular instance of a
non-terminal, and it is restricted to be identical to any other instance of the
same non-terminal with the same underscore suffix within the same expression.
So @paren-x64-v1[(set! reg_1 (+ reg_1 integer))] represents an
@tt{add} instruction in @tech{Paren-x64 v1}, and both occurrences of
@paren-x64-v1[reg_1] must be the same register.
However, two instances of non-terminals with different suffixes, like
@paren-x64-v1[reg_1] and @paren-x64-v1[reg_2], are not necessarily
different---they are only not guaranteed to be the same.}

@item{Any non-terminal that is not defined as syntax, such as
@paren-x64-v1[int64], may be defined by a Racket predicate, such as
@racket[int64?].
Such definitions will link to the documentation defining the predicate.}
]

Using these two conventions, we define the final grammar of @tech{Paren-x64 v1}
with all the above restrictions below.

@bettergrammar*[paren-x64-v1]

The predicates @racket[int64?] and @racket[int32?] are defined by the support
library @racketmodname[cpsc411/compiler-lib], and return @racket[#t] if and only
if given an integer in the range for a 64-bit or 32-bit, respectively, signed
two's complement integer.

Since @tech{Paren-x64 v1} is an imperative language and does not return values
like Racket does, we must decide how to interpret a @tech{Paren-x64 v1} program
as a value.
As discussed above, we can do this by imposing some convention on what the
value of the program is.
We choose to interpret the value of an @tech{Paren-x64 v1} program as the value in
@paren-x64-v1[rax] when the program is finished executing (has no instructions
in the @tech{instruction sequence} left to execute).
This choice is completely up to us, but this choice is designed to continue to
work well as we build up our compiler.

We also require that any register is initialized before it is accessed.
We inherit this restriction from @ch1-tech{x64}.
In @ch1-tech{x64}, the value of an uninitialized register is undefined, that is,
accessing an uninitialized register results in @deftech{undefined behaviour},
behavior that has no specified behavior defined by the language specification.

@tech{Undefined behaviour} is common in low-level languages that lack a strong
enough enforcement mechanism for checking assumptions.
Eliminating undefined behavior by adding static or dynamic checks in the source
language improves the ability of programmers to predict behaviour of all
programs in your language.
However, it is not always practical to achieve.
It may be too difficult to statically check an assumption and still allow all
the programs you want to allow, or too expensive to check a property
dynamically.
In these cases, we are forced to make assumptions that we cannot enforce,
injecting @tech{undefined behaviour} into our compiler.

In this book, we make it a non-negotiable goal: @tech{source languages} must
never have @tech{undefined behaviour}.
If they might, we hobble them until we can eliminate the @tech{undefined behaviour}.

@section{Enforcing Assumptions}
One of the jobs of the front-end of a compiler is to enforce assumptions the
rest of the compiler makes.
Enforcement mechanisms take various forms, such as parsers,
type checkers, linters, and static analyses.

We're going to design a function @racket[check-paren-x64] to validate
@tech{Paren-x64 v1} programs.
It is, in essence, a parser.
It reads an arbitrary value, expected to represent a @tech{Paren-x64 v1}
program, and returns a valid program in the language @tech{Paren-x64 v1}, or
raises an error.
However, it is a trivial parser.
Usually we think of parsers as transforming from one representation to another,
but this parser does not transform the input if it is valid.
It does not deal with transforming strings into abstract syntax.
Instead, it expects its input to be abstract syntax already, and either returns
it if the abstract syntax is already valid, or raises an error.
This means our parsers, like the rest of our compiler, are simple tree automata,
rather than complex string automata.

You could view @racket[check-paren-x64] as a type checker.
In this view, it checks for a single type: @emph{The-Paren-x64-Type}, which
every valid instruction has, and which has quite simple typing rules.
@racket[check-paren-x64] checks that the input program is following the typing
disciplines of the language (which aren't very restrictive).

Writing validators, such as parsers or type checkers, for intermediate language
programs produced by your compiler is a powerful debugging technique.
By designing them in the same way as @racket[check-paren-x64], so that they
return the input if it's valid, you can easily add them as passes in your
compiler and detect when an early pass produces an ill-typed program.
This is a form of @emph{property-based testing}, and will catch many more bugs
than unit testing alone.

To ensure no @tech{undefined behaviour}, our validator should check the
following.
@itemlist[
@item{The input conforms to the grammmar of @tech{Paren-x64 v1}, including
restrictions regarding the valid range of integers and when the same register
must appear in two places.}
@item{No register is referred to before is it initialized. We assume that no
register is initialized at the beginning of a program.}
@item{The register @paren-x64-v1[rax] is initialized before the end of the
program.}
]

We split this into two separate functions to enable separation of concerns.
We always want our syntax to be valid, but because @tech{Paren-x64 v1} will
serve as a target language, we may not always want to enforce that registers are
provabably initialized.
For example, the language may evolve to the point where checking this will be
undecidable, and source languages will be responsible to enforcing the guarantee
isntead.
By separating the two checks, we'll be able to reuse code in this eventuality.

First, we check that the syntax is valid.

@nested[#:style 'inset]{
@defproc[(check-paren-x64-syntax [x any/c]) paren-x64-v1?]{
Takes an arbitrary value and either returns it, if it is valid @tech{Paren-x64
v1} syntax, or raises an error with a descriptive error message.
}

@examples[#:eval eg
(check-paren-x64
 `(begin (set! rax ,(min-int 64))))

(eval:error
 (check-paren-x64
  `(begin (set! rax ,(- (min-int 64) 1)))))

(eval:error
 (check-paren-x64-syntax
  '(begin
     (set! r17 170679))))

(check-paren-x64-syntax
 '(begin
    (set! rax 170679)
    (set! rdi rax)
    (set! rdi (+ rdi rdi))
    (set! rsp rdi)
    (set! rsp (* rsp rsp))
    (set! rbx 8991)))
]}

Then, we check register initialization.
Note that this procedure can assume its input is well-formed @tech{Paren-x64
v1} syntax, and only concern itself with register initialization.

@nested[#:style 'inset]{
@defproc[(check-paren-x64-init [x paren-x64-v1?]) paren-x64-v1?]{
Takes valid @tech{Paren-x64 v1} syntax, and returns a valid @tech{Paren-x64 v1}
program or raises an error with a descriptive error message.
}

@examples[#:eval eg
(eval:error
 (check-paren-x64-init
  '(set! (+ rax rdi) 42)))

(eval:error
 (check-paren-x64-init
  '(begin
     (set! (+ rax rdi) 42))))

(eval:error
 (check-paren-x64-init
  '(begin
     (set! rax (+ rax 42)))))

(eval:error
 (check-paren-x64-init
  '(begin
     (set! rax (+ rdi 42)))))

(check-paren-x64-init
 '(begin
    (set! rax 170679)
    (set! rdi rax)
    (set! rdi (+ rdi rdi))
    (set! rsp rdi)
    (set! rsp (* rsp rsp))
    (set! rbx 8991)))
]
}

For convenience, we define a single validator that validates all properties of
the language as @racket[check-paren-x64].

@nested[#:style 'inset]{
@defproc[(check-paren-x64 [x any/c]) paren-x64-v1?]{
Takes an arbitrary value and either returns it, if it is valid @tech{Paren-x64
v1} program, or raises an error with a descriptive error message. }
}

@section{Understanding Meaning}
When we create a new language, we want to ensure we understand its meaning
separate from how it is compiled.
This is for two reasons.
First, optimizations depend on when various programs in a language are
equivalent.
We need to understand the language in order to understand when programs are
equivalent.
Second, we cannot know whether or not the compiler is correct if we do not know
the meaning of programs before they are compiled.
Unit tests will help us debug, but when we know the meaning of @emph{all}
programs in the language, we can say whether that meaning is preserved through
compilation.

We can define the meaning of a language by writing an interpreter.
To design an interpreter for @tech{Paren-x64 v1} is straightforward.
We implement a register machine: a recursive function over @tech{instruction
sequences} that interprets each instructions by modifying a dictionary mapping
registers mapped to values.
When there are no instructions left, the interpreter returns the value of
the designated register @paren-x64-v1[rax].

In the interpreter, we assume the input is a valid @tech{Paren-x64 v1} program.
In a user interface, we would validate all input, for example by using
@racket[check-paren-x64], but in the implementation of the interpreter, we keep
the two concerns separate.
Instead, the interpreter is free to assume all integers are in the right range,
and arithmetic instructions correctly refer to the same register in both operand
positions.
For example, in the instruction @paren-x64-v1[(set! reg_1 (+ reg_2 integer))],
we assume @paren-x64-v1[reg_1] and @paren-x64-v1[reg_2] are identical, and
@paren-x64-v1[integer] is a 32-bit integer, since otherwise the input would not
have been a valid @tech{Paren-x64 v1} program.
In fact, it would be bad style to check these again, since this mixes concerns
and duplicates code.

@nested[#:style 'inset
@defproc[(interp-paren-x64 [x paren-x64-v1?]) int64?]{
Interprets the @tech{Paren-x64 v1} program, returning the final value as a
64-bit signed integer.

@examples[#:eval eg
(interp-paren-x64
 '(begin
    (set! rax 0)
    (set! rax (+ rax 42))))

(interp-paren-x64
 '(begin
    (set! rax 170679)
    (set! rdi rax)
    (set! rdi (+ rdi rdi))
    (set! rsp rdi)
    (set! rsp (* rsp rsp))
    (set! rbx 8991)))
]
}
]

To properly implement arithmetic operations, you need to handle two's complement
arithmetic, which overflows on large positive numbers and underflows on small
negative numbers.
You may want to use @racket[x64-add] and @racket[x64-mul] from
@racketmodname[cpsc411/compiler-lib].

Once we have the meaning of programs defined, we can define what it means for a
compiler to be correct.

A compiler for @tech{Paren-x64 v1} is correct if:
@itemlist[
@item{the meaning (as defined by the interpreter) of a program @paren-x64-v1[p] is
the value @paren-x64-v1[integer_1]}
@item{we compile @paren-x64-v1[p] and execute it as a @ch1-tech{x64} program and
get the value @paren-x64-v1[integer_2]}
@item{the values @paren-x64-v1[integer_1] and @paren-x64-v1[integer_2] are
@emph{equivalent}. In general, we have to define equivalence for each pair of
source and target languages. We'll define equivalence between @tech{Paren-x64
v1} and @ch1-tech{x64} values shortly.
}
]

@section{Compiling}
Finally, we get to compiling.
We have designed our new abstraction, made it precise in the form of a language,
enforced our assumptions, and understood its meaning
@todo{Should probably do interpreter first then checker? Don't know, this flow seems to work.}

The job of our compiler is to translate one level of abstraction into another.
We currently have a three levels of abstraction: (1) @tech{Paren-x64 v1}, the
abstract syntax representation of @ch1-tech{x64} @tech{instruction sequences},
(2) the string representation of @ch1-tech{x64} @tech{instruction sequences},
and (3), @ch1-tech{x64} programs.
The structure of our compiler is determined partially by these levels of
abstractions, both the source layer and the target layer.
There are two pieces that need to be added to a @tech{Paren-x64 v1} program to
make it a complete @ch1-tech{x64} program: (1) boilerplate, such as the declaration
of the starting label and (2) the run-time system, which implements our
convention for the meaning of @tech{instruction sequences}.

We design our compiler as a series of compiler passes along these natural lines
in order to separate concerns as much as possible.
First, we translate from @tech{Paren-x64 v1} into the string representation of
@tech{instruction sequences}.
Then, we introduce the run-time system.
Finally, we wrap the whole thing in boilerplate.
This order is not arbitrary; it is dictated by our abstract layers.
Implementing the run-time system requires instructions outside the subset used
by @tech{Paren-x64 v1}, so we must reach a lower level of abstraction to
implement it.
However, it can be implemented as an @tech{instruction sequence}, and we want to
take advantage of @tech{instruction sequence} composition if we can.
We won't be able to do that after introducing boilerplate.

@nested[#:style 'inset
@defproc[(generate-x64 [p paren-x64-v1?]) string?]{
Compiles a @tech{Paren-x64 v1} program into a @ch1-tech{x64} @tech{instruction
sequence} represented as a string.

@examples[#:eval eg
(generate-x64
 '(begin
    (set! rax 0)
    (set! rax (+ rax 42))))

(require racket/pretty)
(pretty-display
 (generate-x64
  '(begin
     (set! rax 0)
     (set! rax (+ rax 42)))))

(pretty-display
 (generate-x64
  '(begin
     (set! rax 170679)
     (set! rdi rax)
     (set! rdi (+ rdi rdi))
     (set! rsp rdi)
     (set! rsp (* rsp rsp))
     (set! rbx 8991))))
]
}
]

@subsection{Implementing a Run-time System}
The abstractions provided by the operating system for running @ch1-tech{x64} are
not the same as the convention we just created for @tech{Paren-x64 v1}.
The operating system does not know it should "return" the result of
@racket[rax], whatever "return" means.
To implement this convention, we need to write some @ch1-tech{x64} code
(ideally, an @tech{instruction sequence}) that will take any @tech{instruction
sequence} implementing the @tech{Paren-x64 v1} convention and communicate the
result to the operating system.
This code is a very simple run-time system.

Our choice of run-time system depends on the abstractions provided by the
language, the machine, the operating system, and the user interface we desire.
Some languages print the final result.
Some languages discard the final result, relying on the user program to print
the result, or modify the filesystem, or modify the state of the machine (or the
world) in some other non-temporary way.

Our language does not provide the user with any way to observe the state of the
machine, so our run-time system must do the job of communicating the return
value to the user.

We could print the result, but as we saw in the factorial example above, the
operating system's definition of "print" does not match our intuition.
When trying to print "120", we get the character "x".
This would make for a very confusing user interface, or a very complicated
run-time system.

Instead, we opt for a very simple run-time system: communicate via the operating
system exit code.
This exit code is a number between 0 and 255 given to the exit system call, and
is easily accessible in shells via the variable @tt{$?} (or @tt{$status} in
some shells).
In Racket, we can access the exit code of a subprocess using
@racket[system/exit-code].
This limits how much our programs can communicate; we will lift that restriction
in later versions of our compiler.

Because we've chosen to return the result as an exit code, our definition for
compiler correctness is non-trivial.
The meaning of a @tech{Paren-x64 v1} program is a 64-bit integer, but we've
designed the compiler to only ever return a value between 0 and 255.
This doesn't mean our compiler is incorrect, but instead, our definition of
correctness uses a custom notion of equivalence.
For this compiler, the result of a @tech{Paren-x64 v1} is considered
@emph{equivalent} to the exit code returned by a @ch1-tech{x64} program when the
two are @emph{equal} up to modulo 256:
@racketblock[
(define (v1-results-equivalent? s t)
  (= (modulo s 256) t))
]

Our run-time system is an @ch1-tech{x64} @tech{instruction sequence} which
expects to be composed after another @tech{instruction sequence}.
The run-time system assumes that the first @tech{instruction sequence} must
initialize @paren-x64-v1[rax].
The run-time system then calls the @tt{exit} system call with the value of
@paren-x64-v1[rax] passed as the exit code.
@racketmodname[cpsc411/compiler-lib] provides some definitions, such as
@racket[sys-exit], that are helpful for this.

For formatting strings in Racket, you may want to investigate @racket[format],
@racket[~a], and @racketmodname[at-exp].

@nested[#:style 'inset
@defproc[(wrap-x64-run-time [x string?]) string?]{
Installs the @tech{Paren-x64 v1} run-time system.
The input is the same as the output for @racket[generate-x64]: a
string representing an @tech{x64} @tech{instruction sequence}.
The run-time system is composed with the input as a second @tech{instruction
sequence}.
}]
@todo{should say above that instruction sequence composition is string
concatination in the string representation.}

@subsection{Wrapping it all up}

Finally, we implement a simple pass to wrap the @tech{instruction sequence} with
the @ch1-tech{x64} boilerplate described in @Secref[#:tag-prefixes '("chp1:")]{top}.

@nested[#:style 'inset
@defproc[(wrap-x64-boilerplate [x string?]) string?]{
Takes an @ch1-tech{x64} @tech{instruction sequence} and wraps it with the necessary
boilerplate to return a complete @ch1-tech{x64} program in Intel syntax.
}]

After that, we have a complete compiler.
The compiler is easily defined by composing all the individual passes.
@racketblock[
(define (paren-x64-v1-compiler x)
  (wrap-x64-boilerplate (wrap-x64-run-time (generate-x64 x))))

(define paren-x64-v1-compiler^
  (compose wrap-x64-boilerplate wrap-x64-run-time generate-x64))
]

@examples[#:eval eg
(interp-paren-x64
 '(begin
    (set! rax 170679)
    (set! rdi rax)
    (set! rdi (+ rdi rdi))
    (set! rsp rdi)
    (set! rsp (* rsp rsp))
    (set! rbx 8991)))

(current-pass-list
 (list
  check-paren-x64
  generate-x64
  wrap-x64-run-time
  wrap-x64-boilerplate))

(execute
 '(begin
    (set! rax 170679)
    (set! rdi rax)
    (set! rdi (+ rdi rdi))
    (set! rsp rdi)
    (set! rsp (* rsp rsp))
    (set! rbx 8991)))
]

The support library @racketmodname[cpsc411/compiler-lib] provides a few
abstractions for deriving the compiler from a list of passes.
See @racket[current-pass-list], @racket[compile], and @racket[execute].

@section{Appendix: Compiler Overview}

@dot->svg{
digraph {
node [ shape="box" ]

/* The Languages */

L0 [label="Paren-x64 v1"];
L1 [label="x64 instruction sequence"];
L2 [label="x64 program"];

/* The Passes */

edge [fontname="Courier"]

L0 -> L1 [label="generate-x64"];
L1 -> L1 [label="wrap-x64-run-time"];
L1 -> L2 [label="wrap-x64-boilerplate"];
L2 -> "integer" [label="execute"];

L0 -> "integer" [label="interp-paren-x64"];
}
}

@section{Appendix: Language Definitions}

@declare-exporting[cpsc411/langs/v1]

@deflangs[
paren-x64-v1
]
