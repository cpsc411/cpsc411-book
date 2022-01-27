#lang reader "assignment-lang.rkt"

@(require
  (for-label cpsc411/compiler-lib)
  (for-label cpsc411/2c-run-time)
  (for-label (except-in cpsc411/reference/a2-solution
                        check-paren-x64
                        interp-paren-x64))
  (for-label (only-in cpsc411/reference/a1-solution
                      check-paren-x64
                      interp-paren-x64))
  (for-label cpsc411/langs/v2)
  cpsc411/langs/v2)

@(provide
  (all-defined-out))

@;todo{Reset automatically on new assignment.}
@(reset-exercise-counter!)
@(reset-challenge-counter!)
@title[#:tag "top" #:tag-prefix "a2:"]{Milestone 2: Towards a Declarative Language}

@(define eg
   (make-cached-eval "a2-eval"
     '(require cpsc411/reference/a2-solution cpsc411/compiler-lib cpsc411/2c-run-time)
     '(current-stack-size 512)))

@section{Assignment Summary}

The goal of this assignment is to introduce the idea of designing languages by
abstracting away from annoying details, and implementing those abstractions by
compilation.
We will (1) abstract away from imperative instructions on locations to
expressions on values, and (2) abstract away from a small number of physical
locations and replace them with an essentially unboundedly large number of
abstract names.

This assignment is due @(due 'a2).

You can use the interrogator to get limited access to the reference solution:
@url{https://www.students.cs.ubc.ca/~cs-411/@|semester|/interrogator.cgi?an=a2}.

@subsection{Learning Objectives}
@todo{Redo above summary to split out summary from learning objectives.}

@subsection{Checklist}

@emph{Completely new passes}

@typeset-passlist[
uniquify
sequentialize-let
normalize-bind
select-instructions
assign-homes
uncover-locals
assign-fvars
replace-locations
flatten-begins
patch-instructions
implement-fvars
]

@emph{Minor modifications}
@typeset-passlist[
generate-x64
]

@emph{Remove passes}
@typeset-passlist[
wrap-x64-boilerplate
wrap-x64-run-time
]

@;@itemlist[#:style 'numbered

@;@item{Expose @emph{memory address operands} from @a0-tech{x64} in
@;      @tech{Paren-x64 v2}}
@;
@;@item{Design and implement a compiler for the @tech{Loc-lang} language, which
@;      abstracts all locations in @tech{Paren-x64 v2}.  This requires several
@;      compiler passes.  The two important ones: (1) expand instructions to match
@;      @a0-tech{x64} constraints, and (2) assign abstract locations to real
@;      locations on the machine.  For now, all abstract locations will be
@;      assigned to @deftech{frame locations}.  These are locations in memory on
@;      the @emph{stack frame}.  We'll talk about the stack and frames more in
@;      class, but for now it suffices to think of it as some place in memory of
@;      arbitrary size provided by the run-time system.  With access to memory,
@;      the number of physical locations becomes large enough to be practically
@;      infinite.
@;
@;      The @share{a2-compiler-lib.rkt} file provides an initial stack frame for
@;      your program.}
@;
@;@item{Design and implement the compiler for @tech{Values-lang}, which abstracts
@;      values in @tech{Loc-lang}.  This requires two compiler passes to translate
@;      value-oriented operations into sequences of instructions that operate on
@;      locations.}
@;]

@;@itemlist[
@;@item{Expose memory addressing operations from @a0-tech{x64} in @tech{Paren-x64 v2}.
@;  @itemlist[
@;    @item{Extend your implementation of @racket[check-paren-x64] to support
@;    @tech{Paren-x64 v2}, by adding support for accessing memory addresses.}
@;    @item{Extend your implementation of @racket[generate-x64] to compiler
@;    @tech{Paren-x64 v2} to @a0-tech{x64} by adding cases for instructions that
@;    access memory addresses.}
@;]}
@;@item{Design and implement @racket[patch-instructions], a compiler from
@;@tech{Paren-asm} to @tech{Paren-x64 v2}.}
@;@item{Design and implement @racket[replace-locations], a compiler from
@;@tech{Loc-assigned-lang} to @tech{Paren-asm}.}
@;@item{Design and implement @racket[assign-homes], a compiler from
@;@tech{Loc-locals-lang} to @tech{Loc-assigned-lang}.}
@;@item{Design and implement @racket[uncover-locals], a compiler from
@;@tech{Loc-lang} to @tech{Loc-locals-lang}.}
@;@item{Design and implement @racket[check-loc-lang], a type checker for @tech{Loc-lang}.}
@;@item{Design and implement @racket[select-instructions], a compiler from
@;@tech{Values-unique-lang} to @tech{Loc-lang}.}
@;@item{Design and implement @racket[uniquify], a compiler from @tech{Values-lang v2}
@;to @tech{Values-unique-lang}.}
@;@item{Design and implement @racket[check-values-lang], a type checker for
@;@tech{Values-lang}.}
@;@item{Design and implement @racket[interp-values-lang], an interpreter for
@;@tech{Values-lang}.}
@;]

@section{Reading}
The reading for this week is @Secref[#:tag-prefixes '("book:" "chp2:")]{top} and
@Secref[#:tag-prefixes '("book:" "chp3:")]{top}.
As usual, this milestone description links to the documentation for each
exercise in the chapter for convenience, but you are responsible for the
reading the entire chapter.

You should read first and work the relevant exercises as you read.

@section{Exercises}

@exercise{Design and implement @racket[uniquify] to resolve all @ch3-tech{names} to
@ch2-tech{abstract locations}.

You may find the functions @racket[name?] and @racket[fresh] helpful.
}

@exercise{Design and implement @racket[sequentialize-let].}

@exercise{Design and implement @racket[normalize-bind].}

@exercise{Design and implement @racket[select-instructions] to compile
imperative operations to abstract assembly instructions.

You may find the functions @racket[aloc?] helpful and @racket[fresh] helpful.
}

@exercise{Implement @racket[assign-homes].}

@exercise{Design and implement @racket[uncover-locals] to analyze which
@ch2-tech{abstract locations} need to be assigned @ch2-tech{physical locations}.

You may find the function @racket[aloc?] helpful.
For working with sets, you may want to use @secref["sets" #:doc '(lib
"scribblings/reference/reference.scrbl")].
}

@exercise{Design and implement @racket[assign-fvars] assign @ch2-tech{abstract
locations} to @ch2-tech{physical location}.
}

@exercise{Design and implement @racket[replace-locations] to replace
@ch2-tech{abstract locations} with their assigned @ch2-tech{physical location}.
}

@exercise{Design and implement @racket[flatten-begins] to flatten nested
instructions.

You may find the function @racket[make-begin] helpful.
}

@exercise{Design and implement @racket[patch-instructions] to abstract away from
@ch1-tech{x64} restrictions.
}

@exercise{Design and implement @racket[implement-fvars] to support the
@ch2-tech{frame variables} abstraction.

You may find the function @racket[fvar->index] helpful.
}

@exercise{Extend @racket[generate-x64] to emit @ch2-tech{displacement mode operands}.
@todo{Add hint for nasm syntax?}
}

@exercise{Remove your definitions of @racket[wrap-x64-boilerplate] and
@racket[wrap-x64-run-time].
These are now provided by @racketmodname[cpsc411/2c-run-time], since your
run-time system is getting more complicated.
}

@exercise{
Which passes were simplified by the abiltiy to nest @asm-lang-v2[tail]s?
}

@subsection{Optional Exercises}

These exercises are optional, but if you complete them, you'll be able to use
them when debugging your compiler, by comparing the results of a compiled
program to the result of interpreting it.

@todo{Add a separate chapter on validator, interpreters, etc, their use in
intermediate languages, and source langauges.}

@exercise{Redesign and extend the implementation of @racket[check-paren-x64].
You should be able to modify your validator for the previous
@ch-bp-tech{Paren-x64 v1} to include cases for displacement mode
operands and accept programs for @ch2-tech{Paren-x64 v2}.

You should start by writing new examples and tests for the new specification,
and adding new @racket[match] clauses following the template.
}

@exercise{Redesign and extend your implementation of @racket[interp-paren-x64]
to support @ch2-tech{Paren-x64 v2}.

@;You should start by writing new examples and tests for the new specification,
@;and adding new @racket[match] clauses following the template.
@;You'll need at least 4 new tests, checking for success and failure of the new
@;features.
}

@examples[#:eval eg
(interp-paren-x64 '(begin (set! rax 42)))
(interp-paren-x64 '(begin (set! rax 42) (set! rax (+ rax 0))))
(interp-paren-x64
 '(begin
    (set! (rbp - 0) 0)
    (set! (rbp - 8) 42)
    (set! rax (rbp - 0))
    (set! rax (+ rax (rbp - 8)))))
(interp-paren-x64
 '(begin
    (set! rax 0)
    (set! rbx 0)
    (set! r9 42)
    (set! rax (+ rax r9))))
(interp-paren-x64
 '(begin
    (set! (rbp - 0) 42)
    (set! rax (rbp - 0))))
]

@;@exercise{
@;Design and implement the function @racket[check-loc-lang], a type checker for
@;@ch3-tech{Loc-lang}.
@;}
@;
@;@examples[#:eval eg
@;(eval:error
@; (check-loc-lang
@;  '(begin
@;     (set! x.1 0))))
@;
@;(check-loc-lang
@; '(begin
@;    (set! x.1 0)
@;    (halt x.1)))
@;
@;(check-loc-lang
@; '(begin
@;    (set! x.1 0)
@;    (set! y.1 x.1)
@;    (set! w.1 (+ w.1 y.1))
@;    (halt w.1)))
@;
@;(eval:error
@; (check-loc-lang
@;  '(begin
@;     (set! x.1 0)
@;     (set! y.1 x.1)
@;     (set! w.1 (+ x.1 5))
@;     (halt w.1))))
@;]

@exercise{Design and implement @racket[check-values-lang], a validator for
@ch3-tech{Values-lang v3}.}

@exercise{Design and implement @racket[interp-values-lang], an interpreter for
@ch3-tech{Values-lang v3}.}

@examples[#:eval eg
(interp-values-lang '(module (+ 2 2)))

(interp-values-lang
 '(module
    (let ([x 5]) x)))

(interp-values-lang
 '(module
    (let ([x (+ 2 2)])
      x)))

(interp-values-lang
 '(module
    (let ([x 2])
     (let ([y 2])
       (+ x y)))))

(interp-values-lang
 '(module
    (let ([x 2])
     (let ([x 2])
       (+ x x)))))

(current-pass-list
 (list
  check-values-lang
  uniquify
  sequentialize-let
  normalize-bind
  select-instructions
  assign-homes
  flatten-begins
  patch-instructions
  implement-fvars
  check-paren-x64
  generate-x64
  wrap-x64-run-time
  wrap-x64-boilerplate))

(execute '(module (+ 2 2)))

(execute
 '(module
    (let ([x 5])
      x)))
(execute
 '(module
    (let ([x (+ 2 2)])
      x)))

(execute
 '(module
    (let ([x 2])
      (let ([y 2])
        (+ x y)))))

(execute
 '(module
    (let ([x 2])
     (let ([x 2])
      (+ x x)))))

(define (compile-pre-runtime p)
  (parameterize ([current-pass-list
                  (list
                   check-values-lang
                   uniquify
                   sequentialize-let
                   normalize-bind
                   select-instructions
                   assign-homes
                   flatten-begins
                   patch-instructions
                   implement-fvars
                   check-paren-x64
                   generate-x64)])
    (compile p)))

(require racket/pretty)
(pretty-display
 (compile-pre-runtime '(module (+ 2 2))))

(pretty-display
 (compile-pre-runtime
  '(module
     (let ([x 5])
      x))))

(pretty-display
 (compile-pre-runtime
  '(module
     (let ([x 2])
       (let ([y 2])
         (+ x y))))))
]
