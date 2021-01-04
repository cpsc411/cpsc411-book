#lang scribble/manual
@(require
  (for-label racket)
  scribble/example
  "../assignment/lib.rkt")

@(define eg (make-base-eval))

@title[#:tag "top" #:tag-prefix "style:"]{Organizing Racket Code}
Functional languages, such as Racket, often give you more freedom in organizing
your code base than you might be used to, but this freedom can make it hard to
know how to keep your code base orderly.
Languages like Java force a correspondence between classes and files, almost
forcing you to organize your code.
Racket doesn't care about how functions are organized into files.
While you @emph{can} put all your code in a single file, this makes browsing the
code hard, complicates version control and merge conflicts, defeats separate
compilation, and just @emph{feels gross}.

Typically, a Racket program organizes groups of functions that are conceptually
related into a file.
We might gather all functions related to register allocation into a single file.
But maybe that file is @emph{too big} (a subjective experience, not a clear
dividing line), so we divide further: all functions related to conflict analysis
in one file, all functions related to undead analysis in another.
Often, we have a few functions that are needed in various placed, but don't fit
in any conceptual group.
It's common to stick these functions in a miscellaneous file, perhaps named
@tt{utils.rkt}.

We can emulate anything we would do with files by using @racket[module].
The below examples demonstrate organizing code into separate modules, which we
could store in separate files.

@examples[#:eval eg
(code:comment "Behaves like a file named \"utils.rkt\" that begins \"#lang racket\"")
(module utils.rkt racket
  (code:comment "(all-defined-out) tells the module system to export everything")
  (code:comment "defined at the top-level.")
  (provide (all-defined-out))
  (define (binop? o)
    (and (member o '(* +)) #t))
  (define (aloc? o)
    (and (symbol? o)
         (regexp-match-exact? #rx".+\\.[0-9]+" (symbol->string o))))
  (define (triv? o)
    (and (aloc? o)
         (triv? o))))

(code:comment "Behaves like a file named \"checker.rkt\" that begins \"#lang racket\"")
(module checker.rkt racket
  (require 'utils.rkt)
  (provide check-ae-lang)

  (define (check-ae-lang ls)
    (define (check-statement s)
      (match s
        [`(set! ,aloc ,int)
         #:when (and
                 (aloc? aloc)
                 (integer? int))
         s]
        [`(set! ,aloc1 (,binop ,aloc2 ,triv))
         #:when (and
                 (aloc? aloc1)
                 (aloc? aloc2)
                 (binop? binop)
                 (triv? triv))
         s]
        [_ (error "Unexpected statement" s)]))
    (match ls
      [`(begin ,s ...)
       `(begin ,@(map check-statement s))])))

(require 'checker.rkt)
(check-ae-lang '(begin (set! x.1 5)))
]

Tests are not commonly placed in-line with code, the way you were taught in 110,
but split out into separate files.
The Racket testing framework can automatically scan all files for
@racket[(module+ test ...)] and run the test suites it finds.
It's common to split tests into conceptually related files, too.
For example, I might group all tests related to register allocation into one
file, and tests that check the interpreter against the compiler into another.

@examples[#:eval eg
(module test-checker racket
  (require 'checker.rkt)
  (module+ test
    (require rackunit)
    (check-equal?
     (check-ae-lang '(begin (set! x.1 5)))
     '(begin (set! x 5)))
    (check-exn
     (thunk (check-ae-lang '(begin (+ x.1 5)))))))

(module test-utils racket
  (require 'utils.rkt)
  (module+ test
    (require rackunit)
    (check-true (triv? 5))
    (check-false (triv? `(set! x.1 5)))))
]

It's common to put all the test files in a separate test directory.

Of course, your assignments dictate that there be a particular file that
exports particular names; thankfully, Racket's module system allows reproviding
definitions you've imported from elsewhere.

This requirement isn't about where things live, but about an @emph{interface}.
I need to be able to import that file and get access to certain functions.
One way to implement that interface is to literally define the functions in the
file @tt{compiler.rkt}.
Another way is to use @racket[all-from-out] to reprovide functions that were
imported from elsewhere.
This could allow @tt{compiler.rkt} to be completely empty except for require and
provide declarations; this is a common pattern in Racket.
@examples[#:eval eg
(code:comment "Racket views files as modules.")
(module compiler.rkt racket
  (require 'checker.rkt 'utils.rkt)
  (provide (all-from-out 'checker.rkt)))

(require 'compiler.rkt)
(code:comment "I have access to `check-ae-lang`, which is defined in")
(code:comment "\"checker.rkt\", despite only importing the module \"a3.rkt\"")
(check-ae-lang `(begin (set! x.1 5)))
]

@section{Organizing your code to keep your graders sane}
The above is a stylistic discussion about Racket best practices, some of which
applies to other functional languages.
However, you're also students, and we have to grade your code.

If you're going to refactor your code out of a single file, you must following
these instructions.
@itemlist[
@item{Put all tests either in a file named @tt{a<assignment-number>-tests.rkt},
or under a directory named @tt{tests}.}
@item{Use suggestive file names, like
          @tt{assign-register-tests.rkt}, or @tt{register-allocator.rkt}, or @tt{undead-lang.rkt}.}
@item{Create a file called @tt{README.md}, which tells us in which file each
exercise can be found.
Make it simple and clear, like
@verbatim{
Exercise 2, assign-registers -> register-allocator.rkt
Exercise 2, assign-registers tests -> tests/register-allocator-tests.rkt
}
}
]

@;  LocalWords:  lang eg eval emph undead misc tt utils rkt binop rx
@;  LocalWords:  aloc regexp triv ae ls int rackunit exn reproviding
@;  LocalWords:  reprovide refactor itemlist md
