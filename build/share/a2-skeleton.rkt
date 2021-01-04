#lang racket
(require
 racket/set
 "a2-compiler-lib.rkt")

(provide
 check-values-lang
 interp-values-lang
 uniquify
 select-instructions
 check-loc-lang
 uncover-locals
 assign-homes
 replace-locations
 patch-instructions
 check-paren-x64
 interp-paren-x64
 generate-x64)

(module+ test
  (require rackunit))

;; ------------------------------------------------------------------------
;; Additional starter code for functions in your existing compiler.

(TODO "Merge your solution from a1.rkt into this section. You may replace any of
       your own code with starter code from this file, or may simply modify your
       own solution.")

;; Exercise 1

; Any -> Paren-x64-v2 or Error
(define (check-paren-x64 p)
  (TODO "Implement check-paren-x64 for Exercise 1"))

(module+ test
  (define (check-checker f e)
    (check-equal? (f e) e))

  (check-exn
   exn:fail?
   (thunk
    (check-paren-x64
     '(set! (+ rax rdi) 42))))

  (check-exn
   exn:fail?
   (thunk
    (check-paren-x64
     '(begin
        (set! (+ rax rdi) 42)))))

  (check-checker
   check-paren-x64
   '(begin
      (set! rax (+ rax 42))))

  (check-exn
   exn:fail?
   (thunk
    (check-paren-x64
     '(begin
        (set! rax (+ rdi 42))))))

  (check-checker
   check-paren-x64
   '(begin
      (set! rax 170679)
      (set! rdi rax)
      (set! rdi (+ rdi rdi))
      (set! rsp rdi)
      (set! rsp (* rsp rsp))
      (set! rbx 8991))))

;; Exercise 2

; Paren-x64-v2 -> Integer
(define (interp-paren-x64 p)
  ; Environment (List-of (Paren-x64-v2 Statement)) -> Integer
  (define (eval-instruction-sequence env sls)
    (if (null? sls)
        (dict-ref env 'rax)
        (TODO "Implement the fold over a sequence of Paren-x64-v2 /s/ for Exercise 2.")))

  ; Environment Statement -> Environment
  (define (eval-statement env s)
    (TODO "Implement the transition function evaluating a Paren-x64-v2 /s/ for Exercise 2."))

  ; (Paren-x64-v2 binop) -> procedure?
  (define (eval-binop b)
    (TODO "Implement the interpreter for Paren-x64-v2 /binop/ for Exercise 2."))

  ; Environment (Paren-x64-v2 triv) -> Integer
  (define (eval-triv regfile t)
    (TODO "Implement the interpreter for Paren-x64-v2 /triv/ for Exercise 2."))

  (TODO "Implement the interpreter for Paren-x64-v2 /p/ for Exercise 2."))

;; Exercise 3

; Paren-x64-v2 -> x64
(define (generate-x64 p)
  ; (Paren-x64-v2 Program) -> x64
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (TODO "Implement the compiler for the Paren-x64-v2 /p/ non-terminal for Exercise 3.")]))

  ; (Paren-x64-v2 Statement) -> x64
  (define (statement->x64 s)
    (TODO "Implement the compiler for the Paren-x64-v2 /s/ non-terminal for Exercise 3."))

  ; (Paren-x64-v2 loc) -> string
  (define (loc->x64 loc)
    (TODO "Implement the compiler for the Paren-x64-v2 /loc/ non-terminal for Exercise 3."))

  ; (Paren-x64-v2 binop) -> string
  (define (binop->ins b)
    (TODO "Implement the compiler for the Paren-x64-v2 /binop/ non-terminal for Exercise 3."))

  (program->x64 p))

;; Exercise 4
(TODO "Delete your implementations of wrap-x64-run-time and wrap-x64-boilerplate for Exercise 4.")


;; ------------------------------------------------------------------------
;; New starter code. All of your code from a1 should exist before this line.

;; Exercise 5

; Paren-asm -> Paren-x64-v2
(define (patch-instructions p)
  (TODO "Design and implement patch-instructions for Exercise 5."))

;; Exercise 6

; Loc-assigned-lang -> Paren-asm
(define (replace-locations p)
  (TODO "Design and implement replace-locations for Exercise 6."))

;; Exercise 7

; Loc-locals-lang -> Loc-assigned-lang
(define (assign-homes p)
  (TODO "Design and implement assign-homes for Exercise 7."))

;; Exercise 8

; Loc-lang -> Loc-locals-lang
(define (uncover-locals p)
  (TODO "Design and implement uncover-locals for Exercise 8."))

;; Exercise 9

; Any -> Loc-lang or Error
(define (check-loc-lang p)
  (define (check-p p)
    (TODO "Implement checker for Loc-lang /p/ for Exercise 9."))

  (define (check-s s)
    (TODO "Implement checker for Loc-lang /s/ for Exercise 9."))

  (check-p p)
  p)

;; Exercise 10

; Values-unique-lang -> Loc-lang
(define (select-instructions p)

  ; (Values-unique-lang v) -> (List-of (Loc-lang s)) and (Loc-lang aloc)
  ; Assigns the value v to a fresh temporary, returning two values: the list of
  ; statements the implement the assignment in Loc-lang, and the aloc that the
  ; value is stored in.
  (define (assign-tmp v)
    (TODO "Consider implementing assign-tmp for Exercise 10."))

  ; (Values-unique-lang aloc) -> (Values-unique-lang n) -> List-of (Loc-lang s)
  ; Generate the Loc-lang instruction sequence that assigns the trivial computation n to
  ; the location loc in Loc-lang.
  (define (assign-n loc n)
    (TODO "Consider implementing assign-n for Exercise 10."))

  ; (Values-unique-lang e) (Values-unique-lang n) -> List-of (Loc-lang s)
  (define (select-e e)
    (TODO "Implement select-e for Exercise 10"))

  `(begin ,@(select-e p)))

;; Exercise 11

; Values-lang -> Values-unique-lang
(define (uniquify p)
  (TODO "Design and implement uniquify for Exercise 11."))

;; Exercise 12

; Any -> Values-lang or Error
(define (check-values-lang p)
  (TODO "Design and implement check-values-lang for Exercise 12."))

;; Exercise 13

; Any -> Values-lang or Error
(define (interp-values-lang p)
  (TODO "Design and implement interp-values-lang for Exercise 13."))

(module+ test
  ; Default compiler should go from Values-lang to x64
  (current-pass-list
   (list
    check-values-lang
    uniquify
    select-instructions
    check-loc-lang
    uncover-locals
    assign-homes
    replace-locations
    patch-instructions
    check-paren-x64
    generate-x64
    wrap-x64-run-time
    wrap-x64-boilerplate))

  (let ([x '(let ([x (+ 1 2)])
              x)])
    (check-equal?
     (execute x)
     (interp-paren-x64
      '(begin
         (set! (rbp - 16) 1)
         (set! (rbp - 8) 2)
         (set! rax (rbp - 16))
         (set! rax (+ rax (rbp - 8)))
         (set! (rbp - 24) rax)
         (set! rax (rbp - 24))))))

  ; Test intermediate compilers by parameterizing a test-suite by a different
  ; current-pass-list.
  (parameterize ([current-pass-list
                  (list
                   check-paren-x64
                   generate-x64
                   wrap-x64-run-time
                   wrap-x64-boilerplate)])

    (let ([x '(begin
                (set! r11 0)
                (set! (rbp - 0) 2)
                (set! (rbp - 8) 0)
                (set! rsi r11)
                (set! rsi (+ rsi (rbp - 0)))
                (set! rsi (+ rsi 0))
                (set! rdi rsi)
                (set! rdi (+ rdi (rbp - 8)))
                (set! rax rdi))])
      (check-equal? (execute x) (interp-paren-x64 x)))

      (let ([x '(begin
                  (set! r9 0)
                  (set! r12 0)
                  (set! r9 (* r9 r12))
                  (set! r9 (* r9 0))
                  (set! r9 (+ r9 1))
                  (set! rcx r9)
                  (set! rcx (* rcx 1))
                  (set! rax 7)
                  (set! rax (+ rax rcx))
                  (set! rax (+ rax rax))
                  (set! r14 2)
                  (set! r14 (+ r14 rax))
                  (set! rax (* rax r14)))])
        (check-equal? (execute x) (interp-paren-x64 x)))))
