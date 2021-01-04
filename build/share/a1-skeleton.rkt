#lang racket

(require
 cpsc411/compiler-lib)

(provide
 check-paren-x64
 interp-paren-x64
 generate-x64
 wrap-x64-run-time
 wrap-x64-boilerplate)

(define-syntax-rule (TODO stx)
  (error "unfinished skeleton"))

(define (check-paren-x64-init p)
  (TODO ...))

(define (check-paren-x64-syntax p)
  (TODO ...))

(define (check-paren-x64 p)
  (check-paren-x64-init (check-paren-x64-syntax p)))

(define (interp-paren-x64 p)
  (define (eval-instruction-sequence regfile s)
    (if (empty? s)
        ; If no more instructions, return exit code modulo 256 (since operating
        ; systems return exit code modulo 256).
        (modulo (dict-ref regfile 'rax) 256)
        (TODO ...)))
  (TODO ...))

(define (generate-x64 p)
  ; Paren-x64-v1 -> x64-instruction-sequence
  (define (program->x64 p)
    (match p
      [`(begin ,s ...)
       (TODO ...)]))

  (define (statement->x64 s)
    (TODO ... ))

  (define (binop->ins b)
    (TODO ... ))

  (program->x64 p))

(define (wrap-x64-run-time str)
  (TODO ...))

(define (wrap-x64-boilerplate str)
  (TODO ...))

(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/public/a1)

  (run-tests
   (a1-public-test-suite
    (list
     check-paren-x64
     generate-x64
     wrap-x64-run-time
     wrap-x64-boilerplate)
    interp-paren-x64)))
