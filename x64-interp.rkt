#lang racket

(provide interp-x64)

(define (interp-x64 str)
  (let loop ([strs (string-split str "\n")]
             [start (void)])
    (if (empty? strs)
        (error "something has gone wrong")
        (match (car strs)
          [(regexp #px"^\\s*global (\\w+)\\s*$"
                   (list _ start-label))
           (loop (cdr strs) start-label)]
          [(regexp #px"\\s*section .text")
           (interp-x64-instr (cdr strs) start)]
          [_ (loop (cdr strs) start)]))))

(define (interp-x64-instr strs start-label)
  (let loop ([env '()]
             [strs strs])
    (if (empty? strs)
      env
      (match (car strs)
        [(regexp #px"\\s*(\\w+):"
                 (list _ label))
         (if (equal? label start-label)
             (loop env (cdr strs))
             (error "something weird happened")
             #;(dict-set env (string->symbol label)
                       (lambda (env)
                         (loop env (cdr strs)))))]
        [(regexp #px"\\s*mov (\\w+), (\\w+)"
                 (list _ rand1 rand2))
         (loop
           (dict-set env (string->symbol rand1)
                     (interp-x64-rand env rand2))
           (cdr strs))]
        [(regexp #px"\\s*section .data")
         env]
        [_ (error "invalid instructions")]))))

(define (interp-x64-rand env rand)
  (match rand
    [(regexp #px"^\\d+$")
     (string->number rand)]
    [_ (dict-ref env (string->symbol rand))]))

(module+ test
  (require rackunit)
  (check-match (interp-x64
"global start
section .text
start:
  mov r9, 120
section .data
dummy: db 0"
)
               (app (curryr dict-ref 'r9) 120)))
