#lang racket

(module+ test
  (require rackunit)
  ; Values-lang p -> Values-lang p -> void
  ; Checks that p1 and p2 are alpha-equivalent Values-lang programs.
  ; Also worked for Values-unique-lang.
  ; Returns nothing useful, or raises a test failure.
  (define-check (check-Values-lang-alpha-equal? p1 p2)
    ; Env is a dictionary mapping a Values-lang name to a Values-lang name.

    ; Env -> Values-lang x -> Values-lang x -> void
    ; Check that two variables are equal under env.
    (define (check-var env v1 v2)
      (unless (equal? (dict-ref env v1) v2)
        (fail-check (format "Expected ~a for ~a, but found ~a" v2
                            v1
                            (dict-ref env v1)))))

    ; Env -> Values-lang b -> Values-lang b -> Env
    ; Checks that the Values-lang blocks b1 and b2 are alpha-equivalent.
    ; Returns an environment mapping all definitions from b1 to their
    ; counterparts in b2, or raises a test failure.
    (define (check-b env b1 b2)
      (match (cons b1 b2)
        ;; NOTE: Since I assume the input is a valid Values-lang program (this
        ;; assumption is stated in the signature), I can safely assume both b1
        ;; and b2 are valid Paren-lang bs, and there are no other cases.
        [(cons `(define ,x1 (lambda (,ys1 ...) ,e1))
               `(define ,x2 (lambda (,ys2 ...) ,e2)))
         (define new-env (dict-set env x1 x2))
         (unless (equal? (length ys1) (length ys2))
           (fail-check
            "Expected ~a arguments (in function ~a) but found ~a in (in ~a)."
            (length ys2) x2 (length ys1) x1))
         ;; NOTE: The for/fold construct allows you to loop over one or more
         ;; lists while accumulating data.
         ;; This is convenient short hand for simple recursive functions.
         ;; This one loops of ys1 and ys2, adding each to the dictionary env,
         ;; which is initially set to new-end.
         (check-e (for/fold ([env new-env])
                            ([y1 ys1]
                             [y2 ys2])
                    (dict-set env y1 y2)) e1 e2)
         new-env]))

    ; Env -> Values-lang e1 -> Values-lang e2 -> void
    (define (check-e env e1 e2)
      (match (cons e1 e2)
        [(cons `(let ([,x1 ,n1]) ,e1)
               `(let ([,x2 ,n2]) ,e2))
         (let ([env (dict-set env x1 x2)])
           (check-n env n1 n2)
           (check-e env e1 e2))]
        [(cons `(if (,cmp ,v1 ,v2) ,e1 ,e2)
               `(if (,cmp ,v12 ,v22) ,e12 ,e22))
         (check-v env v1 v12)
         (check-v env v2 v22)
         (check-e env e1 e12)
         (check-e env e2 e22)]
        ;; NOTE: Since I assume the input is a valid Values-lang program (this
        ;; assumption is stated in the signature), I can assume all other cases
        ;; MUST be valid tails.
        [_ (check-tail env e1 e2)]))

    ; Env -> Values-lang tail -> Values-lang tail -> void
    (define (check-tail env tail1 tail2)
      (match (cons tail1 tail2)
        [(cons `(apply ,x1 ,vs1 ...)
               `(apply ,x2 ,vs2 ...))
         (check-var env x1 x2)
         ;; NOTE: (curry check-v env) partially applies check-v to only env,
         ;; returning a function expecting the rest of the arguments.
         ;; This is short-hand for:
         ;;   (lambda (v1 v2) (check-v env v1 v2))
         ;; It's good to avoid curry, but sometimes it useful and not much less
         ;; clear.
         ;; I often use it like below, in a map or for-each.
         ;; For anything more complex, it's usually good to use a for/list,
         ;; for/fold, etc.
         (for-each (curry check-v env) vs1 vs2)]
        ;; NOTE: Since I assume the input is a valid Values-lang program (this
        ;; assumption is stated in the signature), I can assume all other cases
        ;; MUST be valid ns.
        [_ (check-n env tail1 tail2)]))

    ; Env -> Values-lang n -> Values-lang n -> void
    (define (check-n env n1 n2)
      (match (cons n1 n2)
        [(cons `(,binop ,vs1 ...)
               `(,binop ,vs2 ...))
         (for-each (curry check-v env) vs1 vs2)]
        [_ (check-v env n1 n2)]))

    ; Env -> Values-lang v -> Values-lang v -> void
    (define (check-v env v1 v2)
      (match (cons v1 v2)
        [(cons int int)
         #:when (integer? int)
         (void)]
        ;; NOTE: Since I assume the input is a valid Values-lang program, the
        ;; only other case must be variables.
        [(cons x1 x2)
         (check-var env x1 x2)]))
    (match (cons p1 p2)
      [(cons `(module ,bs1 ... ,e1)
             `(module ,bs2 ... ,e2))
       (check-e (for/fold ([env '()])
                          ([b1 bs1]
                           [b2 bs2])
                  (check-b env b1 b2))
                e1 e2)]))

  ;; Example:
  #;(let ([x '(module (let ([v 1])
                      (let ([w 46])
                        (let ([x v])
                          (let ([x (+ x 7)])
                            (let ([y x])
                              (let ([y (+ y 4)])
                                (let ([z x])
                                  (let ([z (+ z w)])
                                    (let ([t.1 y])
                                      (let ([t.1 (* t.1 -1)])
                                        (let ([z (+ z t.1)])
                                          z))))))))))))])
    (check-Values-lang-alpha-equal? (uniquify x) x)))
