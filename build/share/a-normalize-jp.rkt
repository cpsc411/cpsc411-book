#lang racket/base

(require "a9-compiler-lib.rkt")
(provide a-normalize)

(define (aloc? s)
  (and (symbol? s)
       (not (register? s))
       (not (label? s))
       (regexp-match-exact? #rx".+\\.[0-9]+" (symbol->string s))))

; To implement join points, we must DIY closure-convert, since this comes later
; than closure conversion.
; We can't push the pass before closure conversion without really complicating
; specify-representation, which uses algebraic expressions.
; Design trade offs.

; Impure-Exprs-bits-lang v9/e -> (set aloc)
; Get the set of free abstract locations of an expression.
(define (get-free-vars e)
  (define (get-e e)
    (match e
      [`(let ([,aloc ,e1]) ,e2)
       (set-union (get-free-vars e1)
                  (set-remove (get-free-vars e2) aloc))]
      [`(if (,cmp ,e1 ,e2) ,et ,ef)
       (set-union
        (get-free-vars e1)
        (get-free-vars e2)
        (get-free-vars et)
        (get-free-vars ef))]
      [`(begin ,cs ... ,e)
       (apply set-union (get-e e) (map get-c cs))]
      [`(apply ,es ...)
       (apply set-union '() (map get-e es))]
      [`(alloc ,e)
       (get-e e)]
      [`(mref ,e1 ,e2)
       (set-union (get-e e1) (get-e e2))]
      [`(,binop ,e1 ,e2)
       (set-union (get-e e1) (get-e e2))]
      [v (get-v v)]))

  (define (get-v v)
    (match v
      [(? aloc?) (list v)]
      [v '()]))

  (define (get-c c)
    (match c
      [`(begin ,cs ...)
       (apply set-union '() (map get-c cs))]
      [`(mset! ,es ...)
       (apply set-union '() (map get-e es))]))

  (get-e e))

(define Exprs-bits-lang-value?
  (or/c int64? label? aloc?))

(define (Exprs-bits-lang-tail? n)
  (match n
    [(? Exprs-bits-lang-value?) #t]
    [`(apply ,(? Exprs-bits-lang-value?) ...)
     #t]
    [`(,binop ,(? Exprs-bits-lang-value?) ...)
     #t]
    [_ #f]))

; Impure-Exprs-bits-lang-v9/p -> Impure-Values-bits-lang-v9/p
; Normalizes an expression with respect to A-reductions.
; Employs the join-point optimization to prevent exponential code duplication.
(define (a-normalize p)
  (define jps (box '()))

  (define (anf-e-v e k)
    (anf-e
     e
     (lambda (n)
       (match n
         [(? Exprs-bits-lang-value?)
          (k n)]
         [_
          (let ([x (fresh)])
            `(let ([,x ,n])
               ,(k x)))]))))

  (define (anf-e* es k)
    (if (empty? es)
        (k '())
        (anf-e-v (car es) (lambda (v)
                           (anf-e* (cdr es) (lambda (vs)
                                              (k (cons v vs))))))))

  (define (anf-c c k)
    (match c
      [`(mset! ,es ...)
       (anf-e* es
        (lambda (vs)
          (k `(mset! ,@vs))))]
      [`(begin ,cs ...)
       (let loop ([cs cs])
         (if (empty? cs)
             (k '(begin))
             (anf-c (car cs)
                    (lambda (c)
                      (make-begin
                         (list c)
                         (loop (cdr cs)))))))]))

  (define (flatten-begin c)
    (match c
      [`(begin ,cs ...)
       (append-map flatten-begin cs)]
      [_ (list c)]))

  (define (make-begin cs e)
    (match e
      [`(begin ,cs2 ... ,e)
       (make-begin
        (append
         (append-map flatten-begin cs)
         (append-map flatten-begin cs2)) e)]
      [_
       `(begin ,@cs ,e)]))

  ;; Call once per if with the current continuation.
  ;; Returns a continuation that will translate the branches of the if, creating
  ;; a join point (once, if necessary), but avoiding creating a join point for
  ;; simple continuations.
  (define (make-join-point-trans k)
    (let* ([x (fresh)]
           [body (k x)]
           [jp (fresh-label 'jp)]
           [free-vars (set-remove (get-free-vars body) x)]
           [flag (box #f)]
           [lazy-make-join-point!
            (lambda ()
              (unless (unbox flag)
                (set-box! flag #t)
                (set-box!
                 jps
                 (cons
                  `(define ,jp (lambda (,x ,@free-vars) ,body))
                  (unbox jps)))))])
      (lambda (e)
        (match body
          [(? Exprs-bits-lang-tail?)
           (anf-e e k)]
          [_
           (lazy-make-join-point!)
           (anf-e-v e (lambda (v) `(apply ,jp ,v ,@free-vars)))]))))

  (define (anf-e e k)
    (match e
      [`(let ([,aloc ,e1]) ,e2)
       (anf-e
        e1
        (lambda (n1)
          `(let ([,aloc ,n1])
             ,(anf-e e2 k))))]
      [`(if (,cmp ,e1 ,e2) ,et ,ef)
       (let ()
         (let* ([anf-jp (make-join-point-trans k)])
           (anf-e-v
            e1
            (lambda (v1)
              (anf-e-v
               e2
               (lambda (v2)
                 `(if (,cmp ,v1 ,v2)
                      ,(anf-jp et)
                      ,(anf-jp ef))))))))]
      [`(apply ,e1 ,es ...)
       (anf-e-v
        e1
        (lambda (v1)
          (anf-e* es (lambda (vs) (k `(apply ,v1 ,@vs))))))]
      [`(,binop ,es ...)
       #:when (binop? binop)
       (anf-e* es (lambda (vs) (k `(,binop ,@vs))))]
      [`(alloc ,e)
       (anf-e-v e (lambda (v) (k `(alloc ,v))))]
      [`(mref ,es ...)
       (anf-e* es (lambda (vs) (k `(mref ,@vs))))]
      [`(begin ,cs ... ,e)
       (let loop ([cs cs])
         (if (empty? cs)
             (anf-e e k)
             (anf-c (car cs)
                    (lambda (c)
                      (make-begin
                       (list c)
                       (loop (cdr cs)))))))]
      [v (k v)]))

  (define (anf-b b)
    (match b
      [`(define ,label (lambda (,alocs ...) ,e))
       `(define ,label (lambda ,alocs ,(anf-e e (lambda (x) x))))]))

  (match p
    [`(module ,bs ... ,e)
     ; NOTE: Imperative code, sensitive to evaluation order.
     (let* ([new-bs (map anf-b bs)]
            [new-tail (anf-e e (lambda (x) x))])
       `(module ,@(append new-bs (unbox jps)) ,new-tail))]))
