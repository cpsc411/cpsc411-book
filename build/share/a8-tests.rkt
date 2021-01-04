#lang racket
(require
 "a8.rkt"
 "a8-compiler-lib.rkt")

(define (v->ptr shift tag v)
  (bitwise-ior (arithmetic-shift v shift) tag))

(define (make-fixnum-ptr v)
  (v->ptr (current-fixnum-shift) (current-fixnum-tag) v))

(module+ test
  (parameterize ([current-pass-list
                  (list
                   implement-mops
                   generate-x64
                   wrap-x64-boilerplate
                   wrap-x64-run-time)])
    (check-equal?
     (execute
      `(begin
         (set! (rbp + 0) r12)
         (set! r10 (rbp + 0))
         (set! r10 (+ r10 1))
         (set! (rbp + 0) r10)
         (set! (rbp + 8) ,(car-offset))
         (set! (rbp + 16) #b111000)
         (set! r10 (rbp + 0))
         (set! r11 (rbp + 8))
         (set! r10 (+ r10 r11))
         (set! r11 (rbp + 16))
         (mset! r10 0 r11)
         (set! (rbp + 24) ,(cdr-offset))
         (set! (rbp + 32) ,(current-empty-ptr))
         (set! r10 (rbp + 0))
         (set! r11 (rbp + 24))
         (set! r10 (+ r10 r11))
         (set! r11 (rbp + 32))
         (mset! r10 0 r11)
         (set! (rbp + 24) 0)
         (set! rax (mref rbp 0))))
     '(7))

    (check-equal?
     (execute
      `(begin
         (set! (rbp + 0) r12)
         (set! r10 (rbp + 0))
         (set! r10 (+ r10 1))
         (set! (rbp + 0) r10)
         (set! (rbp + 8) ,(car-offset))
         (set! (rbp + 16) #b111000)
         (set! r10 (rbp + 0))
         (set! r11 (rbp + 8))
         (set! r10 (+ r10 r11))
         (set! r11 (rbp + 16))
         (mset! r10 0 r11)
         (set! (rbp + 24) ,(cdr-offset))
         (set! (rbp + 32) ,(current-empty-ptr))
         (set! r10 (rbp + 0))
         (set! r11 (rbp + 24))
         (set! r10 (+ r10 r11))
         (set! r11 (rbp + 32))
         (mset! r10 0 r11)
         (set! (rbp + 24) ,(car-offset))
         (set! r10 (rbp + 0))
         (set! r11 (rbp + 24))
         (set! rax (mref r10 r11))))
     7)))

(module+ test
  (parameterize ([current-pass-list
                  (list
                   patch-instructions
                   implement-mops
                   generate-x64
                   wrap-x64-boilerplate
                   wrap-x64-run-time)])
    (check-equal?
     (execute
      `(begin
         (set! (rbp + 0) r12)
         (set! (rbp + 0) (+ (rbp + 0) 1))
         (set! (rbp + 8) ,(car-offset))
         (set! (rbp + 16) #b111000)
         (mset! (rbp + 0) (rbp + 8) (rbp + 16))
         (set! (rbp + 24) ,(cdr-offset))
         (set! (rbp + 32) #b00010110)
         (mset! (rbp + 0) (rbp + 24) (rbp + 32))
         (set! (rbp + 24) 0)
         (set! rax (mref rbp 0))))
     '(7))

    (check-equal?
     (execute
      `(begin
         (set! (rbp + 0) r12)
         (set! (rbp + 0) (+ (rbp + 0) 1))
         (set! (rbp + 8) ,(car-offset))
         (set! (rbp + 16) #b111000)
         (mset! (rbp + 0) (rbp + 8) (rbp + 16))
         (set! (rbp + 24) ,(cdr-offset))
         (set! (rbp + 32) #b00010110)
         (mset! (rbp + 0) (rbp + 24) (rbp + 32))
         (set! (rbp + 24) ,(car-offset))
         (set! rax (mref (rbp + 0) (rbp + 24)))))
     7)))


(module+ test
  (parameterize ([current-pass-list
                  (list
                   select-instructions
                   expose-allocation-pointer
                   uncover-locals
                   undead-analysis
                   conflict-analysis
                   pre-assign-frame-variables
                   assign-frames
                   assign-registers
                   assign-frame-variables
                   discard-call-live
                   replace-locations
                   implement-fvars
                   expose-basic-blocks
                   flatten-program
                   patch-instructions
                   implement-mops
                   generate-x64
                   wrap-x64-boilerplate
                   wrap-x64-run-time)])

    (check-equal?
     (execute
      `(module
           (let ([x.1 ,(make-fixnum-ptr 7)])
             (let ([x.2 (alloc 16)])
               (let ([x.3 (+ x.2 ,(current-pair-tag))])
                 (begin
                   (mset! x.3 ,(car-offset) x.1)
                   (mset! x.3 ,(cdr-offset) ,(current-empty-ptr))
                   x.3))))))
     '(7))

    (check-equal?
     (execute
      `(module
           (let ([x.1 ,(make-fixnum-ptr 7)])
             (let ([x.2 (alloc 16)])
               (let ([x.3 (+ x.2 ,(current-pair-tag))])
                 (let ([x.4 (alloc 8)])
                   (begin
                     (mset! x.3 ,(car-offset) x.1)
                     (mset! x.3 ,(cdr-offset) ,(current-empty-ptr))
                     x.3)))))))
     '(7))

    (check-equal?
     (execute
      `(module
           (let ([x.1 ,(make-fixnum-ptr 7)])
             (let ([x.4 (alloc 8)])
               (let ([x.2 (alloc 16)])
                 (let ([x.3 (+ x.2 ,(current-pair-tag))])
                   (begin
                     (mset! x.3 ,(car-offset) x.1)
                     (mset! x.3 ,(cdr-offset) ,(current-empty-ptr))
                     x.3)))))))
     '(7))))

(module+ test
  (parameterize ([current-pass-list
                  (list
                   a-normalize
                   select-instructions
                   expose-allocation-pointer
                   uncover-locals
                   undead-analysis
                   conflict-analysis
                   pre-assign-frame-variables
                   assign-frames
                   assign-registers
                   assign-frame-variables
                   discard-call-live
                   replace-locations
                   implement-fvars
                   expose-basic-blocks
                   flatten-program
                   patch-instructions
                   implement-mops
                   generate-x64
                   wrap-x64-boilerplate
                   wrap-x64-run-time)])
    (check-equal?
     (execute
      `(module
           (let ([x.1 (+ (alloc 16) ,(current-pair-tag))])
             (begin
               (mset! x.1 ,(car-offset) #b111000)
               (mset! x.1 ,(cdr-offset) #b00010110)
               x.1))))
     '(7))

    (check-equal?
     (execute
      '(module
           (mref
            (let ((tmp.2646 (+ (alloc 16) 1)))
              (begin
                (mset! tmp.2646 -1 56)
                (mset! tmp.2646 7 22)
                tmp.2646))
            -1)))
     7)

    (check-equal?
     (execute
      '(module
           (let ((x.1
                  (let ((tmp.10 (+ (alloc (* 8 2)) 3)))
                    (begin (mset! tmp.10 -3 16) tmp.10))))
             (begin
               (mset! x.1 5 8)
               (mset! x.1 13 16)
               (mset! x.1 21 24)
               (mref x.1 21)))))
     3)))

(module+ test
  (parameterize ([current-pass-list
                  (list
                   specify-representation
                   a-normalize
                   select-instructions
                   expose-allocation-pointer
                   uncover-locals
                   undead-analysis
                   conflict-analysis
                   pre-assign-frame-variables
                   assign-frames
                   assign-registers
                   assign-frame-variables
                   discard-call-live
                   replace-locations
                   implement-fvars
                   expose-basic-blocks
                   flatten-program
                   patch-instructions
                   implement-mops
                   generate-x64
                   wrap-x64-boilerplate
                   wrap-x64-run-time)])

    (check-equal?
     (execute
      `(module (unsafe-car (cons 7 ()))))
     7)

    (check-equal?
     (execute
      `(module (cons (if (eq? 7 8) (unsafe-fx* 7 8) (unsafe-fx* 8 7)) ())))
     '(56))

    (check-equal?
     (execute
      `(module
           (let ([x.1 (unsafe-make-vector 0)])
             (begin
               x.1))))
     #())

    (check-equal?
     (execute
      `(module
           (let ([x.1 (unsafe-make-vector 2)])
             (begin
               (unsafe-vector-set! x.1 0 1)
               (unsafe-vector-set! x.1 1 2)
               (unsafe-vector-set! x.1 2 3)
               x.1))))
     #(1 2))

    (check-equal?
     (execute
      `(module
           (define L.loop.2
             (lambda (x.2 len.3 vec.4)
               (if (eq? x.2 len.3)
                   vec.4
                   (begin
                     (unsafe-vector-set! vec.4 x.2 0)
                     (apply L.loop.2 (unsafe-fx+ x.2 1) len.3 vec.4)))))
           (let ([x.1 (unsafe-make-vector 2)])
             (apply L.loop.2 0 2 x.1))))
     #(0 0))

    (check-equal?
     (execute
      `(module
           (let ([x.1 (make-procedure 2 2)])
             (begin
               x.1)))
      nasm-run/print-string)
     "#<procedure>")

    (check-equal?
     (execute
      `(module
           (let ([x.1 (make-procedure 0 2)])
             (begin
               (unsafe-procedure-arity x.1)))))
     2)))

(module+ test
  (parameterize ([current-pass-list
                  (list
                   implement-safe-primops
                   specify-representation
                   a-normalize
                   select-instructions
                   expose-allocation-pointer
                   uncover-locals
                   undead-analysis
                   conflict-analysis
                   pre-assign-frame-variables
                   assign-frames
                   assign-registers
                   assign-frame-variables
                   discard-call-live
                   replace-locations
                   implement-fvars
                   expose-basic-blocks
                   flatten-program
                   patch-instructions
                   implement-mops
                   generate-x64
                   wrap-x64-boilerplate
                   wrap-x64-run-time)])
    (check-equal?
     (execute
      `(module (apply cons 7 ())))
     '(7))

    (check-equal?
     (execute
      `(module (apply car (apply cons 7 ()))))
     7)

    (define errno? (and/c number? (lambda (x) (not (zero? x)))))

    (check-pred errno?
     (execute
      `(module (apply car 7))
      nasm-run/exit-code))

    (check-equal?
     (execute
      `(module (apply cons (if (apply eq? 7 8) (apply * 7 8) (apply * 8 7)) ())))
     '(56))

    (check-equal?
     (execute
      `(module
           (let ([x.1 (apply make-vector 0)])
             x.1)))
     #())

    (check-equal?
     (execute
      `(module
           (let ([x.1 (apply make-vector 2)])
             x.1)))
     #(0 0))

    (check-pred errno?
     (execute `(module (apply make-vector #\x)) nasm-run/exit-code))

    (check-pred number?
     (execute
      `(module
           (let ([x.1 (apply make-vector 2)])
             (let ([x.2 (apply vector-set! x.1 0 1)])
               (let ([x.3 (apply vector-set! x.1 1 2)])
                 (let ([x.4 (apply vector-set! x.1 2 3)])
                   x.1)))))
      nasm-run/exit-code))))
