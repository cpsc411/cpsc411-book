#lang racket/base

(require 
  pict 
  racket/class 
  racket/draw
  racket/math
  racket/function)

(provide 
  sys-v-stack-diagram
  frame-var-diagram)

;; TODO: Make a make-memory-diagram function.

(define sys-v-stack-diagram
  (parameterize ([dc-for-text-size (make-object bitmap-dc%
                                                (make-bitmap 1 1))])
    (let* ([HEIGHT 80]
           [TOTAL-WIDTH 250]
           [FRAME-WIDTH (/ TOTAL-WIDTH 10)]
           [make-stack-frame 
             (lambda (contents [width FRAME-WIDTH] [rot (/ pi 2)])
               (cc-superimpose
                 (rectangle width HEIGHT)
                 (text contents '() 12 rot)))]
           [frame-label (lambda (contents)
                          (text contents '() 12 (/ pi 3)))]
           [stack-diagram (lambda (initial-frame als)
                            (define labels (map (compose frame-label car) als))
                            (define frame-picts (map cdr als))
                            (vl-append
                              (apply hb-append
                                     (let loop ([labels labels]
                                                [frames frame-picts]
                                                [prev-label #f]
                                                [prev-frame #f])
                                       (if (null? labels)
                                           '()
                                           (let ([r (cons (car labels)
                                                          (loop (cdr labels)
                                                                (cdr frames)
                                                                (car labels)
                                                                (car frames)))])
                                             (if prev-label
                                                 (cons
                                                   (blank (- (pict-width prev-frame) 
                                                             (pict-width prev-label))
                                                          0)
                                                   r)
                                                 r)))))
                              (apply hb-append 
                                     (cons initial-frame
                                           frame-picts))))])
      (stack-diagram
        (blank (pict-width (text "M")) 0)
        `(("Low address" . ,(cc-superimpose
                               (rectangle TOTAL-WIDTH HEIGHT)
                               (text "Free Space" '() 24)))
          ("rsp - 16" . ,(make-stack-frame "(also free)"))
          ("rsp - 8" . ,(make-stack-frame "(also free)"))
          ("rsp" . ,(make-stack-frame "argc"))
          ("rsp + 8" . ,(make-stack-frame "argv[0]"))
          ("" . ,(make-stack-frame "..." (* FRAME-WIDTH 2) 0))
          ("High address" . ,(blank)))))))

(define frame-var-diagram
  (vl-append
    sys-v-stack-diagram
    (hb-append
      (blank 295 0)
      (text "fv1" '() 12 (/ pi 3))
      (blank (- (/ 300 12) 
                (pict-width (text "fv1" '() 12 (/ pi 3))))
                0)
      (text "fv0" '() 12 (/ pi 3)))))

(module+ main
  sys-v-stack-diagram
  frame-var-diagram)
