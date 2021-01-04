#lang racket/base

(require gregor)

(provide (all-defined-out))

(define course-number "CPSC 411")
(define semester "2020w2")
(define course-title "Introduction to Compiler Construction")

(define url-root (string-append "https://www.students.cs.ubc.ca/~cs-411/" semester))

;; TODO: Are you serious? Racket can't handle a recursive value?!
(define deadline-dict
  (let ([x (make-hasheq)])
    (hash-set! x 'a0 (moment 2021 1 15 23 59 59))
    (hash-set! x 'a1 (+weeks (hash-ref x 'a0) 1))
    (hash-set! x 'a2 (+weeks (hash-ref x 'a1) 1))
    (hash-set! x 'a3 (+weeks (hash-ref x 'a2) 1))
    (hash-set! x 'a4 (+weeks (hash-ref x 'a3) 1))
    (hash-set! x 'a5 (+weeks (hash-ref x 'a4) 1))
    (hash-set! x 'a6 (+weeks (hash-ref x 'a5) 1))
    #;(hash-set! x 'a7 (+weeks (hash-ref x 'a6) 1))
    #;(hash-set! x 'a8 (+weeks (hash-ref x 'a7) 1))
    #;(hash-set! x 'a9 (+weeks (hash-ref x 'a8) 1))
    x)
  #;`((a0 . ,(moment 2019 1 12 23 59 59))
    (a1 . ,(+weeks (moment 2019 1 12 23 59 59) 1))
    (a2 . ,(+weeks (moment 2019 1 12 23 59 59) 2))
    (a3 . ,(+weeks (moment 2019 1 12 23 59 59) 3))
    (a4 . ,(+weeks (moment 2019 1 12 23 59 59) 4))
    (a5 . ,(+weeks (moment 2019 1 12 23 59 59) 5))
    (a6 . ,(+weeks (moment 2019 1 12 23 59 59) 6))))

(define important-dates
  ;; List of date x note
  `((,(moment 2021 1 11) . "First Day of Lecture")
    (,(moment 2021 1 22) . "Last day to withdraw, without W")
    (,(moment 2021 3 12) . "Last day to withdraw, with W")
    (,(moment 2021 2 15) . "Start of Midterm Break")
    (,(moment 2021 2 19) . "End of Midterm Break")
    (,(moment 2021 4 13) . "Last Day of Lecture")))
