#lang racket/base

(require
 gregor
 (only-in scribble/manual emph))

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
    (hash-set! x 'a2 (+weeks (hash-ref x 'a1) 2))
    (hash-set! x 'a3 (+weeks (hash-ref x 'a2) 1))
    (hash-set! x 'a4 (+weeks (hash-ref x 'a3) 2))
    (hash-set! x 'a5 (+weeks (hash-ref x 'a4) 1))
    (hash-set! x 'a6 (+weeks (hash-ref x 'a5) 1))
    (hash-set! x 'a7 (+weeks (hash-ref x 'a6) 1))
    (hash-set! x 'a8 (+weeks (hash-ref x 'a7) 1))
    (hash-set! x 'a9 (+weeks (hash-ref x 'a8) 1))
    (hash-set! x 'a10 (+weeks (hash-ref x 'a9) 1))
    (hash-set! x 'a11 (+weeks (hash-ref x 'a10) 1))
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
  `((,(-days (hash-ref deadline-dict 'a1) 4) . "Weekly Readings: Chp 1 and 2 (4.1 and 4.2)")
    (,(-days (hash-ref deadline-dict 'a2) 4) . "Weekly Readings: Chp 3 and 4 (4.3 and 4.4)")
    (,(-days (hash-ref deadline-dict 'a3) 4) . "Weekly Readings: Chp 5 (4.5)")
    (,(-days (hash-ref deadline-dict 'a4) 4)  . "Weekly Readings: Chp 6 (4.6)")
    (,(-days (hash-ref deadline-dict 'a5) 4)  . "Weekly Readings: Chp 7 (4.7)")
    (,(-days (hash-ref deadline-dict 'a6) 4)  . "Weekly Readings: Chp 8 (4.8)")
    (,(-days (hash-ref deadline-dict 'a7) 4)  . "Weekly Readings: Chp 9 (4.9)")
    (,(-days (hash-ref deadline-dict 'a8) 4)  . "Weekly Readings: Chp 10 (4.10)")
    (,(-days (hash-ref deadline-dict 'a9) 4)  . "Weekly Readings: Chp 11 (4.11)")
    (,(-days (hash-ref deadline-dict 'a10) 4)  . "Weekly Readings: Chp 12 (4.12)")
    (,(-days (hash-ref deadline-dict 'a11) 4)  . "Weekly Readings: Chp 13 (4.13)")
    #;(,(moment 2021 2 22) . "Weekly Readings: Chp 7 (4.7)")
    (,(moment 2021 2 25) . ,(emph "Midterm!"))

    (,(moment 2021 1 11) . "First Day of Lecture")
    (,(moment 2021 1 22) . "Last day to withdraw, without W")
    (,(moment 2021 3 12) . "Last day to withdraw, with W")
    (,(moment 2021 2 15) . "Start of Midterm Break")
    (,(moment 2021 2 19) . "End of Midterm Break")
    (,(moment 2021 4 13) . "Last Day of Lecture")))
