#lang racket/base

(require
 gregor
 (only-in scribble/manual emph))

(provide (all-defined-out))

(define course-number "CPSC 411")
(define semester "2021w2")
(define gh-org "cpsc411-2021w-t2")
(define course-title "Introduction to Compiler Construction")

(define url-root (string-append "https://www.students.cs.ubc.ca/~cs-411/" semester))

;; TODO: Are you serious? Racket can't handle a recursive value?!
(define deadline-dict
  (let ([x (make-hasheq)])
    ;; First Friday of Term
    (hash-set! x 'a0 (moment 2022 1 14 23 59 59))
    (hash-set! x 'a1 (+weeks (hash-ref x 'a0) 1))
    (hash-set! x 'a2 (+weeks (hash-ref x 'a1) 1))
    (hash-set! x 'a3 (+weeks (hash-ref x 'a2) 1))
    ;; Change from Friday to Sunday due dates by popular request
    (hash-set! x 'a4 (+days (+weeks (hash-ref x 'a3) 2) 2))
    (hash-set! x 'a5 (+weeks (hash-ref x 'a4) 1))
    (hash-set! x 'a6 (+weeks (hash-ref x 'a5) 2))
    (hash-set! x 'a7 (+weeks (hash-ref x 'a6) 1))
    (hash-set! x 'a8 (+weeks (hash-ref x 'a7) 1))
    (hash-set! x 'a9 (+weeks (hash-ref x 'a8) 1))
    #;(hash-set! x 'a9 (+days (+weeks (hash-ref x 'a8) 1) 4))
    (hash-set! x 'a10 (+weeks (hash-ref x 'a9) 1))
    #;(hash-set! x 'a10 (moment 2021 4 16 23 59 59))
    #;(hash-set! x 'a11 (moment 2022 4 16 23 59 59)
               #;(+weeks (hash-ref x 'a10) 1))
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
  `((,(-days (hash-ref deadline-dict 'a1) 4) . "Weekly Readings: Chp 1 and 2 (5.1 and 5.2)")
    (,(-days (hash-ref deadline-dict 'a2) 4) . "Weekly Readings: Chp 3 and 4 (5.3 and 5.4)")
    (,(-days (hash-ref deadline-dict 'a3) 4) . "Weekly Readings: Chp 5 (5.5)")
    (,(-days (hash-ref deadline-dict 'a4) 4)  . "Weekly Readings: Chp 6 (5.6)")
    (,(-days (hash-ref deadline-dict 'a5) 4)  . "Weekly Readings: Chp 7 (5.7)")
    (,(-days (hash-ref deadline-dict 'a6) 4)  . "Weekly Readings: Chp 8 (5.8)")
    (,(-days (hash-ref deadline-dict 'a7) 4)  . "Weekly Readings: Chp 9 (5.9)")
    (,(-days (hash-ref deadline-dict 'a8) 4)  . "Weekly Readings: Chp 10 (5.10)")
    (,(-days (hash-ref deadline-dict 'a9) 4)  . "Weekly Readings: Chp 11 (5.11)")
    (,(-days (hash-ref deadline-dict 'a10) 4)  . "Weekly Readings: Chp 12 (5.12)")
    #;(,(-days (hash-ref deadline-dict 'a11) 4)  . "Weekly Readings: Chp 13 (5.13)")
    #;(,(moment 2021 2 22) . "Weekly Readings: Chp 7 (5.7)")
    (,(moment 2022 2 28) . ,(emph "Midterm! (maybe)"))

    (,(moment 2022 1 11) . "First Day of Lecture")
    (,(moment 2022 1 18) . "No lecture; out at POPL")
    (,(moment 2022 1 20) . "No lecture; out at POPL")
    (,(moment 2022 1 21) . "Last day to withdraw, without W")
    (,(moment 2022 3 4) . "Last day to withdraw, with W")
    (,(moment 2022 2 21) . "Start of Midterm Break")
    (,(moment 2022 2 25) . "End of Midterm Break")
    (,(moment 2022 4 8) . "Last Day of Lecture")))
