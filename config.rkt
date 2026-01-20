#lang racket/base

(require
 gregor
 (only-in scribble/manual emph url))

(provide (all-defined-out))

(define course-number "CPSC 411")
(define lecture-room (list "FSC 1611 -- " (url "https://learningspaces.ubc.ca/classrooms/fsc-1611")))
(define semester "2025w2")
(define gh-org "cpsc411-2025w-t2")
(define course-title "Introduction to Compiler Construction")

(define url-root (string-append "https://www.students.cs.ubc.ca/~cs-411/" semester))

;; Actually, there is a recursive binding construct
;;
;; NOTE: The key name is meaningful.
(define deadline-dict
  (let ([x (make-hasheq)])
    ;; First Sunday of Term
    (hash-set! x 'assignment-0 (moment 2026 1 11 23 59 59))
    (hash-set! x 'milestone-1 (+weeks (hash-ref x 'assignment-0) 1))
    (hash-set! x 'milestone-2 (+weeks (hash-ref x 'milestone-1) 2))
    (hash-set! x 'milestone-3 (+weeks (hash-ref x 'milestone-2) 1))
    (hash-set! x 'milestone-4 (+weeks (hash-ref x 'milestone-3) 2))
    (hash-set! x 'milestone-5 (+weeks (hash-ref x 'milestone-4) 1))
    (hash-set! x 'milestone-6 (+weeks (hash-ref x 'milestone-5) 2))
    (hash-set! x 'milestone-7 (+weeks (hash-ref x 'milestone-6) 1))
    (hash-set! x 'milestone-8 (+weeks (hash-ref x 'milestone-7) 1))
    (hash-set! x 'milestone-9 (+weeks (hash-ref x 'milestone-8) 1))
    #;(hash-set! x 'a9 (+days (+weeks (hash-ref x 'milestone-8) 1) 4))
    (hash-set! x 'milestone-10 (+weeks (hash-ref x 'milestone-9) 1))
    #;(hash-set! x 'a11 (+weeks (hash-ref x 'a10) 1))
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
  `((,(-days (hash-ref deadline-dict 'milestone-1) 4) . "Weekly Readings: Chp 1 and 2 (4.1 and 4.2)")
    (,(-days (hash-ref deadline-dict 'milestone-2) 4) . "Weekly Readings: Chp 3 and 4 (4.3 and 4.4)")
    (,(-days (hash-ref deadline-dict 'milestone-3) 4) . "Weekly Readings: Chp 5 (4.5)")
    (,(-days (hash-ref deadline-dict 'milestone-4) 4)  . "Weekly Readings: Chp 6 (4.6)")
    (,(-days (hash-ref deadline-dict 'milestone-5) 4)  . "Weekly Readings: Chp 7 (4.7)")
    (,(-days (hash-ref deadline-dict 'milestone-6) 4)  . "Weekly Readings: Chp 8 (4.8)")
    (,(-days (hash-ref deadline-dict 'milestone-7) 4)  . "Weekly Readings: Chp 9 (4.9)")
    (,(-days (hash-ref deadline-dict 'milestone-8) 4)  . "Weekly Readings: Chp 10 (4.10)")
    (,(-days (hash-ref deadline-dict 'milestone-9) 4)  . "Weekly Readings: Chp 11 (4.11)")
    (,(-days (hash-ref deadline-dict 'milestone-10) 4)  . "Weekly Readings: Chp 12 and 14 (4.12, 4.14)")
    #;(,(-days (hash-ref deadline-dict 'a11) 4)  . "Weekly Readings: Chp 13 (4.13)")
    #;(,(moment 2021 2 22) . "Weekly Readings: Chp 7 (4.7)")
    #;(,(moment 2022 2 28) . ,(emph "Midterm! (maybe)"))

    (,(moment 2026 1 5) . "First Day of Lecture")
    #;(,(moment 2023 1 17) . "No lecture; out at POPL")
    #;(,(moment 2023 1 19) . "No lecture; out at POPL")
    (,(moment 2026 1 16) . "Last day to withdraw, without W")
    (,(moment 2026 3 6) . "Last day to withdraw, with W")
    (,(moment 2026 2 16) . "Start of Midterm Break")
    (,(moment 2026 2 20) . "End of Midterm Break")
    (,(moment 2026 4 10) . "Last Day of Lecture")
    (,(moment 2026 4 14) . "Start of exam week")
    (,(moment 2026 4 25) . "End of exam week")
    ))
