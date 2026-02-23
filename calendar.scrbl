#lang scribble/manual

@(require
  gregor
  "config.rkt")

@title{Course Calendar}
We list some important dates on this calendar for your reference.
This calendar is subject to change.

@tabular[
  #:style 'boxed
  #:column-properties '(left left left)
  #:row-properties '(bottom-border ())
  `(,(list @bold{Lecture} @bold{Date} @bold{Note})
    ,@(for/list ([als (sort
                       (append
                        ; convert lectures-dict into important dates
                        (for/list ([l lectures]
                                   [i (in-naturals lecture-start)])
                          (cons (format "~a" i) l))
                        important-dates
                        ; convert deadline-dict into important dates
                        (for/list ([(k v) deadline-dict])
                          (cons "" (cons v (format "~a due" k)))))
                     moment<?
                     #:key cadr)])
      ; render dates
      (list (car als) (~t (cadr als) "E, MMM d") (cddr als))))]
@;margin-note{a11 does not not actually exist. I've left it in the calendar for technical reasons.}
