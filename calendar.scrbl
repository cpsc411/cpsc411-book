#lang scribble/manual

@(require
  gregor
  "config.rkt")

@title{Course Calendar}
We list some important dates on this calendar for your reference.
This calendar is subject to change.

@tabular[
  #:style 'boxed
  #:column-properties '(left right)
  #:row-properties '(bottom-border ())
  `(,(list @bold{Date} @bold{Note})
    ,@(for/list ([als (sort
                       (append
                        important-dates
                        ; convert deadline-dict into important dates
                        (for/list ([(k v) deadline-dict])
                          (cons v (format "~a due" k))))
                     moment<?
                     #:key car)])
      ; render dates
      (list (~t (car als) "E, MMM d") (cdr als))))]
