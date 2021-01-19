#lang racket/base
(require
 cpsc411/interrogator/interrogator-lib
 racket/pretty
 xml
 net/cgi)

(output-http-headers)
(display (xexpr->string (make-interrogator-form "interrogator.cgi")))
