#lang racket/base

(require
 web-server/dispatchers/dispatch
 web-server/servlet-env)

(serve/servlet (lambda (_) (next-dispatcher))
               #:servlet-path "/index.html"
               #:extra-files-paths (list "build")
               #:port 8000
               #:listen-ip #f
               #:launch-browser? #t)
