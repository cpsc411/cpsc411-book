(define (make-cached-eval name . rest)
  (define cache (cachefile name))
  (define eval-log (box '()))
  (define allow-replay (box #t))
  (define ev
    (let-values ([(ev-replay ev-record)
                  (values
                   (make-log-based-eval
                    cache
                    (if (file-exists? cache)
                        'replay
                        'record))
                   (lambda rest (error 'make-cached-eval "uninitialized")))])
      (lambda (x)
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (if (regexp-match #rx"unable to replay evaluation"
                                             (exn-message e))
                               (begin
                                 (set! ev-record (make-log-based-eval cache 'record))
                                 (for ([r (reverse (unbox eval-log))])
                                   (ev-record r))
                                 (set-box! allow-replay #f))
                               (raise e)))])
          (if allow-replay
              (begin
                (set-box! eval-log (cons x (unbox eval-log)))
                (ev-replay x))
              (ev-record x))))))
  (for ([r rest])
    (ev r))
  ev)

