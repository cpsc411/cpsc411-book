#lang racket
(require rackunit)
(provide FACT_S)

; String -> void
; Raises an error with str as the error message.
(define (TODO str) (error str))

; String -> Integer
; or
; String -> Procedure? -> Integer
;
; Runs the string as a shell command, and either returns the exit code or fails
; with an error message including command and the non-successful exit code.
; Options takes a predicate as the second argument, which takes an exit code
; and returns true if the exit code indicates success.
(define (system/exit-code! str [success? zero?])
  (let ([code (system/exit-code str)])
    (unless (success? code)
      (error (format "Command '~a' failed with exit code '~a'" str code)))
    code))

; An x64 Program
(define FACT_S (TODO #<<EOS
Fill in with the x64 program implementing fact.s,
represented as a string. You may want to use a "here string", as demonstrated here.
Consult the Racket documentation to better understand how here strings work.
EOS
))

; x64 Program -> File name
; Takes an x64 Program, compiles it to an executable, and returns the name of
; the generated executable.
(define (compile str)
  (TODO "Read the implementation of compile, then remove this TODO.")
  (define p (path->string (make-temporary-file "~a.s")))

  (define o (string-replace p ".s" ".o"))

  (define exe (string-replace p ".s" ".exe"))

  (with-output-to-file p (thunk (display str)) #:exists 'replace)
  (system/exit-code! (format "nasm -f elf64 ~a -o ~a" p o))
  (system/exit-code! (format "ld -e start -o ~a ~a" exe o))
  exe)

; x64 Program -> Natural
; Takes an x64 Program, compiles it to an executable, and runs it, returning
; the error code.
(define (execute str)
  (TODO "Read the implementation of execute, then remove this TODO.")
  (system/exit-code! (compile str) number?))

(module+ test
  (check-regexp-match
    #rx"2\\.(13|14|15)"
    (with-output-to-string (thunk (system "nasm --version"))))

  (check-regexp-match
    #rx"\\.exe"
    (compile FACT_S))

  (check-equal? (execute FACT_S) 120))
