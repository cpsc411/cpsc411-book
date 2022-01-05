#lang at-exp racket

(require
 (for-syntax
  racket/base
  racket/syntax
  racket/function)
 racket/dict
 racket/runtime-path
 with-cache
 (except-in scribble/manual racketgrammar* url)
 (rename-in scribble/base [url base:url])
 scribble/example
 scribble/core
 scribble/html-properties
 scribble/decode
 scribble/bettergrammar
 (rename-in scribble/bettergrammar [bettergrammar* racketgrammar*])
 cpsc411/compiler-lib
 #;(only-in graphviz [dot->pict super:dot->pict]
          run-dot)
 "../config.rkt"
 gregor)

(provide
 (all-defined-out)
 (all-from-out
  racket/dict
  scribble/bettergrammar
  scribble/core
  with-cache
  scribble/manual
  scribble/example
  "../config.rkt"))

(define-runtime-path custom-css "custom.css")

(define (load-custom-css)
  (elem #:style (make-style "" (list (make-css-addition custom-css)))))

(define (digression . c)
  (make-nested-flow
   (make-style "refpara" '(command never-indents))
   (list
    (make-nested-flow
     (make-style "refcolumn refcolumn-inline" (list #;(make-css-addition "custom.css")))
     (list
      (make-nested-flow
       (make-style "refcontent" null)
       (decode-flow
        (cons (para (emph "Design digression:")) c))))))))

(define x64-abi
  (hyperlink "https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI" "x64 System V ABI"))

; deprecated
(define object-code racketvarfont)
(define metavar racketvarfont)

(define ie @emph{i.e.,})
(define eg @emph{e.g.,})
(define (url . rest)
  (base:url (apply string-append rest)))

(define a0-tech (curry tech #:tag-prefixes '("milestone:" "a0:")))
(define a1-tech (curry tech #:tag-prefixes '("milestone:" "a1:")))
(define a2-tech (curry tech #:tag-prefixes '("milestone:" "a2:")))
(define a3-tech (curry tech #:tag-prefixes '("milestone:" "a3:")))
(define a4-tech (curry tech #:tag-prefixes '("milestone:" "a4:")))
(define a6-tech (curry tech #:tag-prefixes '("milestone:" "a6:")))
(define a7-tech (curry tech #:tag-prefixes '("milestone:" "a7:")))
(define a8-tech (curry tech #:tag-prefixes '("milestone:" "a8:")))
(define a9-tech (curry tech #:tag-prefixes '("milestone:" "a9:")))

(define ch1-tech (curry tech #:tag-prefixes '("book:" "chp1:")))
(define ch2-tech (curry tech #:tag-prefixes '("book:" "chp2:")))
(define ch3-tech (curry tech #:tag-prefixes '("book:" "chp3:")))
(define ch4-tech (curry tech #:tag-prefixes '("book:" "chp4:")))
(define ch5-tech (curry tech #:tag-prefixes '("book:" "chp5:")))
(define ch6-tech (curry tech #:tag-prefixes '("book:" "chp-return:")))

(define ch7-tech (curry tech #:tag-prefixes '("book:" "chp-immediates:")))
(define ch8-tech (curry tech #:tag-prefixes '("book:" "chp-structured-data:")))
(define ch9-tech (curry tech #:tag-prefixes '("book:" "chp-closures:")))

(define ch-ra-tech (curry tech #:tag-prefixes '("book:" "chp-reg-alloc:")))

(define (Chref n x) (Secref #:tag-prefixes `("book:" ,(format "chp~a:" n)) x))

(define-syntax-rule (typeset-passlist id ...)
  (itemlist
   (item (racket id))
   ...))

(define-syntax (deflangs stx)
  (syntax-case stx ()
    [(_ langs ...)
     (with-syntax ([(preds? ...) (map (curry format-id stx "~a?") (syntax->list #'(langs ...)))])
       #`(begin
           #,@(for/list ([pred? (syntax->list #'(preds? ...))]
                         [lang (syntax->list #'(langs ...))])
                #`(begin
                    (defproc (#,pred? [a any/c]) boolean? @elem{Decides whether @racket[a] is a valid program in the } @racket[#,lang] " grammar. The first non-terminal in the grammar defines valid programs.")
                    (defthing #:kind "" #,lang grammar? (bettergrammar* #,lang))))))]))

#;(define tech list)
#;(define deftech list)

(define (todo . rest) (void)#;(apply margin-note "TODO: " rest))

(define (question . rest)
  (para @bold{Question: } rest))

;;@todo{Use numberers}
;;@todo{Get exercises indexed and in side-bar/TOC}
(define ecounter (box 1))
(define (reset-exercise-counter!) (set-box! ecounter 1))

(define exercise
  (lambda (#:number
           [number (begin
                     (let ([x (unbox ecounter)])
                       (set-box! ecounter (add1 x))
                       x))]
           #:optional [opt? #f]
           . rest)
    (nested #:style "boxed"
            @bold{Exercise @~a[number]@(if opt? " (optional)" ""): }
            rest)))

(define ccounter (box 1))
(define (reset-challenge-counter!) (set-box! ccounter 1))
(define challenge
  (lambda rest
    (define count (unbox ccounter))
    (set-box! ccounter (add1 count))
    (nested #:style "boxed"
            @bold{Challenge @~a[count]: }
            rest)))

(define (share u [text #f])
  (let ([path (format "share/~a" u)])
    (hyperlink path (or text u))))

(define (exit-code-displayer x) (format "> echo $?~n> ~a" x))

(define (nasm-call-string type name.s name.o)
  @~a{> nasm -f @(bin-format type) -o @|name.o| @|name.s|})

;; This is duplicated in cpsc411-lib/compiler-lib.rkt
(define (ld-call-string type name.o name.exe)
  (match type
    ['unix
     @~a{> ld -e start -o @|name.exe| @|name.o|}]
    ['macosx
     ;; no_pie needed until we switch to position independent code, using lea
     ;; instead of mov to function/static data pointers
     @~a{> ld -no_pie -macosx_version_min 10.6 -e start -o @|name.exe| @|name.o|}]
    ['windows
     @~a{> golink /entry Start /fo @|name.exe| @|name.o| kernel32.dll}]))

(struct fake (str))

(define cached-example-counter (box 0))
(define (eg-counter!)
  (let ([x (unbox cached-example-counter)])
    (set-box! cached-example-counter (add1 x))
    x))

(define (nasm-example #:file-name (name "example.s")
                      #:result (result-expected #f)
                      #:runner (runner nasm-run/print-string)
                      #:result-displayer (displayer
                                          (lambda (x) (~a (if (fake? x)
                                                              (fake-str x)
                                                              x))))
                      #:type type
                      #:nasm-call (nasm-call nasm-call-string)
                      #:ld-call (ld-call ld-call-string)
                      . strs)
  (unless (regexp-match ".*\\.s" name)
    (error 'nasm-example "File name should and in '.s', but name is ~a" name))
  (unless (or (equal? (system-type) type)
              result-expected)
    (error
     'nasm-example
     "Expected system type ~a, but running on system ~a; please specify a result."
     type (system-type)))

  (define str (apply string-append strs))

  (define result-actual
    (if (equal? (system-type) type)
        #;(runner str)
        (with-cache (cachefile (format "~a-~a" (eg-counter!) name))
          (thunk (runner str)))
        result-expected))
  (unless (or (fake? result-expected)
              (not result-expected)
              (equal? result-actual result-expected))
    (error
     'nasm-example
     "Result of example differs from expected. Expected ~a, got ~a"
     result-expected
     result-actual))
  (define name.o (string-replace name ".s" ".o"))
  (define name.exe (string-replace name ".s" ".exe"))

  (list
   ; TODO: Needs an abstraction. Maybe use pygments package?
   (nested #:style 'code-inset
           (typeset-code str))
   (nested #:style 'code-inset
           (typeset-code #:indent 0
                         (nasm-call type name name.o)
                         "\n"
                         (ld-call type name.o name.exe)
                         "\n"
                         @~a{> ./@|name.exe|} "\n" (displayer result-actual)))))
;;@todo{Better abstraction for type setting nasm examples}

;; TODO HACK shouldn't be this much code in the macro
(define-syntax (cache-examples syn)
  (syntax-case syn ()
    [(_ args ...)
     #'(with-cache (cachefile (format "scribble-example~a" (eg-counter!)))
         (lambda () (examples args ...)))]))

(define (make-cached-eval name . rest)
  (define cache (cachefile name))
  (define ev
    (make-base-eval)
    #;(make-log-based-eval
     cache
     (if (file-exists? cache)
         'replay
         'record)))
  (for ([r rest])
    (ev r))
  ev)

(define (run-dot str format)
  (define cmd (string-append "dot -y -T" format))
  (match (process cmd)
    [(list stdout stdin pid stderr ctl)
     (write-string str stdin)
     (newline stdin)
     (close-output-port stdin)
     (cond [(eq? (ctl 'status) 'done-error) (error (port->string stderr))]
           [else stdout])]))

(require (only-in pict bitmap) racket/draw)
#;(define (dot->pict . rest)
  (bitmap (make-object bitmap% (run-dot (apply string-append rest) "png"))))

(require (only-in xml cdata)
         scribble/html-properties)
(define (dot->svg . rest)
  (elem #:style
        (style #f
               (list
                (xexpr-property
                 (cdata #f #f (apply string-append (drop (port->lines (run-dot (apply string-append rest) "svg")) 3)))
                 (cdata #f #f ""))))))


(define (due sym)
  (~t (dict-ref deadline-dict sym) "E, MMMM d, y h:mm a"))
