;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval . (put 'for/foldr 'racket-indent-function 2))
                     (eval . (put 'module 'racket-indent-function 0))
                     (eval . (put 'return-point 'racket-indent-function 1))))
 (scribble-mode . ((eval . (put 'nd-match 'racket-indent-function 1)))))
