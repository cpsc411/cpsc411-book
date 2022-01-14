#lang scribble/manual

@title[#:style '(toc) #:tag-prefix "prelim:"]{Appendix: Racket Preliminaries}

Every compiler must choose a @deftech{host language}, the language in which we
write the compiler.
Our @tech{host language} is Racket.
The following notes will help bring you up to speed on software engineering in
Racket.

@itemlist[
@item{@other-doc['(lib "scribblings/guide/guide.scrbl")]}
@item{@other-doc['(lib "scribblings/reference/reference.scrbl")]}
@item{@secref["other-editors" #:doc '(lib "scribblings/guide/guide.scrbl")]}
@item{@other-doc['(lib "scribblings/style/style.scrbl")]
In particular:
@itemlist[
@item[@secref["Units_of_Code" #:doc '(lib "scribblings/style/style.scrbl")]]
@item[@secref["Choosing_the_Right_Construct" #:doc '(lib "scribblings/style/style.scrbl")]]
@item[@secref["Textual_Matters" #:doc '(lib "scribblings/style/style.scrbl")]]
]
}
@item{Racket Unit testing resources:
@itemlist[
@item{@secref["quick-start" #:doc '(lib "rackunit/scribblings/rackunit.scrbl")]}
@item{@other-doc['(lib "rackunit/scribblings/rackunit.scrbl")]}
@item{@url{https://beautifulracket.com/explainer/unit-testing.html}}
]}
@item{
@(local-table-of-contents #:style 'immediate-only)
}
]

@;include-section{organizing-code.scrbl}
@include-section{match-tutorial.scrbl}
