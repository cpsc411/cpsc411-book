.PHONY: all clean

all: a1.html

%.html: %.scrbl lib.rkt
	scribble --redirect-main "https://doc.racket-lang.org" ++xref-in setup/xref load-collections-xref $<

clean:
	rm *.html *.js
