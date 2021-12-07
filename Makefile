.PHONY: all clean sync serve help build docs-server

help:
	@echo "make build	       Build the website."
	@echo "make serve        Launch and browse a local Racket server to serve the website."
	@echo "make sync         Sync the website to the public production server."
	@echo "make clean        Delete the built website."
	@echo "make help         Print this help message."

all: help

.racodeps: info.rkt
	raco pkg install --skip-installed --batch --auto
	touch $@

build: build/.done

build/.done: .racodeps assignment/* appendix/*.scrbl chapter/*.scrbl *.scrbl Makefile config.rkt
	scribble --dest-name build --htmls --redirect-main "https://www.students.cs.ubc.ca/~cs-411/docs/" ++xref-in setup/xref load-collections-xref ++style assignment/custom.css index.scrbl
	touch $@

docs-server:
	rsync --inplace -avz --delete-excluded --delete\
    --exclude srfi/\
    --exclude gui/\
    --exclude teachpack/\
    --exclude plot/\
    --exclude redex/\
    --exclude r6rs/\
    --exclude pict/\
    --exclude raco/\
    --exclude mzlib/\
    --exclude draw/\
    --exclude scheme/\
    --exclude foreign/\
    --exclude syntax/\
    --exclude tools/\
    --exclude r5rs/\
    --exclude mrlib/\
    --exclude net/\
    --exclude inside/\
    --exclude ts-reference/\
    --exclude htdp-langs/\
    --exclude web-server-internal/\
    --exclude drracket/\
    --exclude web-server/\
    --exclude data/\
    --exclude cur/\
    --exclude data/\
    --exclude gregor/\
    --exclude pkg/\
    --exclude lazy/\
    --exclude file/\
    --exclude 'cldr*/'\
    --exclude htdp/\
    --exclude htdp-ptr/\
    --exclude license/\
    --exclude bitsyntax/\
    --exclude browser/\
    --exclude compatibility/\
    --exclude cookies/\
    --exclude frog/\
    --exclude future-visualizer/\
    --exclude markdown/\
    --exclude json/\
    --exclude graphviz/\
    --exclude macro-debugger/\
    --exclude match-plus/\
    --exclude memoize/\
    --exclude metapict/\
    --exclude mzscheme/\
    --exclude openssl/\
    --exclude osx-ssl/\
    --exclude parsack/\
    --exclude parser-tools/\
    --exclude plai/\
    --exclude planet/\
    --exclude profile/\
    --exclude quick/\
    --exclude quickscript/\
    --exclude readline/\
    --exclude rosette-guide/\
    --exclude sexp-diff/\
    --exclude win32-ssl/\
    --exclude stepper/\
    --exclude slideshow/\
    --exclude ts-guide/\
    --exclude turtles/\
    --exclude tzinfo/\
    --exclude xrepl/\
    --exclude xml/\
    --exclude with-cache/\
    --exclude docindex.sqlite\
    --exclude '*.rktd'\
    --exclude '*.sxref'\
    ${PLTHOME}/racket/doc/ cs-411@remote.students.cs.ubc.ca:public_html/docs/

#%.html: %.scrbl .racodeps
#	scribble --html --redirect-main "https://docs.racket-lang.org" ++xref-in setup/xref load-collections-xref ++style assignment/custom.css $<

clean:
	echo "You should manually run `git clean -ixd` in build."
	rm -rf compiled/

sync: docs-server build/*.html build/*.cgi
	rsync -avzl --delete --delete-excluded --chmod=Dg+x,g+r build/ cs-411@remote.students.cs.ubc.ca:public_html/2020w2/

serve: build/.done
	racket -t serve.rkt

tmpsync: build/*.html
	rsync -avzl --delete --delete-excluded --chmod=Dg+x,g+r build/ http@wjb:www/tmp/compilers/
