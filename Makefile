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

build: build/index.html

#build/cpsc411.sxref:
#	mkdir -p build
#	scribble --dest ./build --dest-name cpsc411 --htmls --info-out build/cpsc411.sxref +m --redirect-main "https://docs.racket-lang.org" ++style assignment/custom.css `racket -e "(void (write-string (path->string (collection-file-path \"cpsc411.scrbl\" \"cpsc411\"))))"`

build/index.html: build/cpsc411.sxref .racodeps assignment/* appendix/*.scrbl chapter/*.scrbl *.scrbl Makefile config.rkt
	scribble --dest-name build --htmls --redirect-main https://docs.racket-lang.org/ +m ++style assignment/custom.css index.scrbl

#%.html: %.scrbl .racodeps
#	scribble --html --redirect-main "https://docs.racket-lang.org" ++xref-in setup/xref load-collections-xref ++style assignment/custom.css $<

clean:
	echo "You should manually run `git clean -ixd` in build."
	rm -rf compiled/

sync: build/*.html build/*.cgi
	rsync -avzl --delete --delete-excluded --chmod=Dg+x,g+r build/ cs-411@remote.students.cs.ubc.ca:public_html/2022w2/

serve: build/index.html
	racket -t serve.rkt

tmpsync: build/*.html
	rsync -avzl --delete --delete-excluded --chmod=Dg+x,g+r build/ http@wjb:www/tmp/compilers/
