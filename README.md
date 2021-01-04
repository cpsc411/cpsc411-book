cpsc411-book
=======
This collection defines the "book" for UBC's Computer Science 411 (CPSC 411),
"Introduction to Compiler Construction".

This collection is not really meant to be built like a normal Racket package yet.
Instead, it's meant to be forked and modified to fit your course.
This collection can be easily built into a website with added notes and pages,
or built as a PDF (TODO).

You can view the temporary website at https://www.williamjbowman.com/tmp/compilers/

## Usage
First, install this book by running `raco pkg install` in this (the `cpsc411-book`)
directory.
This is necessary to ensure various dependencies and packages are built, and
speeds up subsequent calls builds.

After a major update to the book, run `raco pkg remove cpsc411-book; raco pkg
install` from this directory again.

Run `make build` to build the website, which contains a copy of the book.
Run `make serve` to run a local server for the local version of the website.
