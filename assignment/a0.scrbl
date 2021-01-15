#lang reader "assignment-lang.rkt"

@(reset-exercise-counter!)
@(reset-challenge-counter!)

@title[#:tag "top" #:tag-prefix "a0:"]{Assignment 0: x64 to elf64}

@section{Assignment Summary}
The goal of this assignment is to ensure that you have a working environment to
start writing your compiler.
Modern compiler construction always starts from an existing language and
toolchain.
This assignment will instruct you to install and test the required tools.

This assignment is due @(due 'a0).

You should have a git repository created for you at @url{https://github.students.cs.ubc.ca/orgs/cpsc411-2020w-t2/}

@subsubsub*section{Assignment Checklist}
@itemlist[
@item{Either:
  @itemlist[
  @item{Connect to @tt{ssh://remote.students.cs.ubc.ca}, or}
  @item{Setup a 64-bit Linux (virtual) machine for local development}
  ]
}
@item{Ensure @code{racket} >= 7.3 is installed.}
@item{Ensure @code{nasm} >= 2.13 is installed.}
@item{Ensure @code{nasm} is callable from Racket.}
@item{Compile and execute fact.s from the command line.}
@item{Compile and execute fact.s from Racket.}
]

@section{Language Diagram}

@dot->svg{
digraph {

node [ shape="box" ]

/* The Languages */

L0 [label="x64"];
L1 [label="elf64"];

/* The Passes */

edge [fontname="Courier"]

L0 -> L1 [label="nasm"];
}
}

@section{Preface}
Throughout this course, each assignment will come in two parts: an
existing target language, and a new source language.
The target language will be the same as the source language from the previous
week's assignment.
The goal of the assignment will be to identify limitations in the target
language, design new abstractions that address those limitations, and implement
a new source language with those abstractions.
The implementation will be at least a compiler from source to target.
The new compiler will be an extension of the old compiler.

However, for the first assignment, we have a problem: there is no previous
week, so we have no previous languages, and no previous compiler.

We therefore start from someone else's languages and compiler.
For the source language, we pick @ch1-tech{x64}.
For the target, we choose @ch1-tech{bin64}.
Thankfully, someone has already written a compiler, called @tt{nasm}, from
@ch1-tech{x64} to @ch1-tech{bin64} for most machines.
You can read more about these languages in in @Secref[#:tag-prefixes
'("book:" "chp1:")]{top}.

@section{Testing Your Development Machine}
If you're running your own Linux machine, I trust you can figure out how to
install @code{racket} and @code{nasm}.

If you don't have a Linux machine, you can use @url{remote.students.cs.ubc.ca},
which has the correct version of @code{nasm} and @code{racket}.
You can sign up for an account at @url{https://www.cs.ubc.ca/getacct/}.

If you want a local development environment, you can use the @tt{Dockerfile}
found in your git repository. to setup a development container that mirrors
@url{remote.students.cs.ubc.ca}.
This will install a container with all the appropriate build tools and version.
We recommend editing the compiler assignment files in the host machine,
mounting them in the Docker container, and running tests in the container.
You can create a new image using @tt{docker image build -t cpsc411 .} from your
git repostory with the @tt{Dockerfile}.
Assuming your compilers assignments are in stored in the path
@tt{~/workspace/}, you can launch a new container with access to your
assignments via @tt{docker run -i -t -v ~/workspace:/app/workspace cpsc411}.

Whichever you choose, the following exercises will make sure your machine is
setup and working properly.

First, let's ensure Racket is installed properly and the right version.
You will need @code{racket} version 7.3 or higher.

@exercise{Run @code{racket --version}, and check that a message like "Welcome to
Racket v7.4.0.1" is printed, and that the version is at least "v7.3".
}

Next, we'll test @code{nasm}. We need @code{nasm} version 2.13 or higher.

@exercise{Run @code{nasm --version}, and check that a message like "NASM
version 2.14.02" is printed and that the version is at least "2.13".
}

We also need to be sure @code{racket} can find @racket{nasm}.

@exercise{Run
@code{racket -e "(with-output-to-string (thunk (system \"nasm --version\")))"},
and check that a message like "NASM version 2.14.02" is printed and that the
version is at least "2.13".
}

Now you should be able to compile the following file "fact.s"

@nasm-example[
#:file-name "fact.s"
#:result 120
#:result-displayer exit-code-displayer
#:runner nasm-run/exit-code
#:type 'unix
]{global start

section .text

start:
  mov r8, 5

fact:
  mov r9, 1

fact_acc:
  cmp r8, 0
  je fact_done
  imul r9, r8
  dec r8
  jmp fact_acc

fact_done:
exit:
  mov     rax, 60
  mov     rdi, r9
  syscall
}

@exercise{Compile and execute "fact.s" from the command line. You should observe
@code{120} as the exit code.}

We can compile and execute a file from Racket using @racket[system] or
@racket[system/exit-code] to make calls to command line programs.
This is how the last pass of the compiler will translate your code into an
executable, and how you will test your compiler from Racket.

@exercise{
Run the file to ensure the test passes, using @tt{raco test fact.rkt}.
Make sure to push to @tt{assignment-0} branch of the git repository we created
for you.
}

@section{x64 References}
@itemlist[
@item{x86_64 NASM tutorial: @url{https://www.tutorialspoint.com/assembly_programming/index.htm}}
@item{x86_64 cheat sheet: @url{https://www.cs.uaf.edu/2017/fall/cs301/reference/x86_64.html}}
@item{x86_64 instruction reference: @url{https://www.felixcloutier.com/x86/}}
@item{Linux syscall reference: @url{https://syscalls.kernelgrok.com/}}
@item{MacOS BSD syscall reference: @url{https://sigsegv.pl/osx-bsd-syscalls/}}
@item{Online NASM explorer: @url{https://godbolt.org/z/0mA2AE}}
@item{Windows Linker: @url{http://www.godevtool.com/Golink.zip}}
@item{Windows NASM: @url{https://www.nasm.us/pub/nasm/releasebuilds/2.14.02/win64/nasm-2.14.02-win64.zip}}
@item{x64 on Windows tutorial: @url{https://github.com/jacwil/nasm-on-windows/}}
]
