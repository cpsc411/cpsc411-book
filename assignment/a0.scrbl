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

You should have a git repository created for you at @url{https://github.students.cs.ubc.ca/orgs/@|gh-org|}

@subsubsub*section{Assignment Checklist}
@itemlist[
@item{Ensure you have a CSID: @url{https://www.cs.ubc.ca/getacct/}.}
@item{Either:
  @itemlist[
  @item{Connect to @exec{ssh://remote.students.cs.ubc.ca}, or}
  @item{Setup a 64-bit Linux (virtual) machine for local development}
  ]
}
@item{Ensure @exec{racket} >= 8.15 is installed.}
@item{Ensure @exec{nasm} >= 2.13 is installed.}
@item{Ensure @exec{nasm} is callable from Racket.}
@item{Compile and execute fact.s from the command line.}
@item{Compile and execute fact.s from Racket.}
@item{Test submission process.}
@item{Install and test @exec{cpsc411-lib}.}
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
Thankfully, someone has already written a compiler, called @exec{nasm}, from
@ch1-tech{x64} to @ch1-tech{bin64} for most machines.
You can read more about these languages in @Secref[#:tag-prefixes
'("book:" "chp1:")]{top}.

@section{Setup a CSID}
You almost certainly already have one, but if you don't, make sure you
sign up for an account at @url{https://www.cs.ubc.ca/getacct/}.

This account will be your GradeScope account and your account on
@url{https://github.students.cs.ubc.ca/orgs/@|gh-org|}.

@section{Testing Your Development Machine}
If you're running your own Linux machine, I trust you can figure out how to
install @exec{racket} and @exec{nasm}.

If you don't have a Linux machine, you can use @url{remote.students.cs.ubc.ca}
or a Docker image described below.
The @exec{remote.students} machines have the correct version of @exec{nasm} and
@exec{racket}.
You can connect via SSH using the username setup at
@url{https://www.cs.ubc.ca/getacct/}.
You may need to be connected to the VPN to connect via @exec{ssh}.

The remote machines have Racket 8.15 installed.
To access the v8.15 Racket, add @exec{/home/c/cs-411/racket-v8-15/bin} to your path.
Remember that you must prepend this to the path, in order for this version to
take precedence.
For example, type this in a shell or add it to your profile file such as @exec{.bash_profile}:
@exec{export PATH="/home/c/cs-411/racket-v8-15/bin:$PATH"}.
You will need to add this to a profile file, unless you want to run the command
manually each time you connect to remote.

If you want a local development environment, you can use the @exec{Dockerfile}
found in your git repository to set up a development container that mirrors
@url{remote.students.cs.ubc.ca}.
This will install a container with all the appropriate build tools and version.
We recommend editing the compiler assignment files in the host machine,
mounting them in the Docker container, and running tests in the container.

On a x86-64 machine, you can create a new image using @exec{docker image build
-t cpsc411 .} from your git repository with the @exec{Dockerfile}.
Assuming your compilers assignments are stored in the path
@exec{~/workspace/}, you can launch a new container with access to your
assignments via @exec{docker run -i -t -v ~/workspace:/app/workspace cpsc411}.
You can use docker compose via @exec{docker-compose run cpsc411}, which should
automate all of the above.

On other machines, such as Apple's M series chips or other ARM machines, you
need to explicitly enable virtualization of the platform.
Use the command build command @exec{docker image build -t cpsc411 . --platform=linux/amd64}.
You may also need to enable or change virtual machines options instruction set
in Docker.
Particularly on macOS, we've had reports that the default virtualization causes
an error while installing Racket, which reports @exec{Error: error reading from
~a ("petite")}.
To fix this, change the Docker virtual machine options to use Docker VMM or
QEMU, in @exec{Settings -> General -> Virtual Machine Options} in Docker
Desktop.

Whichever you choose, the following exercises will make sure your machine is
set up and working properly.

First, let's ensure Racket is installed properly and the right version.
You will need @exec{racket} version 8.15 or higher.

@exercise{Run @exec{racket --version}, and check that a message like "Welcome to
Racket v8.15" is printed, and that the version is at least "v8.15".
}

Next, we'll test @exec{nasm}. We need @exec{nasm} version 2.13 or higher.

@exercise{Run @exec{nasm --version}, and check that a message like "NASM
version 2.14.02" is printed and that the version is at least "2.13".
}

We also need to be sure @exec{racket} can find @exec{nasm}.

@exercise{Run
@exec{racket -e "(with-output-to-string (thunk (system \"nasm --version\")))"},
and check that a message like "NASM version 2.14.02" is printed and that the
version is at least "2.13".
}

Now you should be able to create and compile the following file @hyperlink["fact.s"]{@exec{fact.s}}.

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

@exercise{Compile and execute @exec{fact.s} from the command line. You should observe
@code{120} as the exit code.}


Instead of compiling manually using shell commands, we can compile and execute a
file from Racket using @racket[system] or @racket[system/exit-code] to make
calls to command line programs from Racket.
This is how the last pass of the compiler will translate your code into an
executable, and how you will test your compiler from Racket.
The file @exec{fact.rkt} demonstrates how to do this with the above assembly
program.

@exercise{
Modify and run the @exec{fact.rkt} file to ensure the test passes, using @exec{raco
test fact.rkt}.
When you're done, commit the changes to @exec{fact.rkt}, and make sure to push to
@exec{assignment-0} branch of the git repository we created for you.

It is your responsibility to make sure the final commit to that branch is the
right one; use good software development practices and avoid merging to that
branch until you've tested.
Other commits will not be graded.
}

@exercise{
We will be attempting to grade using GradeScope, and milestones may include an
autograder.
You can manually submit by logging in to GradeScope using the email
@exec{CWL@"@"student.ubc.ca}, and uploading a ZIP archive created from your
git repo using @exec{git archive -o submission.zip assignment-0}.
You will be able to see some information from the autograder after submitting.

Regardless of whether you submit or not, we'll automatically submit your final
commit prior to the deadline from the @exec{assignment-0} branch.
You are not required to submit manually, and submitting manually does not affect
which commit we grade.
}

@exercise{
Future assignments rely on the @racketmodname[cpsc411/compiler-lib] package.
To make sure this is installed, run @exec{raco pkg install --user --auto https://github.com/cpsc411/cpsc411-pub.git?path=cpsc411-lib#2025w2}.
To check this is installed correctly, run @exec{raco test -p cpsc411-lib}.
You should see a lot of files run, followed by @exec{25 tests passed}.
}

@section{x64 References}
@itemlist[
@item{x86_64 NASM tutorial: @url{https://www.tutorialspoint.com/assembly_programming/index.htm}}
@item{x86_64 cheat sheet: @url{https://www.cs.uaf.edu/2017/fall/cs301/reference/x86_64.html}}
@item{x86_64 instruction reference: @url{https://www.felixcloutier.com/x86/}}
@item{Linux syscall reference: @url{https://syscalls.kernelgrok.com/}}
@item{macOS BSD syscall reference: @url{https://sigsegv.pl/osx-bsd-syscalls/}}
@item{Online NASM explorer: @url{https://godbolt.org/z/0mA2AE}}
@item{Windows Linker: @url{http://www.godevtool.com/Golink.zip}}
@item{Windows NASM: @url{https://www.nasm.us/pub/nasm/releasebuilds/2.14.02/win64/nasm-2.14.02-win64.zip}}
@item{x64 on Windows tutorial: @url{https://github.com/jacwil/nasm-on-windows/}}
]
