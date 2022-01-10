#lang scribble/base
@(require "../assignment/assignment-mlang.rkt")

@title[#:tag "top" #:tag-prefix "chp1:"]{A Compiler Begins with a Language}

To me, the study of compilers is the study of languages.
As a mathematical object, a @deftech{compiler} is a transformation between
languages, transforming a source language into a target language---but that is
for another time.
As a program, a compiler is a function that takes any program in a @tech{source
language} and produces some program that "behaves the same" in a @tech{target
language}.

This definition tells us we need three things before we can start designing our compiler:
We need a @tech{source language}, @tech{target language}, and some definition of
"behaviour" for programs in each.
Each of these is a design choice.
Often, we choose a pre-existing @tech{target language}.
Picking a target language is an interesting point in the design of any compiler.
By contrast, the source language is often designed to achieve a balance between
aesthetic and pragmatic goals.

In this course, we will pick a starting @tech{target language} (@tech{x64}), and
systematically design @emph{many} @tech{source languages}.
Each of our @tech{source languages} will be designed to remove a restriction from a
@tech{target language}, or add high-level features to improve usability.
We also take care to ensure the @tech{source languages} can be compiled
efficiently.
For each of these languages, their behaviour is defiend by an interpreter.
For @tech{x64}, the behaviour is defined by whatever happens when we turn it
into binary and run it on a CPU (The First Interpreter).

This last step, understanding the behaviour of programs in the language, is a
key step.
We will write programs that validate, analyze, and transform programs.
We will write templates for programs, whole classes of programs.
That is the nature of a compiler.
So we must understand what a program @emph{means}.

So we begin by picking and studying our choosen @tech{target language}.

@section{Picking the target language}
The @deftech{target language} of a compiler is the language in which the
compiler writes programs.

Our first language, and choosen @tech{target language}, is x86-64 plus
@tech{system calls}, which we call @deftech{x64} for short.
In fact, we'll only use a subset @tech{x64} of this language, which will expand
it slightly throughout the course; @tech{x64} will refer to the current subset.
I elaborate on this language and what @tech{system calls} are shortly.

This is an unusual choice for a @tech{target language} in a contemporary compiler.
Contemporary compilers often choose higher-level languages, such as C, LLVM, or
even JavaScript as @tech{target language}.

There are good reasons to a choose higher-level language as a target.
Often, a higher-level language provides more convenient abstractions, making
generating programs simpler.
The language may already have an efficient implementation, making writing
optimizations less important.
This frees the compiler writer to focus on other design goals of the new
@tech{source language}.
Perhaps they want to focus on ruling out more errors statically, or providing
convenient syntax for certain programming patterns they find common.
These are common goals in @tech{source language} and domain-specific language
design.

However, choosing such languages as a target has a cost.
These targets come with their own implementations, their own compilers and
@tech{run-time systems}.
If we want to use them, we commit ourselves to using their toolchains as well.
This may complicate our own development if it is difficult to setup those
toolchains.
Choosing a language also fixes an abstraction boundary---we cannot choose to
work at a level of abstraction lower than our @tech{target language}.
This may complicate or entirely prevent us from implementing certain
functionality or optimizations.

For this course, we choose @tech{x64} for two main reasons.
First, for educational goals, we want to write transformations and optimizations
at some of the lowest abstraction layers we can.
Second, for pragmatic goals, it is significantly easier on the students (many)
to setup the toolchain for @tech{x64} than a C, LLVM, or JavaScript toolchain,
at a minor cost to the course staff (few) who must maintain some @tech{x64}
support code.
The second reason is a common rationale in compiler design---simplifying
something for the many users at the expense of the few compiler writers is
usually a good trade-off.

@section{Programming in x64}
Having chosen our @tech{target language}, we must now understand the
language---its behaviour, the abstractions it provides, the restrictions it
requires us to work with, and even its syntax.
As far as we are concerned, the @tech{target language} was not designed---it was
discovered@margin-note{Take a computer architecture course if you want to learn
how it was designed.}
It is the language of a wild CPU found by geologists in cave somewhere, probably
in the Amazon.
It is primordial.
It is not sensible to ask questions like "why"---@tech{x64} @emph{is}.
And, because we want to talk to the CPU, we must deal with it.

While we have choosen @tech{x64} as our @tech{target language}, @tech{x64}
itself does not run on most hardware.
Instead, it is the @tech{source language} yet another compiler, which transforms
@tech{x64} to a binary machine code that I call @tech{bin64} for now.
In this course, we use the @tt{nasm} assembler to compile @tech{x64} into
@tech{bin64}.
The compiled programs can run natively on most hardware, but you may need an
interpreter depending on your machine.
@margin-note{Usually, interpreters for machine code are called "virtual machines".}

To get started with @tech{x64}, let's consider the factorial function @racket[fact].
In Racket, following the design recipe, we would simply write:
@examples[
(code:comment "Natural -> Natural")
(code:comment "Takes a natural number n and returns the factorial of n.")
(code:comment "(fact 0) -> 1")
(code:comment "(fact 1) -> 1")
(code:comment "(fact 5) -> 120")
(code:comment "Follows the template for self-referential natural numbers.")
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (sub1 n)))))
(eval:alts
(module+ test
  (require rackunit)
  (check-equal? (fact 0) 1)
  (check-equal? (fact 1) 1)
  (check-equal? (fact 5) 120))
(void))
(fact 5)
]
We run this and the computer prints @code{120}.

We can implement the equivalent computation in @tech{x64} as follows.
@margin-note{@tech{x64} actually supports at least two syntaxes.
We use Intel assembler syntax, as it maps more closely to familiar imperative
language syntax.}

@nasm-example[
#:result (fake "internal OS-defined error")
#:type 'unix
#:file-name "plain-fact-x64.s"
]{global start   ; Declare starting block for linker.

section .text                   ; Start of the "code"

;; Compute (fact 5)
start:
  mov r8, 5

;; Compute the factorial of the value in r8.
;; Expects  r8 : PositiveInt64
;; Produces r9 : PositiveInt64, the result of the factorial of r8.
;; Constraint: factorial of r8 must be less than 2^63-1.
fact:
  mov r9, 1                     ; Initialize accumulator to (fact 0).

;; Computes the factorial of r8 with accumulator r9
;; r9 is PositiveInt64; contains the result so far of computing the factorial of the input.
;;
;; Follows the template for self-referential natural numbers.
fact_acc:
  cmp r8, 0                     ; compare r8 and 0, i.e., (zero? r8)
  je fact_done                  ; jump if last comparison was equal
  imul r9, r8                   ; (set! r9 (* r9 r8))
  dec r8                        ; (set! r8 (sub1 r8))
  jmp fact_acc                  ; (fact r8)

fact_done:
;; Done. The result is in r9.

section .data ; The data section. This is where we declare initialized memory locations.

dummy: db 0
}

In this example, we compute the result, which is stored in @code{r9}.
Intuitively, the meaning of this program is a transformation over a register machine.
It expects a machine with at least two registers, @code{r8} and @code{r9}, whose
initial values are arbitrary.
It begins executing the first instruction, then executes the next, and so on.
After running the machine is left with in a state where @code{r9} contains the
result.
Unfortunately, that's not what happens if we try to run it.

Instead, after reaching @code{fact_done}, the machine happily tries to
execute the next instruction in memory.
There is no next instruction, so it executes whatever happens to be in that
memory location.
The program hopefully crashes with a @tt{SEGFAULT} or @tt{SIGBUS} error, but
this depends on the operating system (@deftech{OS}).

This fact that the behaviour depends on the OS is our first clue that this
example is not implemented in @emph{just} x86-64, the language of the CPU.
The meaning of the program, and thus the actual language in which we were
programming, depends critically on the @tech{OS}.

Each OS makes different assumptions about and enforce different restrictions
on @tech{x64} programs.
Most operating systems require that you explicitly "exit" the process, by calling
a special, @tech{OS}-specific "exit" procedure at the end of the process.
Each @tech{OS} defines a different set of these procedures, which we can think
of as the "standard library" for @tech{x64}.
@tech{OS}, and exactly where the @tech{OS} expects the final answer to be differs.
@;For example, the linker for each @tech{OS} assumes a different entry label for
@;where to start executing (although you can usually override this default).
@;The entry label is assumed to be "start" on macOS (at least, on some versions),
@;"_start" on Linux, and "Start" on Windows (depending on which Windows linker you
@;use).
macOS imposes a restrictions: the @code{.data} section, which is used to declare
initialized memory locations, cannot be empty, and the linker rejects the
program if it is.
In the above example, we don't actually use the heap, so we declare a dummy
value to ensure compatibility with macOS.
This example is compatible with macOS, Linux, and Windows, as long as you
specify "start" as the entry label, and don't mind exiting with a bang.

Because of this critical dependence on the @tech{OS}, the @tech{target language}
in this course cannot @emph{really} be "plain" @tech{x64}---@tech{x64} is a
family of programming languages, indexed by an operating system.
In this class, we never program the raw CPU---we program the operating system.
The CPU together with the operating system implements a different programming
language than the CPU by itself.
From a programming languages perspective, the operating system (@eg Linux) is
the @tech{run-time system} for the OS-flavoured @tech{x64} programming language
(@eg @tech{x64-linux}).

In @tech{x64}, the result of the computation is not implicitly returned to the
user via the REPL like it is in Racket.
Instead, we have to explicitly say what the result is, and how to communicate it
to the user.
To do this, we need to use a @deftech{system call}, a procedure predefined by
the operating system that defines how an @tech{x64} program interacts with the
system.

@todo{Never introduce the linker}

After compiling our @tech{x64} program with @tt{nasm}, we are left with program
in the first target language---a machine code binary, which is just about the
raw language of the CPU.
In fact, it is machine code in a binary format described by the @tech{OS}, which
contains @tech{x64} instructions arranged according to the @tech{OS}
specification, with additional boilerplate (often installed by the linker) to
ensure correct interoperation with the OS.
I call this target language @deftech{bin64}, and like @tech{x64}, it is a
family of languages.
As we rely on @tt{nasm} to generate @tech{bin64}, we do not study this target
language in any detail, and normally forget about its existence except when
directing @tt{nasm} to generate the right language.

@margin-note{Take an operating systems course if you want to know more about how
to program the "raw" CPU to implement the "lowest" level of programming
language.
}
@margin-note{
Of course, a chip designer would really be implementing the "lowest"
level language by implementing the interpreter in raw silicon.
}
@margin-note{
Or, well, I guess the electrical engineer, who builds the transistors that the
chip designer uses, would be implementing the "lowest" language, which
implements an interpreter for binary gates in the language of raw, analogue
electrical signals.
}
@margin-note{
But what about the chemist who ... It's languages all the way
down!
}

@section{System Calls, and Flavours of x64}
Now that we know @tech{x64} is actually a family of languages, let us study the
different flavours of @tech{x64}.

In this course, the main difference between the flavours of @tech{x64} are in
the @tech{system calls}.
@tech{System calls} are @tech{x64} primitives provided by the @tech{OS}.
Once we start using @tech{system calls}, code becomes OS-specific.
One of the first things a compiler writer will do is abstract away from
@tech{system calls}.
But to abstract away from them, we need to understand how they work in various
flavours of @tech{x64}.

Our earlier example @tech{x64} program was limited because we did not know how
to communicate with the @tech{OS}.
In the rest of this section, we walk through how to use basic @tech{system
calls} to make a complete program that can exit properly and communicate its
result.
We will introduce further @tech{system calls} throughout this course.

@subsection{x64-linux}
In this course, @deftech{x64-linux} refers to the Linux-flavoured @tech{x64}.
This is @tech{x64}, using Linux @tech{system calls}, compiled to @tech{elf64}.
This version of @tech{x64} has few quirky restrictions and will be our
"default", the point of comparison for other flavours of @tech{x64}.

There are multiple @tech{system calls} we could use to return a value to the
system in Linux.
For example, we could modify the program to return a value as an @tech{exit
code} through the @tt{exit} @tech{system call}.
In Linux, each process must call the @tt{exit} @tech{system call} to terminate
properly.
@tt{exit} takes an @deftech{exit code}, a number between 0 and 255, which is
passed to the parent process after the child process exits.
Usually, the @tech{exit code} indicates whether the process terminated normally
(0) or not (non-zero).
But we could just as well use it to return the value of factorial.

@nasm-example[
#:file-name "exit-fact-x64-linux.s"
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
  mov     rax, 60         ; I'm about to call the OS sys_exit function
  mov     rdi, r9         ; The exit code is the value from r9.
  syscall                 ; Perform the system call operation
}

Now, when @tt{fact} completes, we call the @tt{exit} @tech{system call}.
This requires we set the register @tt{rax} to the value @tt{60} (in decimal),
which corresponds to the exit @tech{system call} in Linux.
This @tech{system call} expects one argument, the exit code, passed in register
@tt{rdi}.

In general, the Linux @tech{system call} interface uses the following calling
convention.
The @tech{system call} number is set in register @tt{rax}.
The arguments are set according to the System V AMD64 ABI.
The first six integer or pointer arguments are passed in registers (in order):
@tt{rdi}, @tt{rsi}, @tt{rdx}, @tt{rcx}, @tt{r8}, and @tt{r9}.
Remaining arguments are passed on the stack, although we will not use any
@tech{system calls} with more than six arguments.
We also won't be dealing with floating point arguments, which are passed in
separate registers.
For more details, Wikipedia is not a bad place to start:
@url{https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI}.
But you could also go to the source: @url{https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf}.

To compile this on Linux, we run @tt{nasm -f elf64 exit-fact-x64-linux.s -o
exit-fact-x64-linux.o}.
This will generate a binary in the @tech{elf64} target language.
The @deftech{elf64} is the Linux-flavoured @tech{bin64}, a binary format used on
Linux.
It includes the @tech{x64} instructions and additional structure that allows the
Linux operating system to control the binary.

Next, we link the file by running @tt{ld -e start -o exit-fact-x64-linux.exe
exit-fact-x64-linux.o}, which essentially connects the binary to the operating
system and any other external libraries, and specifies the entry point where
execution should begin (the @tt{start} label).
Now we can execute the file @tt{exit-fact-x64-linux.exe} on the command line
in the usual way, @eg set it to executable using @tt{chmod +x exit-fact-x64-linux.exe}
then execute with @tt{./exit-fact-x64-linux.exe}.
We can get the result of the exit code using @code{echo $?} or @code{echo
$status}, depending on your shell.

Most programming languages communicate their result by printing.
Thankfully, the OS does provide a print @tech{system call}.
We could write the program below to print the answer instead of using the exit
status, to get a somewhat more friendly user interface.

@nasm-example[
#:result "x"
#:type 'unix
#:file-name "fact-x64-linux.s"
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
  mov r8, msg  ; Move the address of `msg` into `r8`.
  mov [r8], r9 ; Move r9 into the 0th index of the address pointed to by r8.

print_msg:
  mov     rax, 1      ; Say, "I'm about to call the OS sys_write function"
  mov     rdi, 1      ; And I want it to write to the standard output port
  mov     rsi, msg    ; The message to print is stored in msg
  mov     rdx, len    ; Length is len.
  syscall

exit:
  mov     rax, 60
  mov     rdi, 0                ; The exit code is 0; normal.
  syscall

section .data

len:   equ   1         ; The constant 1, representing the length of the message.
msg:   times len dw 0  ; A `len` length, in bytes, buffer, initialized to be full of 0.
}

To use the @tt{write} @tech{system call}, we need to create a memory buffer to
store the message we wish to print.
We can statically allocate a buffer in the data section by creating a label,
@tt{msg:}, and using the @tt{times} keyword to allocate @tt{len} bytes
(indicated by @tt{dw}) of space, and initializing each to @tt{0}.
Then we call the @tt{write} @tech{system call}, number @tt{1}, specifying the
standard output port as the file to print to by setting @tt{rdi} to @tt{1},
specifying @tt{msg} as the address to print from in @tt{rsi}, and the length
of the buffer in @tt{rdx}.
When we run the program, it prints the expected result---@tt{x}.

@question{Why is @tt{x} the expected result instead of @tt{120}?}

You can find complete lists of the Linux @tech{system calls} online:
@itemlist[
@item{@url{https://syscalls64.paolostivanin.com/}}
@item{@url{https://filippo.io/linux-syscall-table/}}
]

@subsection{x64-macos}
macOS is very similar to Linux, and we can easily adapt the above examples to
macOS.
On macOS, there are 4 @tech{system call} tables, computed by adding the
@tech{system call} number to a specific prefix.
The BSD @tech{system call} table, the most generic and Linux-like, uses the hex
prefix @code{#x2000000}.
The @tt{exit} @tech{system call} is system call 1 in the BSD table, so we use
the @tech{system call} number @code{#x2000001}.
The example "exit-fact-x64-linux.s" from above is rewritten for macOS below.

@nasm-example[
#:file-name "exit-fact-x64-macos.s"
#:result 120
#:result-displayer exit-code-displayer
#:runner nasm-run/exit-code
#:type 'macosx
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
  mov     rax, 0x2000001       ; I'm about to call the OS sys_exit function.
                               ; Specifying the value in hex for readability,
                               ; but could convert it to decimal for a simpler
                               ; assembler.
  mov     rdi, r9              ; The exit code is the value from r9.
  syscall

section .data

dummy: db 0
}

To compile this file, we run @code{nasm -f macho64 exit-fact-x64-macos.s exit-fact-x64-macos.o}.
@code{macho64} is the binary formatted used by 64-bit macOS.
To link, we run @code{ld -macosx_version_min 10.6 -e start -o
exit-fact-x64-macos.exe exit-fact-x64-macos.o}.
We need to specify a minimum macOS version number of 10.6, otherwise the linker
will ignore the custom entry label and fail to link.
We can execute the file @code{exit-fact-x64-macos.exe} on the command line
in the usual way, and can get the result of the exit code using @code{echo $?}
or @code{echo $status}, depending on your shell.

macOS also has a similar @tt{write} @tech{system call}: number 4 in the BSD
table.
The file @code{fact-x64-linux.s} is ported to macOS below.

@nasm-example[
#:result "x"
#:type 'macosx
#:file-name "fact-x64-macos.s"
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
  mov r8, msg  ; Move the address of `msg` into `r8`.
  mov [r8], r9 ; Move r9 into the 0th index of the address pointed to by r8.

print_msg:
  mov     rax, 0x2000004    ; Say, "I'm about to call the OS sys_write function"
  mov     rdi, 1            ; And I want it to write to the standard output port
  mov     rsi, msg          ; The message to print is stored in msg
  mov     rdx, len          ; Length is len.
  syscall

exit:
  mov     rax, 0x2000001
  mov     rdi, 0                ; The exit code is 0; normal.
  syscall

section .data

len:   equ   1         ; The constant 1, representing the length of the message.
msg:   times len dw 0  ; A `len` length, in bytes, buffer, initialized to be full of 0.
}

@subsection{x64-windows}
Windows does not allow user processes to perform @tech{system calls}.
Instead, we have to call kernel functions that provide similar functionality.
The @code{ExitProcess} kernel function provides the same functionality as the
@tt{exit} @tech{system call} on Linux and macOS.
However, making a function call is more complex than making a @tech{system call}.
We have to declare external functions for the linker, and ensure we link with
the library that includes that function.

@nasm-example[
#:file-name "exit-fact-x64-windows.s"
#:result 120
#:result-displayer exit-code-displayer
#:runner nasm-run/exit-code
#:type 'windows
]{
global Start   ; GoLink expects "Start"
  extern ExitProcess ; Declare that ExitProcess is an external symbol to be resolved by the linker.

section .text

Start:
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
  mov rcx, r8                   ; Windows 64-bit calling convention uses rcx as first argument
  call ExitProcess              ; Windows doesn't expose system calls to user processes; call ExitProcess function to exit.
}

Windows also doesn't ship with a linker.
@hyperlink["http://www.godevtool.com/Golink.zip"]{GoLink} is a small, freely
available linker.
After downloading @code{nasm} and @code{GoLink}, you can compile using
@code{nasm -f win64 exit-fact-x64-windows.s -o
exit-fact-x64-windows.o} and link using @code{golink /entry Start /fo
exit-fact-x64-windows.exe exit-fact-x64-windows.o kernel32.dll}.

@;@section{Setuping a virtual machine}
@;We're only going to support one flavor of x64 (and thus, one operating system)
@;in this course.


@;If you're running a Windows or macOS machine, this section will help you setup a
@;simple virtual machine with @code{nasm} and @code{racket}.
@;You can alternatively connect to any of the UBC ssh boxes, which have
@;the right version of @code{nasm} and @code{racket} installed.

@section{Conclusion}
You should now have an understanding of the first @tech{target language} we will
use, @tech{x64}.
We looked at its syntax, its behaviour, and the existing toolchain to compile
and execute it (an operating system, @tt{nasm} and a linker such as @tt{ld}, and
an x86-64 CPU).
We by no means saw a complete description of everything that language offers
us, but it is enough that we can start designing a @tech{source language} that
abstracts aways some of the annoying aspects of the language, and a
@tech{compiler} to write programs in @tech{x64} for us.
