#lang scribble/base

@(require "../assignment/assignment-mlang.rkt")

@title[#:tag "top" #:tag-prefix "chp1:"]{The first language}

@(define eval-eg
   (make-cached-eval
    "x64-eval"
    '(require
      "x64-interp.rkt")))

\todo{Crafting a compiler describes the ISA+OS as a virtual machine in the
intro. This is quite in line with my approach, perhaps better.
Doing it that way could let me start with my first abstraction being to
virtualize or abstract the machine.
If I took that to an extreme, we could virtualize the ISA and target ARM64 (ARM is a RISC) or x64.
That would solve some of the reviewer concerns, might be nice for students as well.
Would I also provide custom assembler? Linker? ... probably not.

Looks like the main difference is load/store are reduced on ARM.
Given that memory instructions are separated in most of the compiler as mset! and mref, and fvars, this shouldn't be a big deal.

This biggest problem with this chapter is I really want to start with language
semantics, but that normal way I do that is with interpreters, but I haven't
introduced abstract syntax yet.
I suppose I could have a short chapter on abstract syntax first, or a prelude.
Even this chapter starts with a lil prelude: "... the study of languages".
Could jump off from there to a refresher about abstract vs concrete syntax, syntax vs semantics.
}


To me, the study of compilers is the study of languages.
I view a @deftech{compiler} as a meaning-preserving transformation between
languages, transforming a source language into a target language.
As a program, a compiler is a function that takes any program in a @ch-bp-tech{source
language} and produces some program that "behaves the same" in a @tech{target
language}.

This definition tells us we need three things before we can start designing our compiler:
We need a @ch-bp-tech{source language}, @tech{target language}, and some definition of
"behaviour" for programs in each.
Each of these is a design choice.

In this course, we pick a starting @tech{target language} (@tech{x64}), and
systematically design @emph{many} @ch-bp-tech{source languages}.
Each of our @ch-bp-tech{source languages} will be designed to remove a restriction from a
@tech{target language}, or add high-level features to improve usability.
We also take care to ensure the @ch-bp-tech{source languages} can be compiled
efficiently.
The behaviour of each language is defined by an interpreter.
For @tech{x64}, the behaviour is defined by whatever happens when we turn it
into binary and run it on a CPU (The First Interpreter).

Understanding the behaviour of programs in each language is a key step.
We will write programs that validate, analyze, and transform programs.
We will write templates for programs, and whole classes of programs.
That is the nature of a compiler.
So we must understand what a program @emph{means}.

We begin by picking and studying our chosen @tech{target language}.

@todo{should introduce interpreters and validators in this chapter.
write one for x64, since we have one for Racket.}

@section{Picking the target language}
The @deftech{target language} of a compiler is the language in which the
compiler writes programs.

Our first language, and ultimate @tech{target language}, is x86-64 plus
@tech{system calls}, which we call @deftech{x64} for short.
In fact, we'll only use a subset @tech{x64} of this language, which will expand
it slightly throughout the book; @tech{x64} will refer to the current subset.
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
@ch-bp-tech{source language}.
Perhaps they want to focus on ruling out more errors statically, or providing
convenient syntax for certain programming patterns they find common.

However, choosing higher-level languages as a target has a cost.
These targets come with their own implementations, their own compilers and
@ch-bp-tech{run-time systems}.
If we want to use them, we commit ourselves to using their toolchains as well.
This may complicate our own development if it is difficult to setup those
toolchains.
Choosing a language also fixes an abstraction boundary---we cannot choose to
work at a level of abstraction lower than our @tech{target language}.
This may complicate or entirely prevent us from implementing certain
functionality or optimizations.

We choose @tech{x64} for two main reasons: one pedgogical, one pramatic.
First, we want to learn to write transformations and optimizations at some of
the lowest abstraction layers.
Choosing a hardware instruction set means we can learn about low-level transformations.
Second, it is easier to setup the toolchain for @tech{x64} than a C, LLVM, or
JavaScript toolchain, at a minor cost to the course staff who must maintain
some @tech{x64} support code.
Choosing @tech{x64} pulls in far fewer dependencies.

@section{The meaning of x64 programs}
Having chosen our @tech{target language}, we must now understand the
language---its behaviour, the abstractions it provides, the restrictions it
requires us to work with, and even its syntax.
As far as we are concerned, the @tech{x64} was not designed---it was
discovered.
It is the language of a wild CPU found by geologists in cave somewhere, probably
in the Amazon.
It is primordial.
It is not sensible to ask questions like "why"---@tech{x64} @emph{is}.
And, because we want to talk to the CPU, we must deal with it.
@margin-note{The reasons x64 ended up this way is interesting, but not in scope for this book.
A computer architecture course would provide some insight, but it is also the
artifact of decades of iteration and design decisions.}

While we have chosen @tech{x64} as our @tech{target language}, @tech{x64}
itself does not run on most hardware.
Instead, it is the @ch-bp-tech{source language} of yet another compiler that
transforms @tech{x64} to a binary machine code that I will name @tech{bin64} for
now.
We will use the @tt{nasm} assembler to compile @tech{x64} into @tech{bin64}.
The compiled programs can run natively on most hardware, but you may need an
interpreter depending on your machine.
@margin-note{Usually, interpreters for machine code are called "virtual machines".}

To get started understanding the meaning of @tech{x64} programs, let's start with a simpler language.
Consider the factorial function @racket[fact].
In Racket, following the design recipe ala @hyperlink["https://htdp.org/"]{@emph{How to Design Programs}}, we would simply write:
%TODO: cite
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

The meaning of this program is easily understood in the usual way for functional languages.
The meaning of each expressions can be assigned a value.
The meaning of 0 is the natural number 0; the meaning of @racket[(zero? n)] is the boolean value true if the meaning of @racket[n] is 0, and the boolean value false otherwise.
We could represent this meaning as an interpreter.
The meaning of any program is simply the value or sequences values of each expression in the program.

This meaning gives us a fairly accurate understanding of what happens when we run the program.
We run this and the computer prints @code{120}, the value of program.

We can implement the equivalent computation in @tech{x64} as follows.
@margin-note{@tech{x64} actually supports at least two syntaxes.
We use Intel assembler syntax, as it maps more closely to familiar imperative
language syntax.}

@nasm-example[
#:result (fake "internal OS-defined error")
#:type 'unix
#:file-name "plain-fact-x64.s"
]{global start                  ; Declare starting block for linker.

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
;; Follows the template for unary natural numbers.
fact_acc:
  cmp r8, 0                     ; compare r8 and 0, i.e., (zero? r8)
  je fact_done                  ; jump if last comparison was equal
  imul r9, r8                   ; (set! r9 (* r9 r8))
  dec r8                        ; (set! r8 (sub1 r8))
  jmp fact_acc                  ; (fact r8)

fact_done:
;; Done. The result is in r9.

section .data                   ; Start of statically allocated "data". Required to be non-empty on MacOS

dummy: db 0
}

We compute the result of factorial of 5, the result of which gets stored in @exec{r9}.
We do this by imperatively transforming the state of registers, until the computation is complete.

This is similar to the Racket program we wrote, but we cannot understand the
meaning of the program by simply assigning each piece of the program a value.
Instead, intuitively, the meaning of this program is a transformation over a register machine.
It expects a machine with at least two registers, @exec{r8} and @exec{r9}, whose
initial values are arbitrary.
It begins executing the first instruction, then executes the next, and so on.
Each instruction transforms the state of the registers.
After running, the machine is left with in a state where @exec{r9} contains the
result.

To formalize this meaning, we could write an interpreter.
Doing so over strings would be slightly tedious, but a basic environment passing interpreter might include a function like
@racketblock[
(define (interp-x64-instr labels regs strs)
  (if (empty? strs)
      regs
      (match (car strs)
        [(regexp #px"\\s*mov (\\w+), (\\w+)" (list _ rand1 rand2))
         (interp-x64-instr 
          labels 
          (dict-set regs (string->symbol rand1) (interp-x64-rand reg labels rand2))
          (cdr strs))]
        ....)))
]
@nested[#:style 'inset]{
@defproc[(interp-x64 [p string?]) dict?]{
Takes a string representing an @tech{x64} program and returns a dictionary mapping registers to values.

@examples[#:eval eval-eg
(interp-x64
"global start
section .text
start:
  mov r9, 120
section .data
dummy: db 0")
]
}
}

@todo{Exercise here: write a little interpreter. Over strings? Then we don't
have to introduce abstract syntax yet. could move on to abstract syntax in the
next chapter.}

Unfortunately, this intuitive meaning isn't complete.
If this were the correct interpretation, and the program stopped with @exec{r9}
containing the result, and we had some way to read the result of @exec{r9} back
to the user, this would be a fairly good compilation of our original Racket program.
Instead, the program crashes rather than producing the result @racket[120].

There are two problems with this intuitive understanding of the meaning of @tech{x64}.
First, there's no signal to stop running the program, so it doesn't stop with
the result in @exec{r9}, and crashes.
Second, even if it did, there is no instruction to communicate that the result
is in @exec{r9}.
Both problems require us to expand our understanding of what @tech{x64} is.

What actually happens when we run this program is that after reaching
@exec{fact_done}, the machine happily tries to execute the next instruction in
memory.
There is no next instruction, so the machine executes whatever happens to be in
the memory location after the final instruction.
The program hopefully crashes with a @tt{SEGFAULT} or @tt{SIGBUS} error, but
this depends on the @tech{operating system} (@tech{OS}).

This fact that the behaviour of the program depends on the @tech{OS} is our
first clue that this example is not implemented in @emph{just} x86-64, the
language of the CPU.
The meaning of the program, and thus the actual language in which we were
programming, depends critically on the @tech{OS}.

The @deftech{operating system} (@deftech{OS}) is the piece of software that the
hardware first loads on boot, and thereafter becomes responsible for managing
the hardware, including running launching new (sub)programs to run on that hardware.
Each @tech{OS} makes different assumptions about and enforce different restrictions
on @tech{x64} programs.
Most @tech{operating systems} require that a program explicitly "exit", by calling
a special, @tech{OS}-specific "exit" procedure when the program is complete.
Each @tech{OS} defines a different set of these procedures, typically called
@tech{system calls}, which we can think of as the standard library for
@tech{x64}.
Each @deftech{system call} is a procedure predefined by the @tech{operating system}
that defines how an @tech{x64} program interacts with the system.
@;tech{OS}, and exactly where the @tech{OS} expects the final answer to be differs.
@;For example, the linker for each @tech{OS} assumes a different entry label for
@;where to start executing (although you can usually override this default).
@;The entry label is assumed to be "start" on macOS (at least, on some versions),
@;"_start" on Linux, and "Start" on Windows (depending on which Windows linker you
@;use).
Some @tech{operating systems} impose additional restrictions, both syntactic
and semantic, on @tech{x64}.
For example, the entry label is assumed to be "start" on macOS (at least, on some versions),
"_start" on Linux, and "Start" on Windows (depending on which Windows linker you
use), although this can often be redefined by the linker.
macOS requires that the @exec{.data} section, which is used to declare
initialized memory locations, cannot be empty, and the linker statically
rejects the program if it is.
In the above example, we don't actually use the heap, so we declare a dummy
value to ensure compatibility with macOS.
This example is compatible with macOS, Linux, and Windows, as long as you
specify "start" as the entry label, and don't mind exiting with a bang.

Because of this critical dependence on the @tech{OS}, our @tech{target language}
cannot @emph{really} be "plain" @tech{x64} that will be interpreted by the
hardware---we can think of @tech{x64} as a family of programming languages,
indexed by an operating system.
We never program the raw CPU---we program the operating system.
The CPU together with the operating system implements a different programming
language than the CPU by itself.
From a programming languages perspective, we can view the operating system (@eg
Linux) as the @ch-bp-tech{run-time system} for the OS-flavoured @tech{x64}
programming language (@eg @tech{x64-linux}).

Even if we had signaled to the @tech{operating system} to stop the program, we
need to explicitly produce a result value in some way.
In a functional language, expressions implicitly have values, so the language
can automatically provide a result: the values of each expression the user
provided.
But no one statement in @tech{x64} necessarily indicates a result.
Any given register might contain a result, or might contain an intermediate value.
We must explicitly produce a result.

Further, how we produce the result matters.
In Racket, like many high-level languages, any result is returned to the
caller.
The top-level call, such as the REPL, prints whatever value is returned to it.
In @tech{x64}, the result of the computation must be explicitly passed to the
@tech{operating system}, and we must explicitly choose how the result is
returned: is it printed, is it an error code, is it written to a file?
To do this, we need to use a @tech{system call}.

After compiling our @tech{x64} program with @tt{nasm}, we are left with a program
in the first target language---a machine code binary, which is just about the
raw language of the CPU.
In fact, it is machine code in a binary format described by the @tech{OS}, which
contains @tech{x64} instructions arranged according to the @tech{OS}
specification. 
I call this target language @deftech{bin64}, and like @tech{x64}, it is a
family of languages.
As we rely on @tt{nasm} to generate @tech{bin64}, we do not study this target
language in any detail, and normally forget about its existence except when
directing @tt{nasm} to generate the right language.

This is not the end of the process either: the program is not complete yet.
We need to to install additional code for the operating system to load the
binary properly.
This is done by the linker, @exec{ld}, which links together different pieces of
machine code into a single binary executable by the @tech{OS}.

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

To compile this on Linux, we run @exec{nasm -f elf64 exit-fact-x64-linux.s -o
exit-fact-x64-linux.o}.
This will generate a binary in the @tech{elf64} target language.
The @deftech{elf64} is the Linux-flavoured @tech{bin64}, a binary format used on
Linux.
It includes the @tech{x64} instructions and additional structure that allows the
Linux operating system to control the binary.

Next, we link the file by running @exec{ld -e start -o exit-fact-x64-linux.exe
exit-fact-x64-linux.o}, which essentially connects the binary to the operating
system and any other external libraries, and specifies the entry point where
execution should begin (the @tt{start} label).
Now we can execute the file @exec{exit-fact-x64-linux.exe} on the command line
in the usual way, @eg set it to executable using @exec{chmod +x exit-fact-x64-linux.exe}
then execute with @exec{./exit-fact-x64-linux.exe}.
We can get the result of the exit code using @exec{echo $?} or @exec{echo
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
prefix @exec{#x2000000}.
The @tt{exit} @tech{system call} is system call 1 in the BSD table, so we use
the @tech{system call} number @exec{#x2000001}.
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

To compile this file, we run @exec{nasm -f macho64 exit-fact-x64-macos.s exit-fact-x64-macos.o}.
@tt{macho64} is the binary formatted used by 64-bit macOS.
To link, we run @exec{ld -macosx_version_min 10.6 -e start -o
exit-fact-x64-macos.exe exit-fact-x64-macos.o}.
We need to specify a minimum macOS version number of 10.6, otherwise the linker
will ignore the custom entry label and fail to link.
We can execute the file @tt{exit-fact-x64-macos.exe} on the command line
in the usual way, and can get the result of the exit code using @exec{echo $?}
or @exec{echo $status}, depending on your shell.

macOS also has a similar @tt{write} @tech{system call}: number 4 in the BSD
table.
The file @tt{fact-x64-linux.s} is ported to macOS below.

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
The @exec{ExitProcess} kernel function provides the same functionality as the
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
After downloading @exec{nasm} and @exec{GoLink}, you can compile using
@exec{nasm -f win64 exit-fact-x64-windows.s -o
exit-fact-x64-windows.o} and link using @exec{golink /entry Start /fo
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
us, but it is enough that we can start designing a @ch-bp-tech{source language} that
abstracts away some of the annoying aspects of the language, and a
@tech{compiler} to write programs in @tech{x64} for us.
