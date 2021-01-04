Now that we're finished the register allocation, we can discard the annotations
on jumps.
Below, we design @deftech{Block-jump-live-lang}.
The only change is that we remove the @object-code{aloc ...} list after a jump
target.
@racketgrammar*[
[p     (module info b ... b)]
[b     (define label info tail)]
[info  ((assignment ((aloc rloc) ...)) any ...)]
[tail  (begin s ... tail)
(jump trg (unsyntax @bnf:sub{aloc ...}))
(if (cmp aloc opand) tail tail)
(halt opand)]
[s     (set! aloc triv)
(set! aloc (binop aloc opand))]
[binop * +]
[triv  opand label]
[opand int64 aloc]
[trg   label aloc]
[rloc  reg addr]
[reg   rsp rbp rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15]
[addr  (rbp - dispoffset) (rbp + dispoffset)]
[cmp   neq? eq? < <= > >=]
]

