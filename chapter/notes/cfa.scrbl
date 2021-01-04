#lang scribble/base

@(require "../assignment/lib.rkt")

@subsection{Control-flow Analysis, and use/def chains}
To compute the undead sets, we first need to perform control-flow analysis to
build a control-flow graph.
A control-flow graph is a directed graph in which each instruction represents a
node (vertex), and an edge from vertex @tt{v1} to @tt{v2} means that @tt{v1} is
executed before @tt{v2}.
In general, a vertex may have multiple predecessors due to jumps, and a multiple
successors due to branching.

We generate the control-flow graph essentially by labeling each instruction
with a uniquie identifier (such as a number, a symbol, or a pointer to the
instruction's representation), traversing the program, and adding an edge from
instruction's ID to the ID of its successor.

While building the control-flow graph, we also collect all the variables defined
at that node in the graph, and all the variables used at that node.
A variable is defined if it gets a new value assigned to it, such as the
variable @object-code{x.1} in the instruction @object-code{(set! x.1 (+ y.6
z.7))}.
A variable is used if it is referenced in the instruction, such as
@object-code{y.6} and @object-code{z.7} in the above instruction.
We associate these sets with the instruction's ID.

For example, consider the following program.
@racketblock[
`(module
   (define main
     (begin
       (set! x.1 5)
       (set! y.2 x.1)
       (begin
         (set! b.3 (+ x.1 y.2))
         (set! c.4 b.3)
         (if (eq? c.4 b.3)
             (halt c.4)
             (begin
               (set! x.1 c.3)
               (jump f x.1 y.2))))))
   (define f (begin (halt x.1))))
]

We can represent its control-flow graph as:
@racketblock[
`(module
   (define main
     (begin
       (1 (successors (2))
          (defs (x.1))
          (uses ())
          (set! x.1 5))
       (2 (successors (3))
          (defs (y.2))
          (uses (x.1))
          (set! y.2 x.1))
       (begin
         (3 (successors (4))
            (defs (b.3))
            (uses (x.1 y.2))
            (set! b.3 (+ x.1 y.2)))
         (4 (successors (5))
            (defs (c.4))
            (uses (b.3))
            (set! c.4 b.3))
         (5 (successors (6 7))
            (defs ())
            (uses (c.4 b.3))
            (if (eq? c.4 b.3)
                (6 (successors ())
                   (uses (c.4))
                   (defs ())
                   (halt c.4))
                (begin
                  (7 (successors (8))
                     (defs (x.1))
                     (uses (c.3))
                     (set! x.1 c.3))
                  (8 (succesors (9))
                     (defs ())
                     (uses (x.1 y.2))
                     (jump f x.1 y.2))))))))
     (define f
       (begin
         (9 (successors ())
            (defs ())
            (uses (x.1))
            (halt x.1)))))
]

Each instruction is replaced by a list whose first element is its ID, second
element contain the set of successors, third element contains the set of defs,
fourth elements containing the set of uses, and the fifth element containing the
original instruction.
This representation is quite inefficient, but useful for pedagogical purposes.

Actually constructing the control-flow graph is difficult.
We need unique identifiers for every label before we analyze them to handle
cycles in the graph.

Like with @racket[undead-analysis], it is faster (and simpler) to compute the
graph by traversing the program backwards, passing the successors up through a
recursive call and then adding them to the graph.

We provide you the function @racket[block-locals-lang->cfg], which constructs a
somewhat more efficient graph representation.
@todo{Rename that @racket[control-flow-analysis] next year.}

After you have a control flow graph, we compute the
@tech{Undead-set-tree}s by iterating over the graph, updating the
@a3-tech{undead-in sets} and @a3-tech{undead-out sets} until a fixed point is reached.
We update the sets according to the following algorithm.
@itemlist[
@item{
Suppose @racket[ins] is a map from instruction IDs to @a3-tech{undead-in sets}, and
@racket[outs] is map from instruction IDs to @a3-tech{undead-out sets}.
Initially, every @racket[id] in the control-flow graph maps to the empty set.
}
@item{Compute new maps @racket[ins_new] and @racket[outs_new] for each
@racket[id]:
@itemlist[
@item{Compute the new @a3-tech{undead-in set} as
the use set for @racket[id] union with the current @a3-tech{undead-out set} for
@racket[id], removing the defs set for @racket[id].
That is, @racket[(set-union uses (set-subtract (dict-ref outs id) defs))].
}
@item{
Compute the new @a3-tech{undead-out set} as the union of the
@a3-tech{undead-in sets} of all the successors.
}
]
}
@item{Repeat the loop, replacing @racket[ins] with @racket[ins_new] and
@racket[outs] with @racket[outs_new] until @racket[ins] is equal to
@racket[ins_new] and @racket[outs] is equal to @racket[outs_new].
}
]

Once the algorithm terminates, the @racket[outs] is a map from instruction IDs
to @a3-tech{undead-out sets}.
We can easily construct an @tech{Undead-set-tree} for each block by traversing
the control-flow graph, replacing each node by its @a3-tech{undead-out set}.

We provide the function @racket[undead-analysis], and decorates each block with
the @a3-tech{undead-in sets} and @a3-tech{undead-out sets}.

Now, @a3-tech{undead-out sets} will contain variables that may not exist in the
block.
The sets should still be associated with blocks, however, since each
@a3-tech{undead-out set} corresponds to an instruction in a particular block.

