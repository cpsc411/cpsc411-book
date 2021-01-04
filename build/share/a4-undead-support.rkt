#lang racket
(require
 "a4-graph-lib.rkt"
 "a4-compiler-lib.rkt")

(provide
 block-locals-lang->cfg
 (struct-out instruction-node)

 undead-analysis
 undead-analysis/option

 localize-assignment
 merge-conflicts
 merge-locals

 add-edges^)

(module+ test
  (require rackunit))

;; ------------------------------------------------------------------------
;; Helpers that you may have defined already.

(define (label? s)
  (and (symbol? s)
       (regexp-match-exact? #rx"L\\..+\\.[0-9]+" (symbol->string s))))

(define (aloc? s)
  (and (symbol? s)
       (not (register? s))
       (not (label? s))
       (regexp-match-exact? #rx".+\\.[0-9]+" (symbol->string s))))

(define (triv/BlockAssignedLang? triv)
  (or (int64? triv)
      (label? triv)
      (aloc? triv)))

;; Undead-set is (listof aloc)
;; interp. a set of undead alocs at a particular instruction

;; Any -> Boolean
;; produce true if the given object is an Undead-set, false otherwise.
(define (undead-set? x)
  (and (list? x)
       (andmap aloc? x)
       (= (set-count (list->set x)) (length x))))

(module+ test
  (check-true (undead-set? '(a.1 b.2 c.3)))
  (check-false (undead-set? '(L.abel.1 a.2)))
  (check-false (undead-set? '(a.1 b.2 a.1))))

;; Undead-set-tree is one of:
;; - Undead-set
;; - (list Undead-set Undead-set-tree Undead-set-tree)
;; - (listof Undead-set)
;; WARNING: datatype is non-canonical since Undead-set-tree can be an
;;          Undead-set, so second and third case can overlap.
;;          An Undead-set-tree is meant to be traversed simultaneously with an
;;          Undead-block-lang/tail, so this ambiguity is not a problem.
;; interp. a tree of Undead-sets.  The structure of the tree mirrors the
;;   structure of a Block-locals-lang tail. There are three kinds of sub-trees:
;; 1) an instruction node is simply an undead sets;
;; 2) an if node has an undead-set for the condition and two branch sub-trees.
;; 3) a begin node is a list of undead sets, culminating in a sub-tree;
;;
;; E.g. the following tree corresponds to the subsequent pseudo-tail,
;; where cmp's and s's are replaced with corresponding undead alloc annotations.
(module+ test
  (define UST1
    `((x.1 y.2)
      (z.2 a.4)
      ((b.5 c.6)
       (c.6 d.7)
       ((e.8 f.9 g.10)
        (f.9)
        ((f.9)
         (g.10)))))))

;; (begin
;;    (undead x y)
;;    (undead z a)
;;    (begin
;;       (undead b c)
;;       (undead c d)
;;       (if (undead e f g)
;;           (undead f)
;;           (begin
;;              (undead f)
;;              (undead g)))))

;; Any -> Boolean
;; produce true if the given object is an Undead-set-tree, false otherwise.
(define (undead-set-tree? ust)
  (match ust
    [(? undead-set?) #t]
    [(list (? undead-set?) (? undead-set-tree?) (? undead-set-tree?)) #t]
    [`(,(? undead-set?) ...) #t]
    [else #f]))

(module+ test
  (check-true (undead-set-tree? UST1))
  (let ([ust1 '((e.8 f.9 g.10)
                (f.9)
                ((f.9)
                 (g.10)))]
        [ust2 '((f.9)
                (g.10))])
    (check-false (undead-set-tree? (list ust1 ust1)))
    (check-false (undead-set-tree? (list ust1 ust1 ust1)))
    (check-false (undead-set-tree? (list ust2 ust2)))
    (check-false (undead-set-tree? (list ust2 ust2 ust2)))))

;; Template for Undead-set-tree
#;(define (fn-for-undead-set-tree ust)
  (local [(define (fn-for-undead-set us)
            (... us))
          (define (fn-for-undead-set-list us*)
            (cond
              [(empty? us*) (...)]
              [else (... (first us*)
                         (fn-for-undead-set-list (rest us*)))]))
          (define (fn-for-undead-set-tree ust)
            (match ust
              [us
               #:when (undead-set? us)
               (... (fn-for-undead-set us))]
              [(list us ust1 ust2)
               #:when (and (undead-set? us)
                           (undead-set-tree? ust1)
                           (undead-set-tree? ust2))
               (... (fn-for-undead-set us)
                    (fn-for-undead-set-tree ust1)
                    (fn-for-undead-set-tree ust2))]
              [`(,us* ...)
               #:when (andmap undead-set? us*)
               (... (fn-for-undead-set-list us*))]))]
    (fn-for-undead-set-tree ust)))

;; Instruction is one of:
;; - Block-locals-lang/s
;; - (if cmp tail tail)
;; interp. the Block-locals-lang expressions referred to by instruction-nodes.

;; Instruction-node is an eq?-able node in a graph containing def and use
;; information for undead analysis.
;;
;; Assumption: the instruction-node-instruction is eq?-able to the instruction
;; in the Block-locals-lang program from which the instruction-node is
;; generated.
(struct instruction-node (instruction defs uses)
  #:transparent)

;; Control-Flow-Graph is a directed graph whose vertexes are Instruction-node.
;; The graph represents per-instruction successor (explicitly) and predecessor
;; (implicitly) information.

; Sanity check
(module+ test
  (let* ([x (instruction-node `(halt 5) '() '())]
         [ls (list x x)])
    (check-eq? (first ls) (second ls)
               "Instructions are identified using eq?")))

;; Env is a map from a label to an Instruction-node

;; Block-locals-lang/p -> (values Env Control-Flow-Graph)
;; produce a control-flow graph and corresponding label map for a given program
(define (block-locals-lang->cfg p)
  (local
    [;; opand -> (listof aloc)
     ;; collect any aloc uses from opand
     (define (opand->uses opand)
       (match opand
         [(? integer?) '()]
         [_ (list opand)]))

     ;; trg -> (listof aloc)
     ;; collect any aloc uses from trg
     (define (trg->uses trg)
       (match trg
         [(? label?) '()]
         [_ (list trg)]))

     ;; triv -> (listof aloc)
     ;; collect any aloc uses from triv
     (define (triv->uses triv)
       (match triv
         [(? label?) '()]
         [_ (opand->uses triv)]))

     ;; s -> Instruction-node
     ;; Create an Instruction node corresponding to given statement
     (define (stmt->inst-node s)
       (match s
         [`(set! ,aloc ,triv)
          #:when (triv/BlockAssignedLang? triv)
          (instruction-node s (list aloc) (triv->uses triv))]
         [`(set! ,aloc1 (,binop ,aloc2 ,opand))
          (instruction-node s (list aloc1)
                            (set-add (opand->uses opand) aloc2))]))

     ;; Control-flow-graph Env tail
     ;;   -> (values Instruction-node Control-flow-graph)
     ;; produce a cfg for the given tail, as well as its entry node.
     (define (tail->cfg cfg env tail)
       (match tail
         [`(begin ,ss ... ,tail)
          (let-values ([(entry^ cfg^) (tail->cfg cfg env tail)])
            ;; more space-efficient to use left-fold over reversed stmt list
            ;; than for/foldr over ss directly.
            (for/fold ([entry^ entry^]
                       [cfg^ cfg^])
                      ([s (reverse ss)])
              (let* ([node (stmt->inst-node s)]
                     [cfg^2 (add-directed-edge
                             (add-vertex cfg^ node)
                             node
                             entry^)]
                     [entry^2 node])
                (values entry^2 cfg^2))))]
         [`(if (,cmp ,aloc ,opand) ,tail1 ,tail2)
          (let*-values ([(entry1 cfg^) (tail->cfg cfg env tail1)]
                        [(entry2 cfg^2) (tail->cfg cfg^ env tail2)])
            (let* ([cmp-node (instruction-node tail '()
                                               (set-union (list aloc)
                                                          (opand->uses opand)))]
                   [cfg^3 (add-vertex cfg^2 cmp-node)]
                   [cfg^4 (add-directed-edge cfg^3 cmp-node entry1)]
                   [cfg^5 (add-directed-edge cfg^4 cmp-node entry2)])
              (values cmp-node cfg^5)))]
         [`(jump ,trg ,alocs ...)
          (let* ([node (instruction-node tail '()
                                         (set-union (trg->uses trg) alocs))]
                 [cfg^ (add-vertex cfg node)]
                 [cfg^2
                  (cond
                    [(label? trg) ;; name-direct: be precise about target
                     (add-directed-edge cfg^ node (dict-ref env trg))]
                    [(aloc? trg)
                     ;; aloc-indirect: conservatively assume *any* block target
                     (for/fold ([cfg^ cfg^])
                               ([ph (dict-values env)])
                       (add-directed-edge cfg^ node ph))])])
            (values node cfg^2))]
         [`(halt ,opand)
          (let ([node (instruction-node tail '() (opand->uses opand))])
            (values node (add-vertex cfg node)))]))

     (define (pgm->cfg p)
       (match p
         [`(module (define ,names ,infos ,tails) ...)
          (let* (;; associate each basic block name to its own graph placeholder
                 [env (for/list ([name names])
                        (cons name (make-placeholder (void))))]

                 ;; add each basic block to cfg and update its placeholder
                 [cfg
                  (for/fold ([cfg (new-graph)])
                            ([name names] [tail tails])
                    (let-values ([(entry cfg^) (tail->cfg cfg env tail)])
                      (begin
                        (placeholder-set! (dict-ref env name) entry)
                        cfg^)))])
            ;; patch the placeholders to form a graph
            (make-reader-graph cfg))]))]
    (pgm->cfg p)))

;; Check that pointer equality on instructions is preserved.
;; This is important for traversing graphs and associating information back to
;; instructions.
(module+ test
  (let* ([x '(module (define L.jump () (jump L.jump)))]
         [y (block-locals-lang->cfg x)])
    (check-eq?
     (car (second (car y)))
     (car (car y)))
    (check-eq?
     (instruction-node-instruction (car (second (car y))))
     (fourth (second x)))))


;; (Either 'undead-out 'undead-in) Block-locals-lang/p -> Undead-block-lang/p
;; compile Block-locals-lang program to its Undead-block-lang analogue,
;; attaching undead analysis information to the latter.
(define (undead-analysis/option option p)
  ;; Undead-set-map is (eq?-map Instruction Undead-set).

  ;; Undead-set-map Undead-set-map Block-locals-lang/p -> Undead-block-lang/p
  ;; Annotate a Block-locals-lang program with its undead set trees.
  ;; Creates info fields for both undead-in and undead-out, and an undead field
  ;; that duplicates which ever one is specified by option.
  (define (construct-undead-sets ins outs p)
    (local
      [;; Undead-set-map stmt -> Undead-set
       (define (construct-s ins s)
         (hash-ref ins s))

       ;; Undead-set-map tail -> Undead-set-tree??
       (define (construct-tail ins tail)
         (match tail
           [`(begin ,ss ... ,tail)
            `(,@(map (curry construct-s ins) ss)
              ,(construct-tail ins tail))]
           [`(if ,_ ,tail1 ,tail2)
            `(,(hash-ref ins tail)
              ,(construct-tail ins tail1)
              ,(construct-tail ins tail2))]
           [_ ;; jump or halt
            (hash-ref ins tail)]))

       ;; Undead-set-map Undead-set-map Block-locals-lang/b
       ;;   -> Undead-block-lang/b
       (define (construct-b ins outs b)
         (match b
           [`(define ,name ,info ,tail)
            (let* ([info^1 (info-set info 'undead-in (construct-tail ins tail))]
                   [info^2
                    (info-set info^1 'undead-out (construct-tail outs tail))]
                   [info^3
                    (info-set info^2 'undead (info-ref info^2 option))])
            `(define ,name ,info^3 ,tail))]))

       ;; Undead-set-map Undead-set-map Block-locals-lang/p
       ;;   -> Undead-block-lang/p
       (define (construct-p ins outs p)
         (match p
           [`(module ,bs ...)
            (let ([in-dict (node-dict->instr-dict ins)]
                  [out-dict (node-dict->instr-dict outs)])
            `(module ,@(for/list ([b bs])
                         (construct-b in-dict out-dict b))))]))]
      (construct-p ins outs p)))

  ;; Undead-set-node-map -> Undead-set-instr-map
  ;; Convert a dictionary that maps Instruction-node to Undead-sets, into a
  ;; dictionary that maps Instruction to an Undead-sets.
  (define (node-dict->instr-dict d)
    (for/fold ([newd (make-immutable-hasheq)])
              ([(key val) (in-dict d)])
      (match key
        [(instruction-node s _ _)
         (hash-set newd s val)])))

  ;; Undead-set-node-map (listof Instruction-node) Undead-set Undead-set
  ;;   -> Instruction-node
  ;; Update the given node's undead sets based on its neighbors
  (define (update-node ins succs undead-in undead-out node)
    (match node
      [(instruction-node _ defs uses)
       (let ([new-undead-in (set-union uses (set-subtract undead-out defs))]
             [new-undead-out
              (apply set-union
                     (cons '() (for/list ([s succs]) (dict-ref ins s))))])
         (values new-undead-in new-undead-out
                 (or (not (set=? undead-in new-undead-in))
                     (not (set=? undead-out new-undead-out)))))]))

  ;; Perform undead analysis on a CFG generated from the Block-locals-lang p
  (define cfg (block-locals-lang->cfg p))

  ;; Get a complete list of instruction nodes from the CFG
  ;; "get-vertexes" graph function isn't implemented: exploiting representation
  (define vs (dict-keys cfg))

  ;; Instruction-node -> Undead-set: archetypal undead-ins/undead-outs maps
  (define initial-map (make-immutable-hasheq (for/list ([v vs]) (cons v '()))))

  ;; Outer loop repeats full instruction-node traversal until nothing changes
  (let loop ([ins initial-map]
             [outs initial-map])
    (define-values (in out any-changed?)
      ;; Iterate over each instruction-node, updating its ins and outs
      (for/fold ([ins ins]
                 [outs outs]
                 [any-changes-yet? #f])
                ([v vs])
        (let-values ([(undead-in undead-out changed?)
                      (update-node
                       ins
                       (get-neighbors cfg v)
                       (dict-ref ins v)
                       (dict-ref outs v)
                       v)])
          (values (dict-set ins v undead-in)
                  (dict-set outs v undead-out)
                  (or changed? any-changes-yet?)))))
    (if any-changed?
        (loop in out)
        (construct-undead-sets in out p))))

(define undead-analysis (curry undead-analysis/option 'undead-out))


;; (listof (listof Aloc)) -> (listof Aloc)
;; Given a list of locals declarations, merge them into a single set of the all
;; abstract locations.
(define (merge-locals ls)
  (for/fold ([globals '()])
            ([l ls])
    (set-union globals l)))

;; (listof Conflict-graph) -> Conflict-graph
;; Given a set of all abstract locations in the program, and a list of conflict
;; graphs, merge the conflict graphs into one global conflict graph.
(define (merge-conflicts lsg)
  (for/fold ([g (new-graph)])
            ([conflictg lsg])
    (for/fold ([g g])
              ([(vertex edges) (in-dict conflictg)])
      (add-edges^ g vertex (car edges)))))

;; (setof Aloc) Assignment -> Assignment
;; Given a set of locals from a block and a global assignment, return a local
;; assignment for the block.
(define (localize-assignment locals assignment)
  (for/fold ([asn '()])
            ([var locals])
    (info-set asn var (info-ref assignment var))))

;; Graph Vertex -> (Listof Vertex)
;; Adds a vertex between v1 and each of the vs, but first ensures that the
;; vertexes exist in the graph.
(define (add-edges^ g v1 vs)
  (define all-vs (dict-keys g))
  (let ([g (for/fold ([g g])
                     ([v (cons v1 vs)])
             (if (member v all-vs)
                 g
                 (add-vertex g v)))])
    (add-edges g v1 vs)))
