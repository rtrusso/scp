;; sasm-dataflow.scm
;; Dataflow analysis for SASM

(need util/list)
(need util/vector)
(need sasm/fastset)
(need algo/graph)
(need sasm/fastgraph)

;; TODO:
;;  Dataflow analysis is currently oblivious of tree-structured SASM statements.
;;  For tree-structured expressions to be a first-class SASM feature, this module
;;  needs to be modified to recognize them.  A better abstraction of SASM statement
;;  syntax obviously needs to be developed.
;;

;; OPT:
;;  Incremental dataflow analysis would be the single most important optimization that
;;  could be done for SASM.  This would be a big investment but would have a big reward.
;;
;;  Basic-block optimization would be a significant investment as well, with I'm guessing
;;  a smaller payoff.
;;

;;
;; Determines if a given pointer into a SASM block ends the current
;; basic block.  The 'start-pointer' is the list of instructions
;; where the basic-block started.  The 'current-pointer' is the
;; list of instructions from the current instruction to the
;; end of the instruction block.
;;
(define (sasm-ends-basic-block? start-pointer current-pointer)
  (cond
   ((null? current-pointer)
    #t)
   (else 
    (case (car (car current-pointer))
      ((label)
       (let ((insns (memq-reverse (car current-pointer) start-pointer)))
         (not (accum (lambda (a b) (and a (eqv? 'label (car b))))
                     '()
                     insns))))
      ((goto branch branch-false branch-true branch-zero branch-nonzero) #t)
      (else
       (and (not (null? (cdr current-pointer)))
            (eqv? 'label (car (car (cdr current-pointer))))) )))))

(define (sasm-split-basic-blocks sasm-code)
  (define (iter result bb-cursor bb-start)
    (cond ((null? bb-cursor)
           (reverse (if (null? bb-start)
                        result
                        (cons (basic-block bb-start) result))))
          ((sasm-ends-basic-block? bb-start bb-cursor)
           (iter (cons (basic-block (reverse (memq-reverse (car bb-cursor)
                                                           bb-start)))
                       result)
                 (cdr bb-cursor) 
                 (cdr bb-cursor)))
          (else
           (iter result (cdr bb-cursor) bb-start))))
  (define (basic-block insns)
    insns)
  (iter '() sasm-code sasm-code))

(define (sasm-basic-block-entry-points basic-block)
  (let iter ((insns basic-block) (result '()))
    (cond ((or (null? insns)
               (not (eqv? 'label (car (car insns)))))
           result)
          (else
           (iter (cdr insns) (cons (list-ref (car insns) 1) result))))))

(define (sasm-statement-exit-points sasm-stmt)
  (case (car sasm-stmt)
    ((return)
     (list))
    ((branch branch-true branch-false branch-zero branch-nonzero)
     (list #f (list-ref (list-ref sasm-stmt 1) 1)))
    ((goto)
     (if (equal? 'label (car (list-ref sasm-stmt 1)))
         (list (list-ref (list-ref sasm-stmt 1) 1)) ;; intraproc goto
         (list) ))                                  ;; interproc tailcall
    (else
     (list #f))))

(define (sasm-basic-block-exit-points basic-block)
  (let ((end (terminal-pair basic-block)))
    (and end
         (sasm-statement-exit-points (car end)))))

(define (sasm-statement-link sasm-code sasm-stmt exit-point)
  (if (not exit-point)
      (let ((rest (memq sasm-stmt sasm-code)))
        (cond ((not rest) (error "Invalid SASM statement -- sasm-statement-link"))
              ((null? (cdr rest)) (error "SASM statement has no 'next' link -- sasm-statement-link"))
              (else (cadr rest))))
      (let ((insns (member `(label ,exit-point) sasm-code)))
        (if insns
            (car insns)
            (error "Unable to resolve label -- sasm-statement-link" exit-point)))))

(define (sasm-basic-block-link basic-blocks current-block exit-point)
  (if (not exit-point)
      (let ((rest (memq current-block basic-blocks)))
        (cond ((not rest) (error "Invalid basic-block -- sasm-basic-block-link"))
              ((null? (cdr rest)) (error "Basic block has no 'next' node -- sasm-basic-block-link"
                                         current-block exit-point))
              (else (cadr rest))))
      (let ((lookup (filter (lambda (bb) (member exit-point 
                                                 (sasm-basic-block-entry-points bb)))
                            basic-blocks)))
        (if (> (length lookup) 1)
            (error "Ambiguous exit-point -- sasm-basic-block-link")
            (car lookup)))))

(define (sasm-statement-id sasm-code stmt)
  (let ((entry (memq stmt sasm-code)))
    (if (not entry)
        (error "Unable to compute statement ID -- sasm-statement-id" stmt)
        (- (length sasm-code) (length entry)))))

(define (sasm-basic-block-id basic-blocks current-basic-block)
  (let ((entry (memq current-basic-block basic-blocks)))
    (if entry
        (- (length basic-blocks) (length entry))
        (error "Invalid basic-block, doesn't have ID wrt basic-blocks -- sasm-basic-block-id"))))

(define (sasm-lookup-statement-by-id sasm-code id)
  (list-ref sasm-code id))

(define (sasm-lookup-basic-block-by-id basic-blocks id)
  (list-ref basic-blocks id))

(define (sasm-build-control-flow-graph-from-basic-blocks basic-blocks)
  (map (lambda (bb)
         (cons (sasm-basic-block-id basic-blocks bb)
               (map (lambda (exit-point)
                      (sasm-basic-block-id basic-blocks 
                                           (sasm-basic-block-link basic-blocks bb exit-point)))
                    (sasm-basic-block-exit-points bb))))
       basic-blocks))

(define (sasm-build-control-flow-graph-from-statements sasm-code)
  (map (lambda (stmt)
         (cons (sasm-statement-id sasm-code stmt)
               (map (lambda (exit-point)
                      (sasm-statement-id sasm-code
                                         (sasm-statement-link sasm-code stmt exit-point)))
                    (sasm-statement-exit-points stmt))))
       sasm-code))

(define (sasm-print-dataflow-information sasm-code)
  (sasm-print-dataflow-information-for-basic-blocks (sasm-split-basic-blocks sasm-code)))

(define (sasm-print-dataflow-information-for-basic-blocks basic-blocks)
  (let ((cgf (sasm-build-control-flow-graph-from-basic-blocks basic-blocks)))
    (for-each (lambda (bb cgf-node)
                (display "== BASIC BLOCK ")
                (display (sasm-basic-block-id basic-blocks bb))
                (display " ==\n")
                (if (not (= (sasm-basic-block-id basic-blocks bb)
                            (car cgf-node)))
                    (error "Invalid CGF! " 
                           (sasm-basic-block-id basic-blocks bb)
                           (car cgf-node)))
                (display "links to:")
                (for-each (lambda (x) (display " ") (display x))
                          (cdr cgf-node))
                (newline)
                (display "entry points:")
                (for-each (lambda (x) (display " ") (display x))
                          (sasm-basic-block-entry-points bb))
                (newline)
                (for-each (lambda (x) (display "   ") (write x) (newline))
                          bb)
                (newline)
                (newline))
              basic-blocks
              cgf)))

;; reaching definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sasm-reachdef-id sasm-code stmt)
  (sasm-statement-id sasm-code stmt))

(define (sasm-reachdef-statement-defines stmt)
  (case (car stmt)
    ((assign)
     (list (list-ref stmt 1)))
    ((perform)
     (case (cadr (list-ref stmt 1))
       ((store-local)
        (list (list 'local (cadr (list-ref stmt 2)))))
       ((store-arg)
        (list (list 'arg (cadr (list-ref stmt 2)))))
       ((call tail-call this-call)
        '((reg accum) (reg operand) (reg index) (reg this)))
       (else
        #f)))
    (else #f)))

(define (sasm-reachdef-compute-defs sasm-code def)
  (accum append
         '()
         (filter-map (lambda (stmt)
                       (let ((d (sasm-reachdef-statement-defines stmt)))
                         (and d
                              (not (null? (intersect-lists equal? def d)))
                              (map (lambda (x) (cons (sasm-reachdef-id sasm-code stmt)
                                                     x))
                                   (intersect-lists equal? def d)))))
                     sasm-code)))

(define (sasm-reachdef-compute-gens sasm-code)
  (map (lambda (stmt)
         (if (sasm-reachdef-statement-defines stmt)
             (map (lambda (x) (cons (sasm-reachdef-id sasm-code stmt) x))
                  (sasm-reachdef-statement-defines stmt))
             '()))
       sasm-code))

(define (sasm-reachdef-compute-kills sasm-code)
  (let ((gens (sasm-reachdef-compute-gens sasm-code)))
    (map (lambda (stmt)
           (let ((def (sasm-reachdef-statement-defines stmt)))
             (if def
                 (subtract-lists equal?
                                 (sasm-reachdef-compute-defs sasm-code def)
                                 (map (lambda (x) (cons (sasm-reachdef-id sasm-code stmt)
                                                        x))
                                      def))
                 '())))
         sasm-code)))

;;
;; Dataflow equations for reaching definitions:
;;
;; (in-set n) = (accum-union p 
;;                           (predecessors n)
;;                           (out-set p))
;;
;; (out-set n) = (union (gen-set n) 
;;                      (minus (in-set n) (kill-set n)))
;;
(define (sasm-slow-reachdef-compute sasm-code)
  (let ((g (sasm-build-control-flow-graph-from-statements sasm-code))
        (nodes (map (lambda (stmt) (sasm-reachdef-id sasm-code stmt)) sasm-code))
        (gen-sets (sasm-reachdef-compute-gens sasm-code))
        (kill-sets (sasm-reachdef-compute-kills sasm-code)))
    (define (compute-in-sets nodes in-sets out-sets)
      (map (lambda (node)
             (accum (list-unioner equal?)
                    '()
                    (map (lambda (x) (list-ref out-sets x))
                         (graph-predecessors g node))))
           nodes))
    (define (compute-out-sets nodes in-sets out-sets)
      (map (lambda (node)
             (union-lists equal?
                          (list-ref gen-sets node)
                          (subtract-lists equal? 
                                          (list-ref in-sets node)
                                          (list-ref kill-sets node))))
           nodes))
    (define (sets-equal? a b)
      (list-set=? equal? a b))
    (define (meta-sets-equal? a b)
      (all? (lambda (x) x)
            (map sets-equal? a b)))
    (define (iterate nodes in-sets out-sets)
      (let ((next-in (compute-in-sets nodes in-sets out-sets)))
        (let ((next-out (compute-out-sets nodes next-in out-sets)))
          (if (and (meta-sets-equal? in-sets next-in)
                   (meta-sets-equal? out-sets next-out))
              (list next-in next-out)
              (iterate nodes next-in next-out)))))
    (iterate nodes 
             (map (lambda (x) '())
                  nodes)
             (map (lambda (x) '())
                  nodes))))

(define (sasm-fast-reachdef-compute sasm-code)
  (let* ((g (sasm-build-control-flow-graph-from-statements sasm-code))
         (g-elem (graph-elements g))
         (nodes (list->vector g-elem))
         (n-nodes (vector-length nodes))

         (parents (list->vector (map (lambda (elem)
                                       (graph-predecessors g elem))
                                     g-elem)))
         (gen-sets (sasm-reachdef-compute-gens sasm-code))
         (kill-sets (sasm-reachdef-compute-kills sasm-code))

         (symset (accum append '() gen-sets))

         (imp (fastset-importer symset))
         (exp (fastset-exporter imp symset))
         (empty-fastset (fastset-collapse imp '()))

         (gen-fs (list->vector (map (lambda (x) (fastset-collapse imp x))
                                    gen-sets)))
         (kill-fs (list->vector (map (lambda (x) (fastset-collapse imp x))
                                     kill-sets))))

    (define (compute-in-sets in-sets out-sets)
      (vector-map (lambda (node)
                    (accum-map fastset-union
                               empty-fastset
                               (lambda (n) (vector-ref out-sets n))
                               (vector-ref parents node)))
                  nodes))

    (define (compute-out-sets in-sets out-sets)
      (vector-map (lambda (node)
                    (fastset-union (vector-ref gen-fs node)
                                   (fastset-subtract (vector-ref in-sets node)
                                                     (vector-ref kill-fs node))))
                  nodes))

    (define (meta-sets-equal? a b)
      (let ((n (vector-length a)))
        (let loop ((i 0))
          (or (>= i n)
              (and (fastset-equal? (vector-ref a i) (vector-ref b i))
                   (loop (+ i 1)))))))

    (define (expand-sets the-set)
      (map (lambda (x) (fastset-expand exp x))
           (vector->list the-set)))

    (define (iterate in-sets out-sets)
      (let* ((next-in (compute-in-sets in-sets out-sets))
             (next-out (compute-out-sets next-in out-sets)))
        (if (and (meta-sets-equal? in-sets next-in)
                 (meta-sets-equal? out-sets next-out))
            (list (expand-sets next-in) (expand-sets next-out))
            (iterate next-in next-out))))

    (iterate (make-vector n-nodes empty-fastset)
             (make-vector n-nodes empty-fastset))

    ))

(define (sasm-reachdef-compute sasm-code)
  (sasm-fast-reachdef-compute sasm-code))

(define (sasm-strong-reachdef-compute-without sasm-code)
  (sasm-strong-reachdef-compute (sasm-reachdef-compute sasm-code) sasm-code))

(define (sasm-strong-reachdef-compute reachdef sasm-code)
  (let* ((g (sasm-build-control-flow-graph-from-statements sasm-code))
         (nodes (map (lambda (stmt) (sasm-statement-id sasm-code stmt)) sasm-code))
         (gen-sets (sasm-reachdef-compute-gens sasm-code))
         (reachdef-in (list-ref reachdef 0))
         (reachdef-out (list-ref reachdef 1)))
    (define (compute-in-sets nodes in-sets out-sets)
      (map (lambda (node)
             (let ((parents (graph-predecessors g node)))
               (if (null? parents)
                   '()
                   (accum (list-intersecter equal?)
                          (list-ref out-sets (car parents))
                          (map (lambda (x) (list-ref out-sets x))
                               (cdr parents))))))
           nodes))
    (define (compute-out-sets nodes in-sets out-sets)
      (map (lambda (node)
             (intersect-lists equal?
                              (list-ref reachdef-out node)
                              (union-lists equal?
                                           (list-ref in-sets node)
                                           (list-ref gen-sets node))))
           nodes))
    (define (sets-equal? a b)
      (list-set=? equal? a b))
    (define (meta-sets-equal? a b)
      (all? (lambda (x) x)
            (map sets-equal? a b)))
    (define (iterate nodes in-sets out-sets)
      (let ((next-in (compute-in-sets nodes in-sets out-sets)))
        (let ((next-out (compute-out-sets nodes next-in out-sets)))
          (if (and (meta-sets-equal? in-sets next-in)
                   (meta-sets-equal? out-sets next-out))
              (list next-in next-out)
              (iterate nodes next-in next-out)))))
    (iterate nodes 
             reachdef-in
             reachdef-out)))

;; available expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sasm-availexp-available-expression stmt)
  (case (car stmt)
    ((assign)
     (if (equal? 'op (car (list-ref stmt 2)))
         (case (cadr (list-ref stmt 2))
           ((load-array load-arg load-local)
            #f)
           (else
            (cddr stmt)))
         #f))
    (else #f)))

(define (sasm-availexp-killed-expressions stmt)
  (sasm-reachdef-statement-defines stmt))

(define (sasm-availexp-all-expressions sasm-code)
  (remove-duplicates equal?
                     (filter-map sasm-availexp-available-expression
                                 sasm-code)))

(define (sasm-availexp-compute-kills sasm-code)
  (let ((all (sasm-availexp-all-expressions sasm-code)))
    (map (lambda (stmt)
           (let ((kills (sasm-availexp-killed-expressions stmt)))
             (if kills
                 (filter-map (lambda (exp)
                               (let loop ((kills kills))
                                 (cond ((null? kills)
                                        #f)
                                       ((member (car kills) exp)
                                        exp)
                                       (else
                                        (loop (cdr kills))))))
                             all)
                 '())))
         sasm-code)))

(define (sasm-availexp-compute-gens sasm-code)
  (let ((kills (sasm-availexp-compute-kills sasm-code)))
    (map (lambda (stmt kill)
           (let ((avail (sasm-availexp-available-expression stmt)))
             (if avail
                 (subtract-lists equal?
                                 (list avail)
                                 kill)
                 '())))
         sasm-code
         kills)))

(define (sasm-availexp-compute sasm-code)
  (let ((g (sasm-build-control-flow-graph-from-statements sasm-code))
        (all (sasm-availexp-all-expressions sasm-code))
        (nodes (map (lambda (stmt) (sasm-statement-id sasm-code stmt)) sasm-code))
        (gen-sets (sasm-availexp-compute-gens sasm-code))
        (kill-sets (sasm-availexp-compute-kills sasm-code)))
    (define (compute-in-sets nodes in-sets out-sets)
      (map (lambda (node)
             (let ((parents (graph-predecessors g node)))
               (if (null? parents)
                   '()
                   (accum (list-intersecter equal?)
                          (list-ref out-sets (car parents))
                          (map (lambda (x) (list-ref out-sets x))
                               (cdr parents))))))
           nodes))
    (define (compute-out-sets nodes in-sets out-sets)
      (map (lambda (node)
             (union-lists equal?
                          (list-ref gen-sets node)
                          (subtract-lists equal? 
                                          (list-ref in-sets node)
                                          (list-ref kill-sets node))))
           nodes))
    (define (sets-equal? a b)
      (list-set=? equal? a b))
    (define (meta-sets-equal? a b)
      (all? (lambda (x) x)
            (map sets-equal? a b)))
    (define (iterate nodes in-sets out-sets)
      (let ((next-in (compute-in-sets nodes in-sets out-sets)))
        (let ((next-out (compute-out-sets nodes next-in out-sets)))
          (if (and (meta-sets-equal? in-sets next-in)
                   (meta-sets-equal? out-sets next-out))
              (list next-in next-out)
              (iterate nodes next-in next-out)))))
    (iterate nodes 
             ; initialize all sets other than the start node's in-set to all
             (map (lambda (x) (if (zero? x) '() all))
                  nodes)
             (map (lambda (x) all)
                  nodes))))

(define (sasm-availexp-compute-kills-id sasm-code)
  (map (lambda (stmt-a)
         (let ((kills (sasm-availexp-killed-expressions stmt-a)))
           (filter-map (lambda (stmt-b)
                         (and (not (eq? stmt-a stmt-b))
                              (sasm-availexp-available-expression stmt-b)
                              kills
                              (let loop ((kills kills))
                                (and (not (null? kills))
                                     (or (member (car kills) (sasm-availexp-available-expression stmt-b))
                                         (loop (cdr kills)))))
                              (sasm-statement-id sasm-code stmt-b)))
                       sasm-code)))
       sasm-code))

(define (sasm-availexp-compute-gens-id sasm-code)
  (let ((kills (sasm-availexp-compute-kills-id sasm-code)))
    (map (lambda (stmt kill)
           (let ((avail (sasm-availexp-available-expression stmt)))
             (if avail
                 (subtract-lists equal?
                                 (list (sasm-statement-id sasm-code stmt))
                                 kill)
                 '())))
         sasm-code
         kills)))

(define (sasm-availexp-id-compute sasm-code)
  (let ((g (sasm-build-control-flow-graph-from-statements sasm-code))
        (all (sasm-availexp-all-expressions sasm-code))
        (nodes (map (lambda (stmt) (sasm-statement-id sasm-code stmt)) sasm-code))
        (gen-sets (sasm-availexp-compute-gens-id sasm-code))
        (kill-sets (sasm-availexp-compute-kills-id sasm-code)))
    (define (compute-in-sets nodes in-sets out-sets)
      (map (lambda (node)
             (let ((parents (graph-predecessors g node)))
               (if (null? parents)
                   '()
                   (accum (list-intersecter equal?)
                          (list-ref out-sets (car parents))
                          (map (lambda (x) (list-ref out-sets x))
                               (cdr parents))))))
           nodes))
    (define (compute-out-sets nodes in-sets out-sets)
      (map (lambda (node)
             (union-lists equal?
                          (list-ref gen-sets node)
                          (subtract-lists equal? 
                                          (list-ref in-sets node)
                                          (list-ref kill-sets node))))
           nodes))
    (define (sets-equal? a b)
      (list-set=? equal? a b))
    (define (meta-sets-equal? a b)
      (all? (lambda (x) x)
            (map sets-equal? a b)))
    (define (iterate nodes in-sets out-sets)
      (let ((next-in (compute-in-sets nodes in-sets out-sets)))
        (let ((next-out (compute-out-sets nodes next-in out-sets)))
          (if (and (meta-sets-equal? in-sets next-in)
                   (meta-sets-equal? out-sets next-out))
              (list next-in next-out)
              (iterate nodes next-in next-out)))))
    (iterate nodes 
             ; initialize all sets other than the start node's in-set to all
             (map (lambda (x) (if (zero? x) '() all))
                  nodes)
             (map (lambda (x) all)
                  nodes))))

;; liveness ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; == SASM statement types ==
;; return
;; label
;; assign
;; perform
;; test
;; [REWRITE] save
;; [REWRITE] restore
;; push
;; pop
;; branch
;; branch-true
;; branch-false
;; branch-zero
;; branch-nonzero
;; goto
;;
;; == SASM expression types ==
;; reg temp arg local const label
;;

;; TODO: change (op call) into (op this-call) and (op static-call)
;;

(define (sasm-liveness-statement-gens stmt)
  (define (rvalues stmt)
    (accum append
           '()
           (filter-map (lambda (x)
                         (and (list? x)
                              (case (car x)
                                ((reg temp arg local sys)
                                 (list x))
                                ((result)
                                 (rvalues (cdr x)))
                                (else #f))))
                       stmt)))
  (cond ((equal? 'assign (car stmt))
         (rvalues (cddr stmt)))
        ((equal? 'return (car stmt))
         (list '(reg accum) '(sys frame-pointer) '(sys stack-pointer)))
        ((and (equal? 'perform (car stmt))
              (= 3 (length stmt))
              (or (equal? '(op this-call) (list-ref stmt 1))
                  (and (equal? '(op call) (list-ref stmt 1))
                       (not (equal? 'const (car (list-ref stmt 2)))))))
         (remove-duplicates equal?
                            (cons '(reg this)
                                  (rvalues stmt))))
        ((and (equal? 'perform (car stmt))
              (>= (length stmt) 3)
              (equal? '(op tail-call) (list-ref stmt 1))
              (not (equal? 'const (car (list-ref stmt 2)))))
         (if (not (= 5 (length stmt)))
             (error "Unrecognized tail-call stmt " stmt))
         (remove-duplicates equal?
                            (cons '(reg this)
                                  (rvalues stmt))))
        (else
         (rvalues stmt))))

(define (sasm-liveness-statement-kills stmt)
  (let ((defs (sasm-reachdef-statement-defines stmt)))
    (if defs
        defs
        '())))

(define (sasm-liveness-compute-gens sasm-code)
  (map sasm-liveness-statement-gens sasm-code))

(define (sasm-liveness-compute-kills sasm-code)
  (map sasm-liveness-statement-kills sasm-code))

(define (sasm-liveness-compute sasm-code)
  (let* ((g (sasm-build-control-flow-graph-from-statements sasm-code))
         (g-elem (graph-elements g))
         (nodes (list->vector g-elem))
         (n-nodes (vector-length nodes))
         (children (list->vector (map (lambda (elem)
                                        (graph-successors g elem))
                                      g-elem)))
         (@gen-sets (sasm-liveness-compute-gens sasm-code))
         (@kill-sets (sasm-liveness-compute-kills sasm-code))
         (symset (remove-duplicates equal?
                                    (accum append
                                           '()
                                           (append @gen-sets
                                                   @kill-sets))))
         ;; function to import a symbol into the fastset
         (import-symbol (fastset-importer symset))
         ;; function to export a symbol from the fastset
         (export-symbol (fastset-exporter import-symbol symset))
         (empty-fastset (fastset-collapse import-symbol '()))
         ;; The gen-sets list as a vector of fastset elements
         (gen-fastsets (list->vector (map (lambda (x)
                                            (fastset-collapse import-symbol
                                                              x))
                                          @gen-sets)))
         (kill-fastsets (list->vector (map (lambda (x)
                                             (fastset-collapse import-symbol
                                                               x))
                                           @kill-sets)))
         )
    (define (compute-in-sets in-sets out-sets)
      (vector-map (lambda (node)
                    (fastset-union (vector-ref gen-fastsets node)
                                   (fastset-subtract (vector-ref out-sets
                                                                 node)
                                                     (vector-ref kill-fastsets
                                                                 node))))
                  nodes))
    (define (compute-out-sets in-sets out-sets)
      (vector-map (lambda (node)
                    (accum-map fastset-union
                               empty-fastset
                               (lambda (n) (vector-ref in-sets n))
                               (vector-ref children node)))
                  nodes))
    (define (meta-sets-equal? a b)
      (let ((n (vector-length a)))
        (let loop ((i 0))
          (or (>= i n)
              (and (fastset-equal? (vector-ref a i) (vector-ref b i))
                   (loop (+ i 1)))))))
    (define (expand-sets the-set)
      (map (lambda (x) (fastset-expand export-symbol x))
           (vector->list the-set)))
    (define (iterate in-sets out-sets)
      (let ((next-out (compute-out-sets in-sets out-sets)))
        (let ((next-in (compute-in-sets in-sets next-out)))
          (if (and (meta-sets-equal? in-sets next-in)
                   (meta-sets-equal? out-sets next-out))
              (list (expand-sets next-in)
                    (expand-sets next-out))
              (iterate next-in next-out)))))
    (iterate (make-vector n-nodes empty-fastset)
             (make-vector n-nodes empty-fastset))
    ))

(define (sasm-liveness-compute-slow sasm-code)
  (let* ((g (sasm-build-control-flow-graph-from-statements sasm-code))
         (nodes (map (lambda (stmt) (sasm-statement-id sasm-code stmt))
                     sasm-code))
         (gen-sets (sasm-liveness-compute-gens sasm-code))
         (kill-sets (sasm-liveness-compute-kills sasm-code))
         (symset (accum append '() gen-sets))
         ;; function to import a symbol into the fastset
         (import-symbol (fastset-importer symset))
         ;; function to export a symbol from the fastset
         (export-symbol (fastset-exporter import-symbol symset))
         ;; The gen-sets vector as a 
         )
    (define (compute-in-sets nodes in-sets out-sets)
      (map (lambda (node)
             (union-lists equal?
                          (list-ref gen-sets node)
                          (subtract-lists equal? 
                                          (list-ref out-sets node)
                                          (list-ref kill-sets node))))
           nodes))
    (define (compute-out-sets nodes in-sets out-sets)
      (map (lambda (node)
             (accum (list-unioner equal?)
                    '()
                    (map (lambda (x) (list-ref in-sets x))
                         (graph-successors g node))))
           nodes))
    (define (sets-equal? a b)
      (list-set=? equal? a b))
    (define (meta-sets-equal? a b)
      (all? (lambda (x) x)
            (map sets-equal? a b)))
    (define (iterate nodes in-sets out-sets)
      (let ((next-out (compute-out-sets nodes in-sets out-sets)))
        (let ((next-in (compute-in-sets nodes in-sets next-out)))
          (if (and (meta-sets-equal? in-sets next-in)
                   (meta-sets-equal? out-sets next-out))
              (list next-in next-out)
              (iterate nodes next-in next-out)))))
    (iterate nodes 
             (map (lambda (x) '())
                  nodes)
             (map (lambda (x) '())
                  nodes))))

(define (sasm-dataflow-annotate sasm-code)
  (sasm-dataflow-annotate-prefix "   " sasm-code))

(define (sasm-dataflow-annotate-prefix prefix sasm-code)
  (let ((reachdef (sasm-reachdef-compute sasm-code))
        (availexp (sasm-availexp-compute sasm-code))
        (liveness (sasm-liveness-compute sasm-code)))
    (define (pre-data data)
      (car data))
    (define (post-data data)
      (cadr data))
    (define (println-iter l)
      (if (null? l)
          (newline)
          (begin (display " ")
                 (display (car l))
                 (println-iter (cdr l)))))
    (define (println msg l)
      (display prefix)
      (display msg)
      (println-iter l))
    (define (print-header header reach avail live)
      (if (or (not (null? reach))
              (not (null? avail))
              (not (null? live)))
          (println header '())))
    (define (print-info reach avail live)
      (if (not (null? reach))
          (println ";; Reaching definitions:" reach))
      (if (not (null? avail))
          (println ";; Available expressions:" avail))
      (if (not (null? live))
          (println ";; Live values:" live)) )
    (let iter ((insns sasm-code)
               (both? #t)
               (first? #t)
               (r-pre (pre-data reachdef))
               (r-post (post-data reachdef))
               (a-pre (pre-data availexp))
               (a-post (post-data availexp))
               (l-pre (pre-data liveness))
               (l-post (post-data liveness)))
      (if (null? insns)
          #t
          (let* ((next? (not (null? (cdr insns))))
                 (both-next? (and next?
                                  (not (and 
                                        (list-set=? equal?
                                                    (car r-post)
                                                    (cadr r-pre))
                                        (list-set=? equal?
                                                    (car a-post)
                                                    (cadr a-pre))
                                        (list-set=? equal?
                                                    (car l-post)
                                                    (cadr l-pre)))))))
            (if (and both? (not first?))
                (print-header ";; === Pre-Statement ==="
                              (car r-pre) (car a-pre) (car l-pre)))
            (if both?
                (print-info (car r-pre) (car a-pre) (car l-pre)))
            (display prefix)
            (write (car insns))
            (display " ;; ")
            (display (sasm-statement-id sasm-code (car insns)))
            (newline)
            (if both-next?
                (print-header ";; === Post-Statement ==="
                              (car r-post) (car a-post) (car l-post)))
            (print-info (car r-post) (car a-post) (car l-post))
            (iter (cdr insns)
                  both-next?
                  #f
                  (cdr r-pre)
                  (cdr r-post)
                  (cdr a-pre)
                  (cdr a-post) 
                  (cdr l-pre)
                  (cdr l-post)))))))

;; transform: constant propagation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sasm-statement-fold-constants sasm-stmt)
  (define (const? exp)
    (eqv? 'const (car exp)))
  (define (const exp)
    (list-ref exp 1))
  (define (known? op)
    (member (cadr op) '(add sub mul)))
  (define (fold op x y)
    (case (cadr op)
      ((add) (+ (const x) (const y)))
      ((sub) (- (const x) (const y)))
      ((mul) (* (const x) (const y)))
      (else (error "unknown fold operation" op))))
  (if (and (eqv? 'assign (car sasm-stmt))
           (eqv? 'op (car (list-ref sasm-stmt 2)))
           (= 5 (length sasm-stmt))
           (known? (list-ref sasm-stmt 2))
           (const? (list-ref sasm-stmt 3))
           (const? (list-ref sasm-stmt 4)))
      `(assign ,(list-ref sasm-stmt 1)
               (const ,(fold (list-ref sasm-stmt 2)
                             (list-ref sasm-stmt 3)
                             (list-ref sasm-stmt 4))))
      sasm-stmt))

(define (map-rvalues proc sasm-stmt)
  (case (car sasm-stmt)
    ((push pop)
     `(,(car sasm-stmt)
       ,(proc (list-ref sasm-stmt 1))))
    ((test perform)
     `(,(list-ref sasm-stmt 0)
       ,(list-ref sasm-stmt 1)
       ,@(map proc (cddr sasm-stmt))))
    ((assign)
     (if (equal? 'op (car (list-ref sasm-stmt 2)))
         `(assign ,(list-ref sasm-stmt 1)
                  ,(list-ref sasm-stmt 2)
                  ,@(map proc (cdddr sasm-stmt)))
         `(assign ,(list-ref sasm-stmt 1)
                  ,(proc (list-ref sasm-stmt 2)))))
    (else
     sasm-stmt)))

(define (sasm-statement-rvalue-known? sasm-code sasm-stmt reachdef rvalue)
  (= 1 (length (filter (lambda (x) (equal? rvalue x))
                       (map cdr (list-ref (car reachdef)
                                          (sasm-statement-id sasm-code sasm-stmt)))))))

(define (sasm-statement-rvalue-def-stmt sasm-code sasm-stmt reachdef rvalue)
  (caar (filter (lambda (x) (equal? rvalue (cdr x)))
                (list-ref (car reachdef)
                          (sasm-statement-id sasm-code sasm-stmt)))))

(define (sasm-reachdef-by-rvalue stmt-reachdef rvalue)
  (filter-map (lambda (x) (and (equal? rvalue (cdr x))
                               (car x)))
              stmt-reachdef))

(define (sasm-reachdef-by-multiple-rvalues stmt-reachdef rvalues)
  (accum append
         '()
         (map (lambda (rvalue) (sasm-reachdef-by-rvalue stmt-reachdef rvalue))
              rvalues)))

(define (sasm-statement-uses-rvalue? sasm-stmt rvalue)
  (member rvalue (sasm-liveness-statement-gens sasm-stmt)))

(define (sasm-statements-using-rvalue sasm-code rvalue)
  (filter-map (lambda (stmt) 
                (and (sasm-statement-uses-rvalue? stmt rvalue)
                     (sasm-statement-id sasm-code stmt)))
              sasm-code))

(define (sasm-statement-rvalue-def-avail? sasm-code ref-code sasm-stmt reachdef reachdef-soft availexp rvalue)
  (and (sasm-statement-rvalue-known? sasm-code sasm-stmt reachdef rvalue)
       (let ((candidate (sasm-statement-rvalue-def-stmt sasm-code sasm-stmt reachdef rvalue)))
         (and (member candidate
                (list-ref (car availexp)
                          (sasm-statement-id sasm-code sasm-stmt)))
              (let* ((new-stmt (list-ref ref-code candidate))
                     (used (sasm-liveness-statement-gens new-stmt))
                     (in-candidate (sasm-reachdef-by-multiple-rvalues (list-ref (car reachdef-soft) 
                                                                                candidate)
                                                                      used))
                     (in-current (sasm-reachdef-by-multiple-rvalues (list-ref (car reachdef-soft)
                                                                              (sasm-statement-id sasm-code sasm-stmt))
                                                                    used)))
                (list-set=? equal?
                            in-candidate
                            in-current))))))

(define (sasm-statement-rvalue-def-fold-rhs sasm-code ref-code sasm-stmt reachdef availexp rvalue)
  (let ((known (sasm-statement-rvalue-def-stmt sasm-code sasm-stmt reachdef rvalue)))
    (cons 'result (cddr (list-ref ref-code known)))))

(define (sasm-const-prop-iter sasm-code)
  (let ((reachdef (sasm-strong-reachdef-compute-without sasm-code)))
    ;; to fold a r-value ref to a constant:
    ;;
    ;;  1. Only one definition of that r-value reaches this statement
    ;;     a. for each reaching definition, find the defined r-value
    ;;     b. filter those that are the same as the r-value in question
    ;;     c. if the list of matches is of length=1
    ;;  2. That definition must be a constant
    ;;     a. check that the definition is an const assignment
    (define (fold-stmt stmt)
      (let ((id (sasm-statement-id sasm-code stmt)))
        (let ((reach (list-ref (car reachdef) id)))
          (define (rvalue-known? rvalue)
            (= 1 (length (filter (lambda (x) (equal? rvalue x))
                                 (map cdr reach)))))
          (define (known-rvalue-def rvalue)
            (caar (filter (lambda (x) (equal? rvalue (cdr x)))
                          reach)))
          (define (known-rvalue-stmt rvalue)
            (list-ref sasm-code (known-rvalue-def rvalue)))
          (define (const-assignment? stmt)
            (and (equal? (car stmt) 'assign)
                 (= 3 (length stmt))
                 (equal? (car (list-ref stmt 2)) 'const)))
          (sasm-statement-fold-constants
           (map-rvalues (lambda (rvalue)
                          (if (and (rvalue-known? rvalue)
                                   (const-assignment? (known-rvalue-stmt rvalue)))
                              (list-ref (known-rvalue-stmt rvalue) 2)
                              rvalue))
                        stmt)))))
    (map fold-stmt sasm-code)))

(define (sasm-statements-defining sasm-code rvalue)
  (filter-map (lambda (stmt)
                (and (sasm-reachdef-statement-defines stmt)
                     (member rvalue (sasm-reachdef-statement-defines stmt))
                     (cons (sasm-statement-id sasm-code stmt)
                           rvalue)))
              sasm-code))

(define (sasm-copy-prop-iter sasm-code)
  ;; to fold temporaries in a statement:
  ;;
  ;;    (assign x y) ;; statement i
  ;;    ...
  ;;    (<foo> x <bar>) ;; statment j
  ;;
  ;; replace with
  ;;
  ;;    (assign x y) ;; statement i
  ;;    ...
  ;;    (<foo> y <bar>) ;; statement j
  ;;
  ;; 1. Only one definition of the x reaches the statement,
  ;;    along all possible code paths
  ;;    a. for each reaching definition, find the defined r-value
  ;;    b. filter those that are the same as the r-value in question
  ;;    c. if the list of matches is of length=1, then OK
  ;; 2. No definitions of y must reach statement j other than i,
  ;;    along any possible code path
  ;;    a. find the set of statements defining y
  ;;    b. intersect with the reachdef of statement i (alpha)
  ;;    c. intersect with the reachdef of statement j (beta)
  ;;    d. if alpha and beta are the same sets, then OK
  ;;
  (let* ((reachdef (sasm-reachdef-compute sasm-code))
         (strong-reachdef (sasm-strong-reachdef-compute reachdef sasm-code)))
    (define (fold-stmt stmt)
      (let ((id (sasm-statement-id sasm-code stmt)))
        (let ((reach (list-ref (car strong-reachdef) id)))
          (define (rvalue-known? rvalue)
            (= 1 (length (filter (lambda (x) (equal? rvalue x))
                                 (map cdr reach)))))
          (define (known-rvalue-def rvalue)
            (caar (filter (lambda (x) (equal? rvalue (cdr x)))
                          reach)))
          (define (known-rvalue-stmt rvalue)
            (list-ref sasm-code (known-rvalue-def rvalue)))
          (define (single-assignment? stmt)
            (and (equal? (car stmt) 'assign)
                 (= 3 (length stmt))))
          (define (single-assignment-rvalue stmt)
            (list-ref stmt 2))
          (sasm-statement-fold-constants
           (map-rvalues (lambda (rvalue)
                          (if (and (rvalue-known? rvalue)
                                   (let ((known-def (known-rvalue-def rvalue))
                                         (known-stmt (known-rvalue-stmt rvalue)))
                                     (and known-def
                                          known-stmt
                                          (single-assignment? known-stmt)
                                          (let* ((known-rvalue (single-assignment-rvalue known-stmt))
                                                 (defs (sasm-statements-defining sasm-code known-rvalue)))
;                                            (println "reach: " reach)
;                                            (println "defs: " defs)
;                                            (println "known-statement: " known-stmt)
;                                            (println "s-a-r: " (single-assignment-rvalue known-stmt))
;                                            (println "id: " id)
;                                            (println "kn: " known-def)
;                                            (println "id-defs: " (list-ref (car reachdef) id))
;                                            (println "kn-defs: " (list-ref (car reachdef) known-def))
                                            (let ((result
                                                   (and (list-set=? equal?
                                                                    (intersect-lists equal? 
                                                                                     defs 
                                                                                     (list-ref (car reachdef) id))
                                                                    (intersect-lists equal?
                                                                                     defs 
                                                                                     (list-ref (car reachdef) known-def)))
                                                        (or ;#t
                                                            (not (equal? 'reg (car rvalue)))
                                                            (equal? 'reg (car known-rvalue))))))
;                                              (println "final-result: " result)
;                                              (if result
;                                                  (println "replacing [" rvalue "] with [" (single-assignment-rvalue known-stmt) "] in " stmt))
;                                              (println)
                                              result)))))
                              (single-assignment-rvalue (known-rvalue-stmt rvalue))
                              rvalue))
                        stmt)))))
    (map fold-stmt sasm-code)))

(define (sasm-deadcode-eliminate-iter sasm-code)
  (let ((live-out (list-ref (sasm-liveness-compute sasm-code) 1)))
    (filter (lambda (stmt)
              ;; Deadcode:
              ;;
              ;;   1. Self-assignments
              ;;   2. Assignments with targets that are not out-live
              (and (not (and (equal? 'assign (car stmt))
                             (= 3 (length stmt))
                             (equal? (list-ref stmt 1) (list-ref stmt 2))))
                   (or (not (equal? 'assign (car stmt)))
                       (member (list-ref stmt 1) 
                               (list-ref live-out 
                                         (sasm-statement-id sasm-code stmt))) )))
            sasm-code)))

(define (sasm-statements-generating-availexp sasm-code availexp)
  (filter-map (lambda (stmt)
                (and (sasm-availexp-available-expression stmt)
                     (equal? availexp (sasm-availexp-available-expression stmt))
                     (sasm-statement-id sasm-code stmt)))
              sasm-code))

(define (sasm-subexp-eliminate-iter sasm-code)
  (let ((availexp-in (list-ref (sasm-availexp-id-compute sasm-code) 0)))
    (define (copycat stmt)
      (and (sasm-availexp-available-expression stmt)
           (let ((parent-ids (filter-map (lambda (x)
                                           (and (sasm-availexp-available-expression x)
                                                (equal? (sasm-availexp-available-expression x)
                                                        (sasm-availexp-available-expression stmt))
                                                (sasm-statement-id sasm-code x)))
                                         (map (lambda (x) (list-ref sasm-code x))
                                              (list-ref availexp-in (sasm-statement-id sasm-code stmt))))))
             (and (not (null? parent-ids))
                  (list (car parent-ids)
                        (sasm-statement-id sasm-code stmt))))))
    (define (optimize stmt)
      (let ((copy-id (copycat stmt)))
        (if copy-id
            (list copy-id)
            '())))
    (define (iter remaining rewrite)
      (if (null? remaining)
          (rewrite-code sasm-code rewrite)
          (iter (cdr remaining)
                (append (optimize (car remaining))
                        rewrite))))
    (define (rewrite-iter original-code new-code parent child)
      (if (null? original-code)
          (reverse new-code)
          (let ((p-ent (assoc (sasm-statement-id sasm-code (car original-code)) parent))
                (c-ent (assoc (sasm-statement-id sasm-code (car original-code)) child)))
            (cond ((and p-ent c-ent)
                   (rewrite-iter 
                    (cdr original-code)
                    (cons `(assign (temp ,(cdr p-ent)) 
                                   ,(list-ref (car original-code) 1))
                          (cons `(assign ,(list-ref (car original-code) 1)
                                         (temp ,(cdr c-ent)))
                                new-code))
                    parent
                    child))
                  (p-ent
                   (rewrite-iter 
                    (cdr original-code)
                    (cons `(assign (temp ,(cdr p-ent)) ,(list-ref (car original-code) 1))
                          (cons (car original-code)
                                new-code))
                    parent
                    child))
                  (c-ent
                   (rewrite-iter
                    (cdr original-code)
                    (cons `(assign ,(list-ref (car original-code) 1)
                                   (temp ,(cdr c-ent)))
                          new-code)
                    parent
                    child))
                  (else
                   (rewrite-iter
                    (cdr original-code)
                    (cons (car original-code)
                          new-code)
                    parent
                    child))))))
    (define (rewrite-code sasm-code rewrite)
      (let ((with-temps (map (let ((id (sasm-temp-max sasm-code)))
                               (lambda (x)
                                 (let ((temp id))
                                   (set! id (+ id 1))
                                   (cons (cons (car x) temp)
                                         (cons (cadr x) temp)))))
                             rewrite)))
        (rewrite-iter sasm-code '() (map car with-temps) (map cdr with-temps))))
    (iter sasm-code '())))

(define (sasm-const-prop sasm-code)
  (let iter ((cur-code sasm-code)
             (new-code (sasm-const-prop-iter sasm-code)))
    (if (equal? cur-code new-code)
        cur-code
        (iter new-code (sasm-const-prop-iter new-code)))))

(define (sasm-copy-prop sasm-code)
  (let iter ((cur-code sasm-code)
             (new-code (sasm-copy-prop-iter sasm-code)))
    (if (equal? cur-code new-code)
        cur-code
        (iter new-code (sasm-copy-prop-iter new-code)))))

(define (sasm-deadcode-eliminate sasm-code)
  (let iter ((cur-code sasm-code)
             (new-code (sasm-deadcode-eliminate-iter sasm-code)))
    (if (equal? cur-code new-code)
        cur-code
        (iter new-code (sasm-deadcode-eliminate-iter sasm-code)))))

(define (sasm-subexp-eliminate sasm-code)
  (let iter ((cur-code sasm-code)
             (new-code (sasm-subexp-eliminate-iter sasm-code)))
    (if (equal? cur-code new-code)
        cur-code
        (iter new-code (sasm-subexp-eliminate-iter new-code)))))

(define (sasm-dataflow-optimization-pass sasm-code)
  (sasm-deadcode-eliminate-iter
   (sasm-copy-prop-iter
    (sasm-subexp-eliminate-iter
     (sasm-const-prop-iter sasm-code)))))

(define (sasm-dataflow-optimize sasm-code)
  (let ((start (current-seconds)))
    (let iter ((cur-code sasm-code)
               (new-code (sasm-dataflow-optimization-pass sasm-code)))
      (if (or (equal? cur-code new-code)
              (> (- (current-seconds) start) 60))
          cur-code
          (iter new-code (sasm-dataflow-optimization-pass new-code))))))

