;; sasm-regalloc.scm
;; Register allocator for SASM

(need sasm/sasm-tracing)
(need util/list)
(need sasm/sasm-dataflow)
(need sasm/sasm-tx)
(need sasm/sasm-insel)
(need algo/graph)

(define *sasm-enable-regalloc* #f)
(define *sasm-enable-only-regalloc* #f)
(define *sasm-enable-temp-to-reg* #f)
(define *sasm-all-temps-are-locals* #f)
(define *sasm-fold-instruction-tree* #f)
(define *sasm-regalloc-cheap* #f)

(define (sasm-filter-out-lvalues type lvalue-list)
  (filter (lambda (x) (not (equal? (car x) type)))
          lvalue-list))

(define (sasm-intgraph-nodes sasm-code liveness)
  (sasm-filter-out-lvalues
   'local
   (sasm-filter-out-lvalues
    'arg
    (remove-duplicates equal?
                       (accum append
                              '()
                              (append (car liveness) (cadr liveness)))))))

(define (sasm-intgraph-live-records liveness lvalue)
  (filter-map (lambda (live)
                (if (member lvalue live)
                    (sasm-filter-out-lvalues
                     'local
                     (sasm-filter-out-lvalues
                      'arg
                      (subtract-lists equal?
                                      live
                                      (list lvalue))))
                    #f))
              (append (car liveness)
                      (cadr liveness))))

(define (sasm-intgraph-contemporaries sasm-code liveness lvalue)
  (remove-duplicates equal?
                     (accum append
                            '()
                            (sasm-intgraph-live-records liveness lvalue))))

(define (sasm-build-intgraph sasm-code liveness)
  (let ((all-lvalues (sasm-intgraph-nodes sasm-code liveness)))
    (map (lambda (lvalue)
           (cons lvalue
                 (sasm-intgraph-contemporaries sasm-code liveness lvalue)))
         all-lvalues)))

(define (sasm-intgraph-test sasm-code)
  (sasm-build-intgraph sasm-code (sasm-liveness-compute sasm-code)))

(define (sasm-color-temporary-partial partial-graph partial-assignments temporary bank)
  (let* ((adj (graph.neighbors partial-graph temporary))
         (used (remove-duplicates equal? (map (lambda (x)
                                                (let ((entry (assoc x partial-assignments)))
                                                  (if (not entry)
                                                      (begin (display "Partial graph has uncolored nodes: \n\n")
                                                             (display "Current:     ") (display x) (display "\n\n")
                                                             (display "Temporary:   ") (display temporary) (display "\n\n")
                                                             (display "Assignments: ") (display partial-assignments) (display "\n\n")
                                                             (display "Graph:       ") (display partial-graph) (display "\n\n")
                                                             (display "Bank:        ") (display bank) (display "\n\n")
                                                             (error "Register allocation unable to continue\n"))
                                                      (cdr entry))))
                                              adj))))
    (let ((candidates (filter (lambda (x) (not (member x used)))
                              bank)))
      (if (null? candidates)
          #f
          (car candidates)))))

(define (sasm-rewrite-program-with-spills sasm-code spilled-temps)
;  (println ";; Spilling temporaries: " spilled-temps)
  (let ((next-local (- (sasm-local-max sasm-code) 1)))
    (let ((mapping (map (lambda (spill) (cons spill (begin (set! next-local (+ next-local 1))
                                                           next-local)))
                        spilled-temps)))
      (define (rewrite-uses stmt uses)
        (cond ((null? uses)
               (list stmt))
              ((assoc (car uses) mapping)
               (cons `(assign ,(car uses) (local ,(cdr (assoc (car uses) mapping))))
                     (rewrite-uses stmt (cdr uses))))
              (else
               (rewrite-uses stmt (cdr uses)))))
      (define (rewrite-spills stmt)
        (cond ((and (equal? 'assign (car stmt))
                    (assoc (list-ref stmt 1) mapping))
               (list stmt
                     `(assign (local ,(cdr (assoc (list-ref stmt 1) mapping)))
                              ,(list-ref stmt 1))))
              (else
               (rewrite-uses stmt
                             (sasm-liveness-statement-gens stmt)))))
      (sasm-fix-locals
       (apply max (sasm-local-max sasm-code) (map (lambda (x) (+ 1 (cdr x)))
                                                  mapping))
       (accum append
              '()
              (map rewrite-spills sasm-code))))))

(define (sasm-regalloc-rewrite sasm-code assignment)
  (define (rewrite-elem elem)
    (if (and (pair? elem)
             (equal? 'result (car elem)))
        (cons 'result (map rewrite-elem (cdr elem)))
        (let ((entry (assoc elem assignment)))
          (if entry
              (cdr entry)
              elem))))
  (define (rewrite-stmt stmt)
    (map rewrite-elem stmt))
  (map rewrite-stmt sasm-code))

(define (sasm-machine-register-bank)
  (map (lambda (x) `(reg ,(string->symbol (cdr x))))
       (sasm-machine-registers)))
(define (sasm-soft-register-bank)
  (map (lambda (x) `(reg ,(car x)))
       (sasm-machine-registers)))

(define (sasm-system-machine-register-bank)
  (map (lambda (x) `(sys ,(string->symbol (cdr x))))
       (sasm-system-registers)))
(define (sasm-system-soft-register-bank)
  (map (lambda (x) `(sys ,(car x)))
       (sasm-system-registers)))

(define (sasm-particle-is-register? particle)
  (and (list? particle)
       (= 2 (length particle))
       (symbol? (cadr particle))
       (or (eqv? 'reg (car particle))
           (eqv? 'sys (car particle)))))

(define (sasm-simple-regalloc-intermediate sasm-code)
  (let ((regalloc-code (profile "simple-regalloc ~"(sasm-simple-regalloc sasm-code))))
    (profile "regalloc-rewrite ~"
    (sasm-regalloc-rewrite regalloc-code
                           (map cons
                                (append (sasm-machine-register-bank)
                                        (sasm-system-machine-register-bank))
                                (append (sasm-soft-register-bank)
                                        (sasm-system-soft-register-bank)))))))

(define (sasm-simple-regalloc-final sasm-code)
  (sasm-simple-regalloc sasm-code))

(define (sasm-simple-regalloc-final-rewrite sasm-code)
  (sasm-regalloc-rewrite sasm-code
                         (map cons
                              (append (sasm-soft-register-bank)
                                      (sasm-system-soft-register-bank))
                              (append (sasm-machine-register-bank)
                                      (sasm-system-machine-register-bank)))))

;; TODO OPTIMIZE
(define (sasm-simple-regalloc sasm-code)
  (define (get-insig-nodes graph)
    (map car 
         (filter (lambda (adj) (< (length (cdr adj))
                                  (length (sasm-machine-registers))))
                 graph)))
  ;; the 'spill-registers' and 'simplify' functions
  ;; take a parameter 'graph' which is a working copy of the
  ;; main intgraph that is modified during computation.  They work
  ;; mutually, passing this intermediate graph between them until
  ;; they finish the computation.
  (define (spill-registers graph stack spill)
    (let ((non-regs (filter (lambda (x) (not (sasm-particle-is-register? (car x))))
                            graph))
          (regs (filter (lambda (x) (sasm-particle-is-register? (car x)))
                        graph)))
      (cond ((not (null? regs))
             (simplify (graph.remove-node graph (caar regs))
                       (cons (caar regs) stack)
                       spill))
            ((not (null? non-regs))
             (simplify (graph.remove-node graph (caar non-regs))
                       (cons (caar non-regs) stack)
                       (cons (caar non-regs) spill)))
            (else
             (error "Unable to continue register allocation; internal failure in spill-registers -- SASM-SIMPLE-REGALLOC")))))
  (define (simplify graph stack spill)
;    (println ";; SIMPLIFY-ITER " stack spill)
    (if (null? graph)
        (list stack spill)
        (let ((insig (get-insig-nodes graph)))
;          (println ";;   insig: " insig)
          (if (null? insig)
              (spill-registers graph stack spill)
              (simplify (graph.remove-node graph (car insig))
                        (cons (car insig) stack)
                        spill)))))
  ;; the 'select' function takes a parameter 'graph' which is the original,
  ;; unmodified intgraph.  The 'stack' and 'spill' parameters come from
  ;; the results of the simplify/spill computation.
  (define (select graph stack spill)
    (define (select-iter partial-graph stack assignments)
      (if (null? stack)
          (list assignments '())
          (let ((next (car stack))
                (next-graph (graph.rebuild-per-node partial-graph
                                                    graph
                                                    (car stack))))
;            (println ";; selecting " next)
            (if (sasm-particle-is-register? next)
                (select-iter next-graph
                             (cdr stack) 
                             assignments)
                (let ((color (sasm-color-temporary-partial next-graph 
                                                           assignments 
                                                           next 
                                                           (sasm-machine-register-bank))))
;                  (println ";;  colored " next " as " color)
                  (if color
                      (select-iter next-graph
                                   (cdr stack)
                                   (cons (cons next color) assignments))
                      (list '() (list next))))))))
    (select-iter (graph.rebuild-multiple-nodes '() 
                                               graph
                                               (filter (lambda (x) (assoc x graph))
                                                       (sasm-soft-register-bank)))
                 (filter (lambda (x) (not (member x (sasm-soft-register-bank))))
                         stack)
                 (map (lambda (x y) (cons x y))
                      (append (sasm-soft-register-bank) (sasm-system-soft-register-bank))
                      (append (sasm-machine-register-bank) (sasm-system-machine-register-bank)))))
  (define (cycle sasm-code)
;    (println ";; cycle")
    (let* ((liveness (sasm-liveness-compute sasm-code))
           (graph (sasm-build-intgraph sasm-code liveness)))
;      (println ";; graph: " graph)
      (let ((simp (simplify graph '() '())))
;        (println ";; simplify: " simp)
        (let ((sel (select graph (list-ref simp 0) (list-ref simp 1))))
;          (println ";; select: " sel)
          (if (null? (list-ref sel 1))
              (sasm-regalloc-rewrite sasm-code (list-ref sel 0))
              (cycle (sasm-rewrite-program-with-spills sasm-code (list-ref sel 1))))))))
;  (println ";; sasm-insel locals:" (sasm-local-max sasm-code))
  (cycle sasm-code))

(define (sasm-call-insn? sasm-stmt)
  (and (equal? 'perform (car sasm-stmt))
       (or (equal? '(op call) (list-ref sasm-stmt 1))
           (equal? '(op this-call) (list-ref sasm-stmt 1)))))

(define (sasm-canonicalize-temps sasm-code)
;; In SASM, temporary registers have semantics equivalent to
;; locals (i.e. their values are preserved across function call
;; boundaries).  However, for purposes of register allocation,
;; it would be convenient if temporaries provided only the weak
;; guarantee that values are not preserved across function calls.
;;
;; The purpose of this routine is to ensure any temporaries that
;; rely on this guarantee are eliminated.  It does this by transforming
;; them into explicit locals.
  (if *sasm-all-temps-are-locals*
      (sasm-canonicalize-temps-trivially sasm-code)
      (sasm-canonicalize-temps-using-liveness sasm-code)))

(define (sasm-canonicalize-temps-using-liveness sasm-code)
;; The purpose of this routine is to determine, for each temporary
;; used in the specified sasm-code, whether the temp is live across a
;; function-call boundary.  If so, it must be turned into a local
;; variable.
  (let ((liveness (sasm-liveness-compute sasm-code)))
    (define (rewrite-stmt stmt rewrites)
      (map (lambda (exp)
             (let ((entry (assoc exp rewrites)))
               (cond (entry (cdr entry))
                     ((and *sasm-enable-temp-to-reg*
                           (list? exp)
                           (equal? 'temp (car exp)))
                      (list 'reg (cadr exp)))
                     (else exp))))
           stmt))
    (define (rewrite-code sasm-code rewrites)
;      (println "rewrite-code [" rewrites "]")
      (let ((code (map (lambda (stmt)
                         (rewrite-stmt stmt rewrites))
                       sasm-code)))
        (sasm-fix-locals (sasm-local-max code) code)))
    (define (append-bad-temps cur-temps new-temps counter)
      (cond ((null? new-temps)
             (cons cur-temps counter))
            ((assoc (car new-temps) cur-temps)
             (append-bad-temps cur-temps (cdr new-temps) counter))
            (else
             (append-bad-temps (cons (cons (car new-temps)
                                           `(local ,counter))
                                     cur-temps)
                               (cdr new-temps)
                               (+ counter 1)))))
    (let iter ((live-in (car liveness))
               (live-out (list-ref liveness 1))
               (code sasm-code)
               (rewrites '())
               (counter (sasm-local-max sasm-code)))
      (if (null? code)
          (rewrite-code sasm-code rewrites)
          (let ((bad-temps (filter (lambda (x) (equal? 'temp (car x)))
                                   (intersect-lists equal?
                                                    (car live-in)
                                                    (car live-out)))))
            (if (sasm-call-insn? (car code))
                (let ((result (append-bad-temps rewrites bad-temps counter)))
                  (iter (cdr live-in)
                        (cdr live-out)
                        (cdr code)
                        (car result)
                        (cdr result)))
                (iter (cdr live-in)
                      (cdr live-out)
                      (cdr code)
                      rewrites
                      counter)))))))

(define (sasm-canonicalize-temps-trivially sasm-code)
  ;; This routine does the job of canonicalizing temp registers
  ;; trivially by translating *ALL* temp registers into locals.
  (define (rewrite-stmt stmt rewrites)
    (map (lambda (exp)
	   (let ((entry (assoc exp rewrites)))
	     (cond (entry (cdr entry))
		   ((and *sasm-enable-temp-to-reg*
			 (list? exp)
			 (equal? 'temp (car exp)))
		    (list 'reg (cadr exp)))
		   (else exp))))
	 stmt))
  (define (rewrite-code sasm-code rewrites)
;      (println "rewrite-code [" rewrites "]")
    (let ((code (map (lambda (stmt)
                       (rewrite-stmt stmt rewrites))
                     sasm-code)))
      (sasm-fix-locals (sasm-local-max code) code)))
  (define (append-bad-temps cur-temps new-temps counter)
    (cond ((null? new-temps)
           (cons cur-temps counter))
          ((assoc (car new-temps) cur-temps)
           (append-bad-temps cur-temps (cdr new-temps) counter))
          (else
           (append-bad-temps (cons (cons (car new-temps)
                                         `(local ,counter))
                                   cur-temps)
                             (cdr new-temps)
                             (+ counter 1)))))
  (let iter ((code sasm-code)
             (rewrites '())
             (counter (sasm-local-max sasm-code)))
    (if (null? code)
        (rewrite-code sasm-code rewrites)
        (let ((bad-temps (filter (lambda (x) (equal? 'temp (car x)))
                                 (remove-duplicates equal?
                                                    (append (sasm-liveness-statement-gens (car code))
                                                            (sasm-liveness-statement-kills (car code)))))))
          (let ((result (append-bad-temps rewrites bad-temps counter)))
            (iter (cdr code)
                  (car result)
                  (cdr result)))))))

(define (sasm-fold-instruction-tree sasm-code)
  (if *sasm-fold-instruction-tree*
      (sasm-fold-linear sasm-code)
      sasm-code))

(define (sasm-fold-linear sasm-code)
  (let* ((reachdef-soft (sasm-reachdef-compute sasm-code))
         (reachdef (sasm-strong-reachdef-compute reachdef-soft sasm-code))
         (availexp (sasm-availexp-id-compute sasm-code)))
    (define (sasm-rvalue-can-fold? new-code cur-stmt rvalue)
      ;; need to find what definition of rvalue reaches cur-stmt
      ;; need to figure out that the RHS is availexp
      ;; fold that into a result
      (and (sasm-statement-rvalue-def-avail? sasm-code new-code cur-stmt reachdef reachdef-soft availexp rvalue)
           (= 1 (length (sasm-statements-using-rvalue sasm-code rvalue)))))
    (define (sasm-rvalue-fold new-code cur-stmt rvalue)
      (sasm-statement-rvalue-def-fold-rhs sasm-code new-code cur-stmt reachdef availexp rvalue))
    (let iter ((new-code '())
               (cur-code sasm-code))
      (if (null? cur-code)
          (sasm-deadcode-eliminate new-code)
          (iter (append new-code
                        (list (map-rvalues (lambda (rvalue)
                                             (if (sasm-rvalue-can-fold? new-code (car cur-code) rvalue)
                                                 (sasm-rvalue-fold new-code (car cur-code) rvalue)
                                                 rvalue))
                                           (car cur-code))))
                (cdr cur-code))))))

(define (sasm-fold-framevars sasm-code)
  (define n-locals (sasm-local-max sasm-code))
  (define (tail-call-free-reg input)
    (cond ((equal? input '(reg accum))
           '(reg operand))
          ((equal? input '(reg operand))
           '(reg index))
          ((equal? input '(reg index))
           '(reg accum))
          ((equal? input '(reg this))
           (error "invalid tail-call input -- sasm-fold-framevars"))
          (else
           '(reg accum))))
  (define (tail-call-free-reg2 input)
    (tail-call-free-reg (tail-call-free-reg input)))
  (define (codegen-tail-call stmt input n-next-args n-cur-args)
    (let ((reg (tail-call-free-reg input))
          (reg2 (tail-call-free-reg2 input))
          (preserve-this (if (member '(reg this) (sasm-liveness-statement-gens stmt))
                             `((perform (op use-garbage) (reg this)))
                             '())))
      `((assign ,reg (op load-array) (sys frame-pointer) (const 1))
        (push ,reg)
        (assign ,reg2 (op add-pointer) (sys stack-pointer) (const ,n-next-args))
        (assign (sys stack-pointer) (sys frame-pointer))
        (pop (sys frame-pointer))
        (assign (sys stack-pointer) (op add-pointer) (sys stack-pointer) (const ,n-cur-args))
        ,@(let iter ((i 0) (result '()))
            (if (< i n-next-args)
                (iter (+ i 1)
                      (append result
                              `((assign ,reg (op load-array) ,reg2 (const 0))
                                (perform (op store-array) (sys stack-pointer) (const 0) ,reg)
                                (assign ,reg2 (op sub-pointer) ,reg2 (const 1))
                                (assign (sys stack-pointer) (op sub-pointer) (sys stack-pointer) (const 1)))))
                result))
        (assign ,reg (op load-array) ,reg2 (const 0))
        (perform (op store-array) (sys stack-pointer) (const 0) ,reg)
        ,@preserve-this
        (goto ,input))))
  (define (fold-rvalue rvalue)
    (case (car rvalue)
      ((local)
       (if (>= (cadr rvalue) n-locals)
           (error "Bad local index; " rvalue n-locals))
       `(result (op load-array)
                (sys frame-pointer)
                (const ,(- (+ 1 (cadr rvalue))))))
      ((arg)
       `(result (op load-array)
                (sys frame-pointer)
                (const ,(+ 2 (cadr rvalue)))))
      ((result)
       (cons 'result
             (map fold-elem (cdr rvalue))))
      (else rvalue)))
  (define (fold-elem elem)
    (if (symbol? elem)
        elem
        (fold-rvalue elem)))
  (define (fold-assign-exp assign-exp)
    (if (equal? 'op (caar assign-exp))
        `(result
          ,(car assign-exp)
          ,@(map fold-rvalue (cdr assign-exp)))
        (begin (if (not (null? (cdr assign-exp)))
                   (error "malformed assign-exp -- sasm-fold-framevars" assign-exp))
               (fold-rvalue (car assign-exp)))))
  (define (fold-statement stmt)
    (case (car stmt)
      ((assign)
       (case (car (list-ref stmt 1))
         ((local)
          (list 
           `(perform (op store-array)
                     (sys frame-pointer)
                     (const ,(- (+ 1 (cadr (list-ref stmt 1)))))
                     ,(let ((result (fold-assign-exp (cddr stmt))))
                        (if (equal? '(reg (reg this)) result)
                            (error "foo " result stmt)
                            result)))))
         ((arg)
          (list
           `(perform (op store-array)
                     (sys frame-pointer)
                     (const ,(+ 2 (cadr (list-ref stmt 1))))
                     ,(let ((result (fold-assign-exp (cddr stmt))))
                        (if (equal? '(reg (reg this)) result)
                            (error "bar " result stmt)
                            result)))))
         (else
          (list 
           `(assign ,(list-ref stmt 1)
                    ,(fold-assign-exp (cddr stmt)))))))
      ((perform)
       (cond ((equal? '(op push-frame) (list-ref stmt 1))
              `((push (sys frame-pointer))
                (assign (sys frame-pointer) (sys stack-pointer))))
             ((equal? '(op pop-frame) (list-ref stmt 1))
              `((assign (sys stack-pointer) (sys frame-pointer))
                (pop (sys frame-pointer))))
;             ((equal? '(op reserve-locals) (list-ref stmt 1))
;              `((assign (sys stack-pointer) (op sub) (sys stack-pointer) ,(list-ref stmt 2))))
             ((equal? '(op tail-call) (list-ref stmt 1))
              (codegen-tail-call stmt (list-ref stmt 2) (cadr (list-ref stmt 3)) (cadr (list-ref stmt 4))))
             ((equal? '(op this-call) (list-ref stmt 1))
              (list (map fold-elem `(,(car stmt) (op call) ,@(cddr stmt)))))
             (else
              (list (map fold-elem stmt)))))
      (else
       (list (map fold-elem stmt)))))
;  (println ";; sasm-fold-framevars max locals " (sasm-local-max sasm-code))
  (accum append '() (map fold-statement sasm-code)))

(define (sasm-regalloc-fake-soft sasm-code)
  (sasm-fold-framevars
   (sasm-fold-instruction-tree
    (sasm-canonicalize-temps sasm-code))))

(define (sasm-regalloc-fake sasm-code)
  (sasm-regalloc-rewrite
   (sasm-regalloc-fake-soft sasm-code)
   (map cons (sasm-soft-register-bank) (sasm-machine-register-bank))))

(define (sasm-fold-framevars-cheap sasm-code)
  (define n-locals (sasm-local-max sasm-code))
  (define (fold-rvalue rvalue)
    (case (car rvalue)
      ((local)
       (if (>= (cadr rvalue) n-locals)
           (error "Bad local index; " rvalue n-locals))
       `(result (op load-array)
                (sys frame-pointer)
                (const ,(- (+ 1 (cadr rvalue))))))
      ((arg)
       `(result (op load-array)
                (sys frame-pointer)
                (const ,(+ 2 (cadr rvalue)))))
      (else rvalue)))
  (define (fold-elem elem)
    (if (symbol? elem)
        elem
        (fold-rvalue elem)))
  (define (fold-assign-exp assign-exp)
    (if (equal? 'op (caar assign-exp))
        `(,(car assign-exp)
          ,@(map fold-rvalue (cdr assign-exp)))
        (begin (if (not (null? (cdr assign-exp)))
                   (error "malformed assign-exp -- sasm-fold-framevars-cheap" assign-exp))
               (let ((r-value (fold-rvalue (car assign-exp))))
                 (if (and (list? r-value)
                          (eqv? 'result (car r-value)))
                     (cdr r-value)
                     (list r-value))))))
;               (list (fold-rvalue (car assign-exp))))))
  (define (fold-statement stmt)
    (case (car stmt)
      ((assign)
       (case (car (list-ref stmt 1))
         ((local)
          (list
           `(perform (op store-array)
                     (sys frame-pointer)
                     (const ,(- (+ 1 (cadr (list-ref stmt 1)))))
                     ,@(let ((result (fold-assign-exp (cddr stmt))))
                         (if (equal? '(reg (reg this)) result)
                             (error "foo " result stmt)
                             result)))))
         ((arg)
          (list
           `(perform (op store-array)
                     (sys frame-pointer)
                     (const ,(+ 2 (cadr (list-ref stmt 1))))
                     ,@(let ((result (fold-assign-exp (cddr stmt))))
                         (if (equal? '(reg (reg this)) result)
                             (error "bar " result stmt)
                             result)))))
         (else
          (list
           `(assign ,(list-ref stmt 1)
                    ,@(fold-assign-exp (cddr stmt)))))))
      ((perform)
       (cond ((equal? '(op push-frame) (list-ref stmt 1))
              `((push (sys frame-pointer))
                (assign (sys frame-pointer) (sys stack-pointer))))
             ((equal? '(op pop-frame) (list-ref stmt 1))
              `((assign (sys stack-pointer) (sys frame-pointer))
                (pop (sys frame-pointer))))
;             ((equal? '(op reserve-locals) (list-ref stmt 1))
;              `((assign (sys stack-pointer) (op sub) (sys stack-pointer) ,(list-ref stmt 2))))
             ;; ((equal? '(op tail-call) (list-ref stmt 1))
             ;;  (codegen-tail-call stmt (list-ref stmt 2) (cadr (list-ref stmt 3)) (cadr (list-ref stmt 4))))
             ;; ((equal? '(op this-call) (list-ref stmt 1))
             ;;  (list (map fold-elem `(,(car stmt) (op call) ,@(cddr stmt)))))
             (else
              (list (map fold-elem stmt)))))
      (else
       (list (map fold-elem stmt)))))
;  (println ";; sasm-fold-framevars-cheap max locals " (sasm-local-max sasm-code))
  (accum append '() (map fold-statement sasm-code)))

(define (sasm-regalloc-cheap sasm-code)
  (sasm-regalloc-rewrite
   (sasm-fold-framevars-cheap sasm-code)
   (map cons (sasm-soft-register-bank) (sasm-machine-register-bank))))

(define *before* '())
(define *after* '())

(define (sasm-code-has-temps-or-locals? sasm-code)
  (define (temp-or-local? x)
    (or (eqv? 'temp (car x))
        (eqv? 'local (car x))))
  (define (stmt-has-temp-or-local? stmt)
    (or (any? temp-or-local? (sasm-liveness-statement-gens stmt))
        (any? temp-or-local? (sasm-liveness-statement-kills stmt))))
  (if (null? sasm-code)
      #f
      (or (stmt-has-temp-or-local? (car sasm-code))
          (sasm-code-has-temps-or-locals? (cdr sasm-code)))))

(define (sasm-regalloc-transform sasm-code)
  (define (first-pass sasm-code)
    (profile
     "regalloc-intermediate ~"
    (sasm-simple-regalloc-intermediate
     (profile
      "insel ~"
     (sasm-insel
      (profile
       "fold-framevars ~"
      (sasm-fold-framevars
       (profile
        "fold-tree ~"
       (sasm-fold-instruction-tree
        (profile
         "canon-temps ~"
         (sasm-canonicalize-temps sasm-code)))))))))))

  (define (second-pass sasm-code)
    (profile
     "regalloc-intermediate2 ~"
    (sasm-simple-regalloc-intermediate
     (profile
      "insel2 ~"
     (sasm-insel
      (profile
       "fold-framevars2 ~"
      (sasm-fold-framevars sasm-code)))))))

  (define (final-pass sasm-code)
    (profile "final-pass ~"(sasm-simple-regalloc-final sasm-code)))

;  (let loop ((rewrite (first-pass sasm-code)))
;    (let ((next (second-pass rewrite)))
;      (if (equal? rewrite next)
;          (final-pass next)
;          (loop next)))))

  (profile
   "sasm-regalloc ~"
  (let loop ((regalloc-code (profile "first-pass ~" (first-pass sasm-code))))
    (if (not (sasm-code-has-temps-or-locals? regalloc-code))
        (sasm-simple-regalloc-final-rewrite regalloc-code)
        (loop (profile "second-pass ~" (second-pass regalloc-code))))))
)
