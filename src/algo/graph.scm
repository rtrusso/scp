(define (graph-predecessors graph node)
  (filter-map (lambda (adj)
                (and (member node (cdr adj))
                     (car adj)))
              graph))

(define (graph-successors graph node)
  (let ((entry (assoc node graph)))
    (if (not entry)
        (error "Invalid graph node -- graph-successors")
        (cdr entry))))

(define (graph-elements graph)
  (map car graph))

(define (graph.neighbors graph node)
  (let ((l (assoc node graph)))
    (if l
        (cdr l)
        '())))

(define (graph.nodes graph)
  (map car graph))

(define (println . args)
  (for-each display args) (newline))

(define (graph.unify graph)
  (map (lambda (node)
         (cons node
               (remove-duplicates 
                equal?
                (append (graph.neighbors graph node)
                        (filter (lambda (parent)
                                  (member node (graph.neighbors graph
                                                                parent)))
                                (graph.nodes graph))))))
       (graph.nodes graph)))

(define (graph.produce-coloring g)
  (let ((graph (graph.unify g))
        (nodes (graph.nodes g)))
    (let ((numbers (map (lambda (node) (cons node 0)) nodes)))
      (define (num node)
        (cdr (assoc node numbers)))
      (define (num! node num)
        (set-cdr! (assoc node numbers) num))
      (define (smallest-not-in list)
        (define (iter i)
          (if (member i list)
              (iter (+ i 1))
              i))
        (iter 1))
      (define (dfs node)
        (if (zero? (num node))
            (begin
              (num! node
                    (smallest-not-in (map num (graph.neighbors graph node))))
              (for-each dfs (graph.neighbors graph node))
              (println node "->" (num node))
              (num node))
            (num node)))
      (cond ((null? nodes) 0)
            (else (dfs (car nodes)) numbers)))))

(define (graph.remove-node graph node)
  (filter (lambda (adj) (not (equal? (car adj) node)))
          (map (lambda (adj)
                 (cons (car adj)
                       (filter (lambda (x) (not (equal? x node)))
                               (cdr adj))))
               graph)))

(define (graph.remove-multiple-nodes graph nodes)
  (if (null? nodes)
      graph
      (graph.remove-multiple-nodes (graph.remove-node graph (car nodes))
                                   (cdr nodes))))

(define (graph.rebuild-per-node cur-graph orig-graph node)
  (let ((cur-nodes (map car cur-graph)))
    (if (member node cur-nodes)
        (error "node is already present in rebuilt graph -- graph.rebuild-per-node"))
    (cons (cons node
                (filter (lambda (x) (member x cur-nodes))
                        (cdr (assoc node orig-graph))))
          (map (lambda (adj)
                 (cons (car adj)
                       (if (member node (cdr (assoc (car adj) orig-graph)))
                           (cons node (cdr adj))
                           (cdr adj))))
               cur-graph))))

(define (graph.rebuild-multiple-nodes cur-graph orig-graph nodes)
  (if (null? nodes)
      cur-graph
      (graph.rebuild-multiple-nodes (graph.rebuild-per-node cur-graph orig-graph (car nodes))
                                    orig-graph
                                    (cdr nodes))))

(define (graph.equal? graph-a graph-b)
  (and (list-set=? equal?
                   (map car graph-a)
                   (map car graph-b))
       (let iter ((adj-a graph-a))
         (if (null? adj-a)
             #t
             (and (list-set=? equal?
                              (cdr (car adj-a))
                              (cdr (assoc (car (car adj-a)) graph-b)))
                  (iter (cdr adj-a)))))))

(define (graph.chromatic-number graph)
  (apply max (map cdr (graph.produce-coloring graph))))

(define g (list '(guess t1 t2 t3 t4 t5 x)
                '(t1 guess x)
                '(t2 guess x)
                '(t3 guess x)
                '(t4 guess x)
                '(t5 guess x)
                '(x guess t1 t2 t3 t4 t5)))

(define t (list '(a b)
                '(b c)
                '(c a)))

