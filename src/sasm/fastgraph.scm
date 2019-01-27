;; sasm/fastgraph.scm
;; Fast graph primitives

(need util/counting)
(need util/matrix)
(need util/list)

;; fast graph-processing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graph->adj-matrix graph =?)
  (let ((elements (graph-elements graph))
        (adj-matrix (square-matrix (length graph) #f)))
    (define (index obj) (index-of obj =? elements))
    (for-each (lambda (adj)
                (let ((parent (index (car adj))))
                  (for-each (lambda (child)
                              (matrix-set! adj-matrix
                                           parent
                                           (index child)
                                           #t))
                            (cdr adj))))
              graph)
    adj-matrix))
