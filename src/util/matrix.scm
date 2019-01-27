;; util/matrix.scm
;; Vector and matrix utility functions

(define (vector-map! v func)
  (let loop ((i 0))
    (if (< i (vector-length v))
        (begin (vector-set! v (func i (vector-ref v i)))
               (loop (+ i 1)))
        v)))

(define (square-matrix n val)
  (let ((rows (make-vector n)))
    (vector-map! rows (lambda (i x) (make-vector n val)))))

(define (matrix-ref m row col)
  (vector-ref (vector-ref m row) col))

(define (matrix-set! m row col val)
  (vector-set! (vector-ref m row) col val))
