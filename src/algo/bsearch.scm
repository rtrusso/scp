;; bsearch.scm
;;
;; An implementation of binary search.

(define (binary-search vector vector-ref vector-length less-than? value)
  (define (iter low high)
    (if (> low high)
        #f
        (let ((m (quotient (+ low high) 2)))
          (cond ((less-than? (vector-ref vector m) value)
                 (iter (+ m 1) high))
                ((less-than? value (vector-ref vector m))
                 (iter low (- m 1)))
                (else m)))))
  (iter 0 (- (vector-length vector) 1)))

(define (make-binary-searcher vector-ref vector-length less-than?)
  (lambda (vector value)
    (binary-search vector vector-ref vector-length less-than? value)))

(define (binary-search-vector vector less-than? value)
  (binary-search vector vector-ref vector-length less-than? value))
