;; util/counting.scm
;; Utility functions for counting and producing counting closures

(define (counter start step)
  (let ((i (- start step)))
    (lambda args
      (set! i (+ i step))
      i)))

(define (zero-counter)
  (counter 0 1))

(define (step-count-list start stop step)
  (define (iter r i)
    (if (or (= i stop) 
            (and (> step 0) (> i stop))
            (and (< step 0) (< i stop)))
        (reverse (if (<= (min start stop) i (max start stop)) 
                     (cons i r) r))
        (iter (cons i r) (+ i step))))
  (if (and (zero? step) (not (= start stop)))
      '()
      (iter '() start)))

(define (zero-count-list n)
  (if (not (and (integer? n) (positive? n)))
      (error "Need a positive integer -- ZERO-COUNT-LIST" n))
  (step-count-list 0 (- n 1) 1))
