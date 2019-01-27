;; algo/heapsort.scm
;; An implementation of the textbook heapsort algorithm.


;; Given the index of a vector element, get the index of the left element.
(define (heapsort-left-child i)
  (+ (* 2 i) 1))


;; Given the index of a vector element, get the index of the right element.
(define (heapsort-right-child i)
  (+ (* 2 i) 2))


;; Given the index of a vector element, get the index of the parent element.
(define (heapsort-parent i)
  (quotient (- i 1) 2))


(define (heapsort-heapify! vec n lt?)

  ;; Following are a few convenience functions to make the code shorter.
  (define (@ i)
    (vector-ref vec i))

  (define (@= i v)
    (vector-set! vec i v))

  (define (swap i j)
    (let ((a (@ i)))
      (@= i (@ j))
      (@= j a)))

  (define (parent i)
    (heapsort-parent i))

  (define (left-child i)
    (heapsort-left-child i))

  (define (right-child i)
    (heapsort-right-child i))

  (define (bubble-down! i)
    (let ((left (left-child i))
          (right (right-child i)))
      (cond ((>= i n) 
             vec)
            ((and (< left n) (lt? (@ i) (@ left)))
             (begin (swap i left)
                    (bubble-down! left)))
            ((and (< right n) (lt? (@ i) (@ right)))
             (begin (swap i right)
                    (bubble-down! right)))
            (else
             vec))))

  ;; Perform the textbook heapsort "bubble up" operation - i.e. move the
  ;; element at index i up to its proper position in the heap.
  (define (bubble-up! i)
    (let ((p (parent i)))
      (cond ((= i (parent i)) vec)
            ((lt? (@ i) (@ p))
             vec)
            (else
             (swap i p)
             (bubble-up! p)))))

  (define (check-heap i)
    (cond ((>= i n) #t)
          ((and (or (>= (left-child i) n)
                    (lt? (@ (left-child i)) (@ i)))
                (or (>= (right-child i) n)
                    (lt? (@ (right-child i)) (@ i))))
           (and (check-heap (left-child i))
                (check-heap (right-child i))))
          (else
           #f)))

  (let loop ((idx 0))
    (if (< idx n)
        (begin (heapsort-bubble-up! idx)
               (loop (+ idx 1))))))
