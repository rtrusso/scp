;; util/vector.scm
;; Vector utility functions

(define (vector-map func vec)
  (let* ((n (vector-length vec))
         (new-vec (make-vector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin (vector-set! new-vec i (func (vector-ref vec i)))
                 (loop (+ i 1)))
          new-vec))))

(define (vector-accum op base vec)
  (let ((n (vector-length vec)))
    (let loop ((i 0) (val base))
      (if (< i n)
          (loop (+ i 1) (op val (vector-ref vec i)))
          val))))

(define (vector-map-accum op base map vec)
  (let ((n (vector-length vec)))
    (let loop ((i 0) (val base))
      (if (< i n)
          (loop (+ i 1) (op val (map (vector-ref vec i))))
          val))))

(define (vector-copy-up! src index-start index-end dst dst-offset)
  (if (< index-start index-end)
      (begin (vector-set! dst 
                          dst-offset
                          (vector-ref src index-start))
             (vector-copy-up! src 
                              (+ index-start 1)
                              index-end
                              dst
                              (+ dst-offset 1)))))

(define (vector-copy-down! src index-start index-end dst dst-offset)
  (if (>= index-end index-start)
      (begin (vector-set! dst
                          dst-offset
                          (vector-ref src index-end))
             (vector-copy-down! src 
                                index-start
                                (- index-end 1)
                                dst
                                (- dst-offset 1)))))

(define (vector-copy! src index-start index-end dst dst-offset)
  (cond ((not (eq? src dst))
         (vector-copy-up! src index-start index-end dst dst-offset))
        ((<= index-start dst-offset index-end)
         (vector-copy-down! src 
                            index-start 
                            (- index-end 1)
                            dst
                            (+ dst-offset (- index-end index-start 1))))
        (else (vector-copy-up! src index-start index-end dst dst-offset))))

(define (vector-copy-full! src dst)
  (vector-copy! src 0 (vector-length src) dst 0))

(define (vector-insert v i obj)
  (let ((res (make-vector (+ (vector-length v) 1))))
    (define (copy i j vi)
      (if (< i j)
	  (begin (vector-set! res i (vector-ref v vi))
		 (copy (+ i 1) j (+ vi 1)))))
    (copy 0 i 0)
    (vector-set! res i obj)
    (copy (+ i 1) (vector-length res) i)
    res))
