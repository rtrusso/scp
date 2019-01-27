;; sasm/fp.scm
;; Floating point routines for SASM 

;;  S EEEEEEEE FFFFFFFFFFFFFFFFFFFFFFF
;;  0 1      8 9                    31

(define (ieee754-decompose fp)
  (let* ((neg (negative? fp))
         (val (abs fp))
         (exponent (let loop ((v val) (e 0))
                     (cond ((or (>= e 128) (<= e -127)) e)
                           ((< 1 v 2) e)
                           ((< v 1) (loop (* v 2) (- e 1)))
                           ((> v 1) (loop (/ v 2) (+ e 1)))
                           (else e))))
         (fraction (/ val (expt 2 exponent))))
    (if (= -127 exponent)
        (list (if neg 1 0) exponent (+ (/ fraction 2) 1))
        (list (if neg 1 0) exponent fraction))))

(define (ieee754-pack-fraction f)
  (let loop ((f (* 2 (- f 1)))
             (i 0)
             (e (expt 2 22))
             (v 0))
    (cond ((> i 22) v)
          ((< f 1) (loop (* 2 f) (+ i 1) (/ e 2) v))
          (else (loop (* 2 (- f 1)) (+ i 1) (/ e 2) (+ v e))))))

(define (ieee754-pack decomp)
  (+ (* (expt 2 31) (list-ref decomp 0))
     (* (expt 2 23) (+ 127 (list-ref decomp 1)))
     (ieee754-pack-fraction (list-ref decomp 2))))

(define (ieee754->integer fp)
  (ieee754-pack (ieee754-decompose fp)))
