;; sasm/fastset.scm
;;
;; This was originally a relatively "fast" set primitives in terms of
;; arbitrary-precision bitwise integer arithmetic.
;;
;; However, the arbitrary-precision bitwise integer arithmetic was
;; part of a specific and now defunct scheme implementation that is no
;; longer a good dependency. Therefore the code was rewritten in a
;; much slower fashion using strings to implement a poor version of
;; arbitrary-precision bitwise arithmetic.

(define (fastset-importer symbolic-set)
  (let ((len (length symbolic-set)))
    (vector
     (lambda () (make-string len #\0))
     (lambda (symbol)
       (let ((match (member symbol symbolic-set)))
         (if (not match)
             (error "Unrecognized symbol -- fastset" symbol symbolic-set))
         (- len (length match)))))))

(define (fastset-exporter importer symbolic-set)
  (define (get-index symbol)
    ((vector-ref importer 1) symbol))
  (lambda (fastset)
    (let loop ((symbols symbolic-set)
               (result '()))
      (if (null? symbols)
          (reverse result)
          (let ((sym (car symbols)))
            (if (char=? (string-ref fastset (get-index sym)) #\1)
                (loop (cdr symbols) (cons sym result))
                (loop (cdr symbols) result)))))))

(define (fastset-collapse importer symset)
  (define (make-fastset)
    ((vector-ref importer 0)))
  (define (get-index symbol)
    ((vector-ref importer 1) symbol))
  (let loop ((result (make-fastset))
             (rest symset))
    (if (null? rest)
        result
        (begin (string-set! result (get-index (car rest)) #\1)
               (loop result (cdr rest))))))

(define (fastset-expand exporter fastset)
  (if (not (string? fastset))
      (error "fastset-expand, not a fastset " fastset))
  (exporter fastset))

(define (fastset-union fastset-a fastset-b)
  (define (union a b)
    (if (or (char=? a #\1)
            (char=? b #\1))
        #\1
        #\0))
  (if (not (string? fastset-a))
      (error "fastset-union, fastset-a not a fastset " fastset-a))
  (if (not (string? fastset-b))
      (error "fastset-union, fastset-b not a fastset " fastset-b))
  (let* ((len (string-length fastset-a))
         (result (make-string len #\0)))
    (let loop ((index 0))
      (if (>= index len)
          result
          (begin (string-set! result
                              index
                              (union (string-ref fastset-a index)
                                     (string-ref fastset-b index)))
                 (loop (+ 1 index)))))))
;    (bitwise-ior fastset-a fastset-b))

(define (fastset-subtract fastset-a fastset-b)
  (define (subtract a b)
    (if (and (char=? a #\1)
             (char=? b #\0))
        #\1
        #\0))
  (if (not (string? fastset-a))
      (error "fastset-subtract, fastset-a not a fastset " fastset-a))
  (if (not (string? fastset-b))
      (error "fastset-subtract, fastset-b not a fastset " fastset-b))
  (let* ((len (string-length fastset-a))
         (result (make-string len #\0)))
    (let loop ((index 0))
      (if (>= index len)
          result
          (begin (string-set! result
                              index
                              (subtract (string-ref fastset-a index)
                                        (string-ref fastset-b index)))
                 (loop (+ 1 index)))))))
;  (bitwise-and fastset-a (bitwise-not fastset-b)))

(define (fastset-equal? fastset-a fastset-b)
  (if (not (string? fastset-a))
      (error "fastset-equal?, fastset-a not a fastset " fastset-a))
  (if (not (string? fastset-b))
      (error "fastset-equal?, fastset-b not a fastset " fastset-b))
  (string=? fastset-a fastset-b))

; unused
;(define (fastset-intersect fastset-a fastset-b)
;  (bitwise-and fastset-a fastset-b))

;(define (fastset-subset? fastset-a fastset-b)
;  (= 0 (fastset-subtract fastset-a fastset-b)))

