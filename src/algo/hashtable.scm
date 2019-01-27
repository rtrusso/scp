;; hashtable.scm
;; A hashtable implementation that sucks.

(need algo/quicksort)
(need algo/bsearch)

(define (hash-lookup hash key)
  (hash key))

(define (hash-insert! hash key value)
  (hash key value))

(define (make-hashtable key-<)
  (let ((hash (make-vector 100))
        (size 0)
        (sorted? #t))

    (define (ensure-capacity! required)
      (if (> required (vector-length hash))
          (let ((new-hash (make-vector (+ required (vector-length hash)))))
            (vector-copy! hash 0 size new-hash 0)
            (set! hash new-hash))))

    (define (hash-< x y)
      (key-< (car x) (car y)))

    (define (ensure-sorted!)
      (if (not sorted?)
          (begin (quicksort-vector! hash 0 size hash-<)
                 (set! sorted? #t))))

    (define (insert! key value)
      (ensure-capacity! (+ size 1))
      (vector-set! hash size (cons key value))
      (set! size (+ size 1))
      (if (and sorted?
               (> size 1)
               (not (key-< (car (vector-ref hash (- size 2))) key)))
          (set! sorted? #f)))

    (define (lookup key)
      (ensure-sorted!)
      (let ((index (binary-search hash
                                  vector-ref
                                  (lambda (x) size)
                                  hash-<
                                  (cons key '()))))
        (and index (vector-ref hash index))))

    (lambda (key . value)
      (if (null? value)
          (lookup key)
          (insert! key (car value))))))

      
