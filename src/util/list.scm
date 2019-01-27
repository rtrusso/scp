;; util/list.scm
;; List utility functions

(need util/counting)

(define (for-each-idx proc list)
  (let loop ((i 0)
             (l list))
    (if (not (null? l))
        (begin (proc (car l) i)
               (loop (+ i 1) (cdr l))))))

(define (terminal-pair l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (terminal-pair (cdr l)))))

(define (list-prefix l n)
  (if (or (zero? n)
          (null? l))
      '()
      (cons (car l)
            (list-prefix (cdr l) (- n 1)))))

(define (memq-reverse a b)
  (define (iter result rest)
    (cond ((null? rest) #f)
          ((eq? a (car rest)) (cons (car rest) result))
          (else (iter (cons (car rest) result) (cdr rest)))))
  (iter '() b))

(define (filter good? list)
  (define (iter result list)
    (if (null? list)
        (reverse result)
        (iter (if (good? (car list))
                  (cons (car list) result)
                  result)
              (cdr list))))
  (iter '() list))

(define (filter-map good list)
  (define (iter result list)
    (if (null? list)
        (reverse result)
        (let ((t (good (car list))))
          (iter (if t (cons t result) result) (cdr list)))))
  (iter '() list))

(define (list-find-first ok? list)
  (cond ((null? list) list)
        ((ok? (car list)) (car list))
        (else (list-find-first ok? (cdr list)))))

(define (accumulate op base list)
  (if (null? list)
      base
      (op (car list)
          (accumulate op base (cdr list)))))

(define (accum op base list)
  (define (iter res list)
    (if (null? list)
        res
        (iter (op res (car list))
              (cdr list))))
  (iter base list))

(define (accum-map op base map list)
  (define (iter res list)
    (if (null? list)
        res
        (iter (op res (map (car list)))
              (cdr list))))
  (iter base list))

(define (accum-implicit op list)
  (define (iter res list)
    (cond ((null? list) res)
          ((null? (cdr list)) (op res (car list)))
          (else (iter (op res (car list)) (cdr list)))))
  (if (not (null? list))
      (iter (car list) (cdr list))))

(define (flatten-tree l)
  (cond ((null? l) l)
        ((pair? l) (apply append (map flatten-tree l)))
        (else (list l))))

(define (treemap f tree)
  (define (iter result list)
    (cond ((null? list) (reverse result))
          ((pair? (car list)) (iter (cons (treemap f (car list)) result)
                                    (cdr list)))
          (else (iter (cons (f (car list)) result)
                      (cdr list)))))
  (iter '() tree))

(define (all? p? list)
  (accum (lambda (x y) (and x y)) #t (map p? list)))

(define (none? p? list)
  (null? (filter p? list)))

(define (any? p? list)
  (not (none? p? list)))

(define (or-proc list)
  (cond ((null? list) #f)
        ((car list) (car list))
        (else (or-proc (cdr list)))))

(define (cons-end a l)
  (reverse (cons a (reverse l))))

(define (list-contains? =? elem elem-list)
  (and (not (null? elem-list))
       (or (=? elem (car elem-list))
           (list-contains? =? elem (cdr elem-list)))))

(define (remove-duplicates . args)
  (define (remove =? l)
    (define (iter r l)
      (cond ((null? l) (reverse r))
            ((list-contains? =? (car l) r) (iter r (cdr l)))
            (else (iter (cons (car l) r) (cdr l)))))
    (iter '() l))
  (case (length args)
    ((2) (remove (car args) (cadr args)))
    ((1) (remove equal? (car args)))
    (else (error "Invalid arguments -- REMOVE-DUPLICATES" args))))

(define (depth-first-search =? start get-children)
  (define (reach visited current)
    (define (visited? x)
      (or (=? x current) (list-contains? =? x visited)))
    (cons current
          (apply append
                 (map (lambda (node)
                        (if (visited? node) '() (reach (cons current visited)
                                                       node)))
                      (remove-duplicates =? (get-children current))))))
  (reach '() start))

(define (intersect-lists =? list-a list-b)
  (filter (lambda (obj) (list-contains? =? obj list-b))
          list-a))

(define (list-intersecter =?)
  (lambda (list-a list-b)
    (intersect-lists =? list-a list-b)))

(define (union-lists =? list-a list-b)
  (remove-duplicates =? (append list-a list-b)))

(define (list-unioner =?)
  (lambda (list-a list-b)
    (union-lists =? list-a list-b)))

(define (subtract-lists =? list-a list-b)
  (filter (lambda (obj) (not (list-contains? =? obj list-b)))
	  list-a))

(define (list-subtractor =?)
  (lambda (list-a list-b)
    (subtract-lists =? list-a list-b)))

(define (list-set=? =? set-a set-b)
  (and (= (length set-a) (length (intersect-lists =? set-a set-b)))
       (= (length set-b) (length (intersect-lists =? set-a set-b)))))

(define (list-set-comparator =?)
  (lambda (set-a set-b)
    (list-set=? =? set-a set-b)))

(define (list-subset? =? list-a list-b)
  (cond ((null? list-a) #t)
        ((list-contains? =? (car list-a) list-b) (list-subset? =?
                                                               (cdr list-a)
                                                               list-b))
        (else #f)))

(define (list-or list)
  (cond ((null? list) #f)
        ((car list) => (lambda (x) x))
        (else (list-or (cdr list)))))

(define (list-and list)
  (cond ((null? list) #t)
        ((car list) (list-and (cdr list)))
        (else #f)))

(define (length* l)
  (cond ((null? l) 0)
        ((pair? l) (+ 1 (length* (cdr l))))
        (else 1)))

(define (index-of-from obj =? list start)
  (cond ((null? list) #f)
        ((=? obj (car list)) start)
        (else (index-of-from obj =? (cdr list) (+ start 1)))))

(define (index-of obj =? list)
  (index-of-from obj =? list 0))

;; Assuming f returns lists, maps f over list, then appends the results of
;; f into a single list
(define (append-map f list)
  (apply append (map f list)))

(define (splice list-a list-b)
  (apply append (map list list-a list-b)))

;; (define (unsplice list-a)
;;   (if (null? list-a)
;;       (list '() '())
;;       (list
;;        (map (lambda (n) (list-ref list-a n))
;;             (filter even? (zero-count-list (length list-a))))
;;        (map (lambda (n) (list-ref list-a n))
;;             (filter odd? (zero-count-list (length list-a)))))))

(define (take n l)
  (define (iter r i l)
    (if (= 0 i) (reverse r) (iter (cons (car l) r) (- i 1) (cdr l))))
  (if (and (list? l) (not (negative? n)))
      (iter '() n l)
      (error "Expecting a list and a non-negative integer -- TAKE" n l)))

(define (n-cdr n l)
  (cond ((negative? n) (error "Need a non-negative integer -- N-CDR!" n l))
        ((zero? n) l)
        ((null? l) (error "Index out of range -- N-CDR!" n l))
        (else (n-cdr (- n 1) (cdr l)))))

(define (improper->proper list)
  (cond ((pair? list) (cons (car list)
                            (improper->proper (cdr list))))
        ((null? list) (error "improper->proper given proper list"))
        (else (cons list '()))))

(define (proper->improper list)
  (cond ((null? list) 
         (error "proper->improper given null list"))
        ((not (pair? list))
         (error "proper->improper given improper list"))
        ((null? (cdr list)) 
         (car list))
        (else 
         (cons (car list) (proper->improper (cdr list))))))

(define (assoc-flat-opt symbol a-list)
  (cond ((null? a-list) #f)
        ((equal? symbol (car a-list)) #t)
        ((equal? symbol (caar a-list)) (car a-list))
        (else (assoc-flat-opt symbol (cdr a-list)))))

;; List builder //////////////////////////////////////////////////////

(define (list-builder)
  (cons '() '()))

(define (list-build! list-builder atom)
  (if (null? (car list-builder))
      (begin (set-car! list-builder (cons atom '()))
             (set-cdr! list-builder (car list-builder)))
      (begin (set-cdr! (cdr list-builder)
                       (cons atom '()))
             (set-cdr! list-builder (cddr list-builder)))))

(define (list-inner list-builder)
  (car list-builder))

(define (list-finalize! list-builder)
  (let ((l (car list-builder)))
    (set-car! list-builder '())
    (set-cdr! list-builder '())
    l))
