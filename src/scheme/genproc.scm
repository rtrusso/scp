(define-syntax declare-genproc
  (syntax-rules ()
    ((_ <func-name>)
     (define (<func-name> . args)
       (genproc-apply '<func-name> args)))))

(define-syntax define-genproc
  (syntax-rules ()
    ((_ (<func-name> (<arg-name> <arg-type>) ...)
        <body> ...)
     (let ((<func-name> (lambda (<arg-name> ...) <body> ...)))
       (genproc-install '<func-name> 
                        '(<arg-type> ...)
                        <func-name>)))
    ((_ (<func-name> <keyword> (<arg-name> <arg-type>) ...)
        <body> ...)
     (let ((<keyword> (lambda (<arg-name> ...) <body> ...)))
       (genproc-install '<func-name> 
                        '(<arg-type> ...)
                        <keyword>)))))

(define *genproc-table* '())

(define (genproc-install func-name signature impl)
  (define (genproc-record func-name signature impl)
    (list func-name signature impl))
  (set! *genproc-table*
        (cons (genproc-record func-name signature impl)
              *genproc-table*)))

(define (genproc-lookup func-name arg-types)
  (define (iter tab)
    (if (null? tab)
        #f
        (let ((rec (car tab))
              (rest (cdr tab)))
          (if (and (equal? (car rec) func-name)
                   (equal? (cadr rec) arg-types))
              (caddr rec)
              (iter rest)))))
  (iter *genproc-table*))

(define (genproc-apply func-name args)
  (let ((impl (genproc-lookup func-name (map get-tag args))))
    (if impl
        (apply impl args)
        (error "Unable to find suitable generic procedure -- GENPROC-APPLY"
               func-name (map get-tag args)))))

(define (genproc-invoke func-name . args)
  (genproc-apply func-name args))

