(need sasm/sasm-tracing)

(define (sasm-parse-by-case expression . cases)
  (define (iter cases)
    (debug sasm-parse-by-case-iter cases)
    (if (null? cases)
        #f
        (let* ((current (begin (debug sasm-parse-by-case-current-case (car cases))
                               (car cases)))
               (result ((car cases) expression)))
          (debug sasm-parse-by-case-result result)
          (or result
              (iter (cdr cases))))))
  (debug sasm-parse-by-case expression)
  (iter cases))

