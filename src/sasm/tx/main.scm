(need sasm/sasm-tracing)

;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sasm-assemble input)
  (define (in-data-segment? stmt)
    (or (equal? 'global (car stmt))
        (equal? 'label (car stmt))
        (equal? 'data (car stmt))
        (and *sasm-assemble-class-in-data-segment*
             (equal? 'class (car stmt)))))
  (define (preamble-directive? stmt)
    (case (car stmt)
      ((export extern) #t)
      (else #f)))
  (define (invert-predicate predicate?)
    (lambda (x) (not (predicate? x))))
  (define (get-module)
    (let ((module
           (string-append "module"
                          (number->string *sasm-assemble-module-count*))))
      (set! *sasm-assemble-module-count* (+ 1 *sasm-assemble-module-count*))
      module))
  (debug sasm-parse-program)
  (let ((ast (sasm-parse-program input)))
    (if (not ast)
        (error "SASM source parse error"))
    (if (not (sasm-program-analyze-symbols! (get-module) ast))
        (error "One or more SASM errors detected")))
  (debug sasm-assemble-here)
  (let* ((preamble-directives (filter preamble-directive? input))
         (statements (filter (invert-predicate preamble-directive?)
                             input))
         (text-segment (filter (invert-predicate in-data-segment?)
                               statements))
         (data-segment (filter in-data-segment? statements)))
    (debug sasm-assemble-here2)
    (assemble-instructions `((global-preamble)))
    (emit-whitespace)
    (for-each sasm-assemble-statement preamble-directives)
    (emit-whitespace)
    (for-each sasm-assemble-statement text-segment)
    (emit-whitespace)
    (assemble-instructions `((data-preamble)))
    (emit-whitespace)
    (for-each sasm-assemble-statement data-segment)))
