(define (sasm-symconst-preprocess-code sasm-code)
  (define (fix-element x)
    (if (and (list? x)
             (= 2 (length x))
             (equal? 'symconst (car x))
             (symbol? (cadr x)))
        (let ((entry (assoc (cadr x) *sasm-assemble-symconst-alist*)))
          (or (and entry `(const ,(cadr entry)))
              (error "Invalid symbolic constant -- " x)))
        (if (and (list? x)
                 (equal? 'result (car x)))
            (map fix-element x)
            x)))
  (map (lambda (base-instruction)
         (let ((instruction (map fix-element base-instruction)))
;           (println "preprocess [" base-instruction "] -> [" instruction)
           instruction))
       sasm-code))

(define (sasm-symconst-alist-append symconsts)
  (set! *sasm-assemble-symconst-alist* (append *sasm-assemble-symconst-alist*
                                               symconsts)))

