(define (sasm-read-port context input-port)
  (let loop ((stmt (read input-port))
             (input '()))
;    (println "sasm-read-port read[" stmt "]")
    (cond ((eof-object? stmt)
           input)
          ((equal? 'include (car stmt))
           (loop (read input-port)
                 (append input
                         (sasm-read-file (build-path context
                                                     (cadr stmt))))))
          (else 
           (loop (read input-port)
                 (append input (list stmt)))))))

(define (sasm-read-file file)
  (call-with-input-file file 
    (lambda (port) 
      (sasm-read-port (sasm-assemble-file-context file) 
                      port))))

