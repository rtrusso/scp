(need regex/regex)

(define (generate-lexer description ignored)
  (define (validate a-dfa)
    (or (fast-dfa-validate a-dfa)
        (error "Failed to validate regex DFA"))
    a-dfa)
  (let ((big-regex (cons ': (map (lambda (to-emit regex)
                                   (list ':emit
                                         to-emit
                                         (index-of to-emit equal? (map car description))
                                         regex))
                                 (map car description)
                                 (map cadr description)))))
    (let ((big-dfa (validate (compile-regex big-regex))))
      (list big-dfa ignored))))

(define (write-lexer generated-lexer dfa-symbol entrypoint-symbol port)
  (let ((big-dfa (car generated-lexer)))
    (let ((code `((need regex/dfa)
                  (need regex/genlex)
                  (define ,dfa-symbol ,(fast-dfa-serialize big-dfa))
                  (define ,entrypoint-symbol
                    (let ((main-dfa ,dfa-symbol))
                      (lambda (input-port)
                        (run-generated-lexer main-dfa 
                                             (quote ,(list-ref generated-lexer 1))
                                             input-port)))))))
      (for-each (lambda (code)
                  (write code port)
                  (newline port))
                code))))

(define (write-java-lexer generated-lexer . args)
  (apply write-java-lexer-from-dfa (car generated-lexer) args))

(define (write-java-lexer-from-dfa main-dfa . args )
  (apply fast-dfa-serialize-java main-dfa args))

(define (run-lexer generated-lexer input-port)
  (let ((main-dfa (list-ref generated-lexer 0))
        (ignored-tokens (list-ref generated-lexer 1)))
    (run-generated-lexer main-dfa ignored-tokens input-port)))

(define (run-generated-lexer main-dfa ignored-tokens input-port)
  (define result '())
  (define (iter line column)
    (define (process-match in)
      (let ((value (car in))
            (token (cadr in)))
        (if (not (member token ignored-tokens))
            (set! result (cons (list token value line column)
                               result)))
        (iter (list-ref in 2) (list-ref in 3))))
;    (for-each display (list ";; iter (" line "," column ")\n"))
    (if (not (eof-object? (peek-char input-port)))
        (let ((in (regex-match-port-line-column main-dfa
                                                input-port
                                                line
                                                column)))
          (cond ((eqv? '*lexer-error* (car in))
                 in)
                (else
                 (process-match in))))
        (cons line column)))
;  (display ";; run-generated-lexer\n")
  (let ((res (iter 0 0)))
    (if (and (pair? res)
             (eqv? '*lexer-error* (car res)))
        res
        (reverse (cons (list '$ "" (car res) (cdr res)) result)))))
