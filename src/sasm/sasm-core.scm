;; sasm-core.scm
;; SASM transform routines

(need pat/pat)
(need sasm/sasm-dataflow)
(need sasm/sasm-regalloc)
(need sasm/sasm-parse)
(need sasm/sasm-analyze)

(define *sasm-transform-function-count* 0)
(define *sasm-output-progress* #f)

(define (sasm-remove-directives sasm)
  (filter (lambda (x) (not (equal? 'class-info (car x))))
          sasm))

(define *sasm-transform-code-module-count* 0)

(define (sasm-transform-code sasm-program)
  (define (get-module)
    (let ((module
           (string-append "module"
                          (number->string *sasm-transform-code-module-count*))))
      (set! *sasm-transform-code-module-count* (+ 1 *sasm-transform-code-module-count*))
      module))
  (let ((ast (sasm-parse-program sasm-program)))
    (if (not ast)
        (error "SASM source parse error"))
    (if (not (sasm-program-analyze-symbols! (get-module) ast))
        (error "One or more SASM errors detected")))
  (map sasm-transform-statement 
       (sasm-remove-directives sasm-program)))

(define (sasm-transform-port port)
  (sasm-transform-code (sasm-read-port (sasm-assemble-interactive-context) port)))

(define (sasm-transform-file file-name)
  (if *sasm-output-progress*
      (begin (display ";; starting scan of input file "
                      (current-output-port))
             (display file-name
                      (current-output-port))
             (newline (current-output-port))))
  (let ((code (sasm-read-file file-name)))
    (if *sasm-output-progress*
        (begin (display ";; finished scan of input file "
                        (current-output-port))
               (display file-name
                        (current-output-port))
               (newline (current-output-port))))
    (sasm-transform-code (sasm-read-file file-name))))

(define (sasm-write-transform-port code port)
  (error "Unexpected!")
  (for-each (lambda (x) (write x port))
            code))

(define (sasm-transform-statement sasm-code)
  (case (car sasm-code)
    ((class entry global export extern)
     (if *sasm-output-progress*
         (begin (display ";; transform directive: "
                         (current-output-port))
                (display (car sasm-code)
                         (current-output-port))
                (newline (current-output-port))))
     sasm-code)
    ((function)
     (if *sasm-output-progress*
         (begin
           (display ";; starting function "
                    (current-output-port))
           (display (+ *sasm-transform-function-count* 1)
                    (current-output-port))
           (display ": "
                    (current-output-port))
           (display (cadr (list-ref sasm-code 1))
                    (current-output-port))
           (newline (current-output-port))))
     (let ((func (sasm-transform-function sasm-code)))
       (if *sasm-output-progress*
           (begin
             (display ";; finished function "
                      (current-output-port))
             (set! *sasm-transform-function-count*
                   (+ *sasm-transform-function-count* 1))
             (display *sasm-transform-function-count*
                      (current-output-port))
             (display ": "
                      (current-output-port))
             (display (cadr (list-ref sasm-code 1))
                      (current-output-port))
             (newline (current-output-port)))
           #f)
       func))
    ((define-symconst)
     (sasm-symconst-alist-append (cdr sasm-code))
     sasm-code)
    ((include) (error "include directive is not allowed"))
    (else
     (error "Invalid SASM statement " sasm-code))))

(define (sasm-fix-locals n-locals sasm-code)
  (map (lambda (stmt)
         (if (and (equal? 'perform (list-ref stmt 0))
                  (equal? '(op reserve-locals) (list-ref stmt 1)))
             `(perform (op reserve-locals) (const ,n-locals))
             stmt))
       sasm-code))

(define (sasm-temp-max sasm-code)
  (let ((nums (filter-map (lambda (x)
                            (and (list? x)
                                 (= 2 (length x))
                                 (equal? 'temp (car x))
                                 (number? (cadr x))
                                 (cadr x)))
                          (apply append sasm-code))))
    (if (null? nums)
        0
        (+ 1 (apply max nums)))))

(define (sasm-local-max sasm-code)
  (let ((nums (filter-map (lambda (x)
                            (and (list? x)
                                 (= 2 (length x))
                                 (or (equal? 'local (car x))
                                     (equal? 'n-locals (car x)))
                                 (+ 1 (cadr x))))
                          (apply append sasm-code)))
        (decl (apply max 
                     0
                     (filter-map (lambda (x)
                                   (and (equal? 'perform (car x))
                                        (equal? '(op reserve-locals) (list-ref x 1))
                                        (equal? 'const (car (list-ref x 2)))
                                        (cadr (list-ref x 2))))
                                 sasm-code))))
    (apply max decl nums)))

(define (sasm-optimize-simple sasm-code)
  (define (sasm-optimize-insn insn)
    (if (pattern-match `(assign (? dest) (reg (? dest))) insn)
        '()
        (list insn)))
  (apply append (map sasm-optimize-insn sasm-code)))

(define (sasm-optimize sasm-code)
  (if *sasm-regalloc-cheap*
      (sasm-regalloc-cheap sasm-code)
      (if *sasm-enable-only-regalloc*
          (sasm-regalloc-transform sasm-code)
          (let ((opt-code (sasm-dataflow-optimize (sasm-optimize-simple sasm-code))))
            (if *sasm-enable-regalloc*
                (sasm-regalloc-transform opt-code)
                opt-code)))))

(define (sasm-transform-function sasm-function)
  (define (lookup attrib)
    (case attrib
      ((name locals)
       (cadr (assoc attrib (cdr sasm-function))))
      ((body)
       (cdr (assoc 'body (cdr sasm-function))))
      (else
       (error "Invalid attribute -- SASM-TRANSFORM" attrib))))
  (define (rewrite-stmts stmts level top result)
    (if (null? stmts)
        (let ((rewritten (reverse result)))
          (let ((opt-code (sasm-optimize (sasm-symconst-preprocess-code (sasm-fix-locals (sasm-local-max rewritten)
                                                                                         rewritten)))))
            `((locals ,(max top (sasm-local-max opt-code)))
              (body ,@opt-code))))
        (let ((stmt (car stmts)))
          (define (as-reg x)
            (cond ((symbol? x)
                   `(reg ,x))
                  ((and (list? x)
                        (eqv? 'reg (car x))
                        (= 2 (length x))
                        (symbol? (list-ref x 1)))
                   x)
                  (else
                   (error "Invalid register " x stmt))))
          (case (car stmt)
            ((save)
             (rewrite-stmts (cdr stmts) (+ level 1) (max top (+ level 1))
                            (cons `(assign (local ,level)
                                           ,(as-reg (cadr stmt)))
                                  result)))
            ((restore)
             (rewrite-stmts (cdr stmts) (- level 1) top
                            (cons `(assign ,(as-reg (cadr stmt))
                                           (local ,(- level 1)))
                                  result)))
            (else
             (rewrite-stmts (cdr stmts) level top (cons stmt result)))))))
  `(function (name ,(lookup 'name))
             ,@(rewrite-stmts (lookup 'body) (lookup 'locals) (lookup 'locals) '())))

