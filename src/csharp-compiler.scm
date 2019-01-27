(need parse/parse)
(need regex/dfa)
(need regex/genlex)

(need ast)
(need csharp-scanner)
(need csharp-parser)
(need types)
(need symtab)
(need typechk)
(need tailcall)
(need codegen)
(need sasm/sasm-codegen)

(define *output-port* (current-output-port))

(define (emit-code context code)
  (sasm-pretty-print code *output-port*)
  (newline *output-port*))

(define (emit-statement ast-node code)
  code)

(define (parse-port port)
  (let ((lex (csharp-lexically-analyze port)))
    (and lex
         (csharp-parse-input lex))))

(define (run-typecheck ast)
  (let ((symtab (build-symtab ast)))
    (and symtab
         (let ((typecheck-result (typecheck ast symtab)))
           (if (not typecheck-result)
               (exit 1))
           (and typecheck-result
                symtab)))))

(define (run-codegen-analysis ast env)
  (mark-tailcalls ast env)
  #t)

(define (compile port)
  (let ((ast (parse-port port)))
    (if (not ast)
        (exit 1))
    (and ast
         (let ((env (run-typecheck ast)))
           (and env
                (run-codegen-analysis ast env)
                (codegen ast env (tag 'context '())))))))

(define (merge-programs program-a program-b)
  (define (choose-main main-a main-b)
    (cond (main-b main-b)
          (main-a main-a)
          (else #f)))
  (define (get-main)
    (choose-main (and (ast-node-attr? program-a ':main-class) (@ast program-a :main-class))
                 (and (ast-node-attr? program-b ':main-class) (@ast program-b :main-class))))
  (ast-node program
            (:main-class (get-main))
            (:class-list (append (@ast program-a :class-list)
                                 (@ast program-b :class-list)))))

(define (merge-multiple-programs list)
  (accum merge-programs (car list) (cdr list)))

(define (compile-multiple main-class programs)
  (let ((asts (map (lambda (file) (call-with-input-file file parse-port)) programs)))
    (and (all? (lambda (x) x) asts)
         (let* ((master-program (merge-multiple-programs asts))
                (env (run-typecheck master-program)))
           (and env
                (run-codegen-analysis master-program env)
                (codegen master-program env (tag 'context `((main-class ,main-class)))))))))

(cond ((= 1 (vector-length *argv*))
       (compile (current-input-port)))
      ((= 2 (vector-length *argv*))
       (call-with-input-file (vector-ref *argv* 1) compile))
      ((and (>= (vector-length *argv*) 3)
            (starts-with? (vector-ref *argv* 1) "--main="))
       (let ((main-class-name (let ((cmd (vector-ref *argv* 1)))
                                (substring cmd (string-length "--main=") (string-length cmd))))
             (args
              (filter (lambda (arg)
                        (cond
                         ((starts-with? arg "--out=")
                          (set! *output-port*
                                (open-output-file (string-strip-prefix arg
                                                                       "--out=")))
                          #f)
                         (else #t)))
                      (cddr (vector->list *argv*)))))
         (compile-multiple main-class-name args)))
      (else
       (error "Invalid command line " *argv*)))
