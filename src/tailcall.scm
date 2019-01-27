;; tailcall.scm
;; Analyze AST nodes, marking tail calls

(need util/list)
(need scheme/genproc)
(need ast)

(declare-genproc mark-tailcalls)

(define (tailcall? exp)
  (and (ast-node-attr? exp 'tailcall)
       (@ast exp tailcall)))

(define-genproc (mark-tailcalls (p program) (env global-symtab))
  (for-each (lambda (class)
              (mark-tailcalls class env))
            (@ast p :class-list)))

(define-genproc (mark-tailcalls (c class) (env global-symtab))
  (for-each (lambda (m)
              (mark-tailcalls m c env))
            (append (@ast c :methods)
                    (@ast c :static-methods)
                    (@ast c :constructors))))

(define-genproc (mark-tailcalls (m method) (c class) (env global-symtab))
  (let ((tail-stmt (terminal-pair (@ast m :statements))))
    (if (not (null? tail-stmt))
        (mark-tailcalls (car tail-stmt) m c env))))

(define-genproc (mark-tailcalls (ctor constructor) (c class) (env global-symtab))
  (let ((tail-stmt (terminal-pair (@ast ctor :statements))))
    (if (not (null? tail-stmt))
        (mark-tailcalls (car tail-stmt) 
                        (tag 'method
                             (contents ctor))
                        c
                        env))))

(define-genproc (mark-tailcalls (r return-statement) (m method) (c class) (env global-symtab))
  (if (ast-node-attr? r ':exp)
      (mark-tailcalls (@ast r :exp) m c env)))

(define-genproc (mark-tailcalls (stmt compound-statement) (m method) (c class) (env global-symtab))
  (let ((tail-stmt (terminal-pair (@ast stmt :statements))))
    (if (not (null? tail-stmt))
        (mark-tailcalls (car tail-stmt) m c env))))

(define-genproc (mark-tailcalls (stmt if-statement) (m method) (c class) (env global-symtab))
  (mark-tailcalls (@ast stmt :consequent) m c env)
  (mark-tailcalls (@ast stmt :antecedent) m c env))

(define-genproc (mark-tailcalls (stmt while-statement) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (stmt print-statement) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (stmt assignment-statement) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (stmt array-assignment-statement) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp and-expression) (m method) (c class) (env global-symtab))
  (mark-tailcalls (@ast exp :right) m c env))

(define-genproc (mark-tailcalls (exp less-than-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp equal-to-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp not-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp add-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp subtract-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp multiply-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp int-binop-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp int-unop-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp array-ref-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp length-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp method-call-expression) (m method) (c class) (env global-symtab))
  (ast-node-attr! exp 'tailcall #t))

(define-genproc (mark-tailcalls (exp number-constant-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp boolean-constant-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp char-constant-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp string-constant-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp variable-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp this-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp new-array-expression) (m method) (c class) (env global-symtab))
  'nothing)

(define-genproc (mark-tailcalls (exp new-object-expression) (m method) (c class) (env global-symtab))
  'nothing)
