(need util/list)
(need scheme/genproc)

(define-syntax ast-node
  (syntax-rules ()
    ((_ type (name value) ...)
     (tag 'type (list (cons 'name value) ...)))))

(define-syntax @ast
  (syntax-rules ()
    ((_ node name)
     (ast-node-attr node 'name))))

(define known-ast-node-types
  '(program
    class
    method
    constructor
    simple-type
    param
    array-type
    var
    static-var
    return-statement
    user-type
    compound-statement
    if-statement
    while-statement
    print-statement
    assignment-statement
    array-assignment-statement
    and-expression
    less-than-expression
    equal-to-expression
    add-expression
    subtract-expression
    multiply-expression
    int-binop-expression
    int-unop-expression
    not-expression
    array-ref-expression
    length-expression
    method-call-expression
    number-constant-expression
    boolean-constant-expression
    char-constant-expression
    string-constant-expression
    variable-expression
    this-expression
    new-array-expression
    new-object-expression))

(define (ast-node-type node)
  (get-tag node))

(define (ast-node-attr node attr)
  (let ((entry (assoc attr (contents node))))
    (if entry
        (cdr entry)
        (error "AST Node " node " does not have attribute " attr))))

(define (ast-node? node)
  (member (ast-node-type node) known-ast-node-types))

(define (ast-node-attr? node attr)
  (if (not (ast-node? node))
      (error "Not an AST -- ast-node-attr?" node attr))
  (assoc attr (contents node)))

(define (ast-node-attr! node attr value)
  (let ((entry (assoc attr (contents node))))
    (if entry
        (set-cdr! entry value)
        (let ((last (terminal-pair (contents node))))
          (set-cdr! last (list (cons attr value)))))))

(define (visit-each visitor nodes user)
;  (if (not (list? nodes))
;      (begin (println "Bad nodes ")
;             (write (car nodes))
;             (newline)
;             (error "Unable to visit-each - not a proper list")))
  (for-each (lambda (node) (accept-visitor node visitor user))
            nodes))

(define (visit-each-accum visitor nodes user base accum)
  (if (null? nodes)
      base
      (visit-each-accum visitor
                        (cdr nodes)
                        user
                        (let ((val (accept-visitor (car nodes) visitor user)))
                          (accum val base))
                        accum)))

(define (accept-visitor node visitor user)
  (if (not (member (ast-node-type node) known-ast-node-types))
      (error "Accept-Visitor: unknown AST node type " node))
  (visitor (ast-node-type node) node user))

;; neo-ast-visitor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - neo-ast-visitor 
;;
;;   - better user dispatch, i.e. user shouldn't have to do their own dispatch
;;   - context; user might need to know statement corresponding to expression;
;;     class corresponding to method; which sub-exp of the statement they were
;;     given, etc.
;;

(define (ast-visit-context interesting-types visit-proc . user-context)
  (tag 'ast-visit-context (vector interesting-types visit-proc user-context)))

(define (ast-visit-node ast-node context)
  (let ((vec (safe-contents 'ast-visit-context context)))
    (if (or (not (vector-ref vec 0))
            (member (get-tag ast-node) (vector-ref vec 0)))
        (apply (vector-ref vec 1) ast-node (vector-ref vec 2)))))

(define (ast-visit-tree ast-node context)
  (ast-visit ast-node context))

(define (ast-visit-start ast-node context)
  #t)

(define (ast-visit-finish ast-node context)
  (ast-visit-node ast-node context))

(define (ast-visit-trees nodes context)
  (for-each (lambda (n) (ast-visit-tree n context))
            nodes))

(declare-genproc ast-visit)

(define-genproc (ast-visit (p program) (ctx ast-visit-context))
  (ast-visit-start p ctx)
  (ast-visit-trees (@ast p :class-list) ctx)
  (ast-visit-finish p ctx))

(define-genproc (ast-visit (c class) (ctx ast-visit-context))
  (ast-visit-start c ctx)
  (ast-visit-trees (@ast c :methods) ctx)
  (ast-visit-trees (@ast c :static-methods) ctx)
  (ast-visit-trees (@ast c :constructors) ctx)
  (ast-visit-finish c ctx))

(define-genproc (ast-visit (c constructor) (ctx ast-visit-context))
  (ast-visit-start c ctx)
  (ast-visit-trees (@ast c :statements) ctx)
  (ast-visit-finish c ctx))

(define-genproc (ast-visit (m method) (ctx ast-visit-context))
  (ast-visit-start m ctx)
  (ast-visit-trees (@ast m :statements) ctx)
  (ast-visit-finish m ctx))

(define-genproc (ast-visit (stmt return-statement) (ctx ast-visit-context))
  (ast-visit-start stmt ctx)
  (if (ast-node-attr? stmt ':exp)
      (ast-visit-tree (@ast stmt :exp) ctx))
  (ast-visit-finish stmt ctx))

(define-genproc (ast-visit (stmt compound-statement) (ctx ast-visit-context))
  (ast-visit-start stmt ctx)
  (ast-visit-trees (@ast stmt :statements) ctx)
  (ast-visit-finish stmt ctx))

(define-genproc (ast-visit (stmt if-statement) (ctx ast-visit-context))
  (ast-visit-start stmt ctx)
  (ast-visit-tree (@ast stmt :condition) ctx)
  (ast-visit-tree (@ast stmt :consequent) ctx)
  (ast-visit-tree (@ast stmt :antecedent) ctx)
  (ast-visit-finish stmt ctx))

(define-genproc (ast-visit (stmt while-statement) (ctx ast-visit-context))
  (ast-visit-start stmt ctx)
  (ast-visit-tree (@ast stmt :condition) ctx)
  (ast-visit-tree (@ast stmt :body) ctx)
  (ast-visit-finish stmt ctx))

(define-genproc (ast-visit (stmt print-statement) (ctx ast-visit-context))
  (ast-visit-start stmt ctx)
  (ast-visit-tree (@ast stmt :exp) ctx)
  (ast-visit-finish stmt ctx))

(define-genproc (ast-visit (stmt assignment-statement) (ctx ast-visit-context))
  (ast-visit-start stmt ctx)
  (ast-visit-tree (@ast stmt :exp) ctx)
  (ast-visit-finish stmt ctx))

(define-genproc (ast-visit (stmt array-assignment-statement) (ctx ast-visit-context))
  (ast-visit-start stmt ctx)
  (ast-visit-tree (@ast stmt :value-exp) ctx)
  (ast-visit-tree (@ast stmt :index-exp) ctx)
  (ast-visit-finish stmt ctx))

(define-genproc (ast-visit (exp not-expression) (ctx ast-visit-context))
  (ast-visit-unop exp ctx))

(define-genproc (ast-visit (exp less-than-expression) (ctx ast-visit-context))
  (ast-visit-binop exp ctx))

(define-genproc (ast-visit (exp equal-to-expression) (ctx ast-visit-context))
  (ast-visit-binop exp ctx))

(define-genproc (ast-visit (exp and-expression) (ctx ast-visit-context))
  (ast-visit-binop exp ctx))

(define-genproc (ast-visit (exp add-expression) (ctx ast-visit-context))
  (ast-visit-binop exp ctx))

(define-genproc (ast-visit (exp int-binop-expression) (ctx ast-visit-context))
  (ast-visit-binop exp ctx))

(define-genproc (ast-visit (exp int-unop-expression) (ctx ast-visit-context))
  (ast-visit-unop exp ctx))

(define-genproc (ast-visit (exp subtract-expression) (ctx ast-visit-context))
  (ast-visit-binop exp ctx))

(define-genproc (ast-visit (exp multiply-expression) (ctx ast-visit-context))
  (ast-visit-binop exp ctx))

(define (ast-visit-binop exp ctx)
  (ast-visit-start exp ctx)
  (ast-visit-tree (@ast exp :left) ctx)
  (ast-visit-tree (@ast exp :right) ctx)
  (ast-visit-finish exp ctx))

(define (ast-visit-unop exp ctx)
  (ast-visit-start exp ctx)
  (ast-visit-tree (@ast exp :exp) ctx)
  (ast-visit-finish exp ctx))

(define (ast-visit-trivial exp ctx)
  (ast-visit-start exp ctx)
  (ast-visit-finish exp ctx))

(define-genproc (ast-visit (exp array-ref-expression) (ctx ast-visit-context))
  (ast-visit-start exp ctx)
  (ast-visit-tree (@ast exp :index-exp) ctx)
  (ast-visit-tree (@ast exp :array-exp) ctx)
  (ast-visit-finish exp ctx))

(define-genproc (ast-visit (exp length-expression) (ctx ast-visit-context))
  (ast-visit-unop exp ctx))

(define-genproc (ast-visit (exp length-expression) (ctx ast-visit-context))
  (ast-visit-trivial exp ctx))

(define-genproc (ast-visit (exp number-constant-expression) (ctx ast-visit-context))
  (ast-visit-trivial exp ctx))

(define-genproc (ast-visit (exp char-constant-expression) (ctx ast-visit-context))
  (ast-visit-trivial exp ctx))

(define-genproc (ast-visit (exp string-constant-expression) (ctx ast-visit-context))
  (ast-visit-trivial exp ctx))

(define-genproc (ast-visit (exp boolean-constant-expression) (ctx ast-visit-context))
  (ast-visit-trivial exp ctx))

(define-genproc (ast-visit (exp variable-expression) (ctx ast-visit-context))
  (ast-visit-trivial exp ctx))

(define-genproc (ast-visit (exp this-expression) (ctx ast-visit-context))
  (ast-visit-trivial exp ctx))

(define-genproc (ast-visit (exp new-array-expression) (ctx ast-visit-context))
  (ast-visit-start exp ctx)
  (ast-visit-tree (@ast exp :length) ctx)
  (ast-visit-finish exp ctx))

(define-genproc (ast-visit (exp new-object-expression) (ctx ast-visit-context))
  (ast-visit-start exp ctx)
  (ast-visit-trees (@ast exp :arg-list) ctx)
  (ast-visit-finish exp ctx))

(define-genproc (ast-visit (exp method-call-expression) (ctx ast-visit-context))
  (ast-visit-start exp ctx)
  (ast-visit-tree (@ast exp :object) ctx)
  (ast-visit-trees (@ast exp :arg-list) ctx)
  (ast-visit-finish exp ctx))
