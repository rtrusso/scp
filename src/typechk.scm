;; typechk.scm
;; Type-checking module of the MiniJava frontend
;;

(need scheme/genproc)
(need ast)

;; typecheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare-genproc typecheck)

(define-genproc (typecheck (p program) (env global-symtab))
  (and (check-class-names-unique (@ast p :class-list) env)
       (check-class-hierarchy (@ast p :class-list) env)
       (map-and (lambda (class)
                  (typecheck class env))
                (@ast p :class-list))))

(define-genproc (typecheck (c class) (env global-symtab))
  (and (or-error (check-parent-exists c env)
                 "Class " (@ast c :class-name) " extends a class '" (@ast c :parent) "' that does not exist; "
                 "see " (@:line-col c))
       (check-instance-var-names-unique (append (@ast c :instance-vars) (@ast c :static-vars)) env)
       (map-and (lambda (var)
                  (or-error (check-instance-var-type-exists var env)
                            "Type " (@ast var :type) " doest exist; " (@:line-col var)))
                (@ast c :instance-vars))
       (let ((all-methods (append (@ast c :methods) (@ast c :static-methods))))
         (map-and (lambda (method-name)
                    (check-method-sigs-orthogonal (filter (lambda (m) (method-name=? method-name (@ast m :name)))
                                                          all-methods)
                                                  env))
                  (remove-duplicates string=? (map (lambda (m) (@ast m :name)) all-methods))))
       (check-overloaded-methods (@ast c :methods) c env)
       (check-ctor-sigs-unique (@ast c :constructors) env)
       (map-and (lambda (method)
                  (typecheck method c env))
                (append (@ast c :methods)
                        (@ast c :static-methods)
                        (@ast c :constructors)) )))

(define-genproc (typecheck (m method) (c class) (env global-symtab))
  (and (check-param-names-unique (@ast m :param-list) env)
       (check-local-var-names-unique (@ast m :local-vars) env)
       (check-param-types-exist (@ast m :param-list) env)
       (check-local-var-types-exist (@ast m :param-list) env)
       (check-return-type-exists (@ast m :return-type) env)
       (map-and (lambda (statement)
                  (typecheck statement m c env))
                (@ast m :statements))))

(define-genproc (typecheck (ctor constructor) (c class) (env global-symtab))
  (and (check-param-names-unique (@ast ctor :param-list) env)
       (check-local-var-names-unique (@ast ctor :local-vars) env)
       (check-param-types-exist (@ast ctor :param-list) env)
       (check-local-var-types-exist (@ast ctor :local-vars) env)
       (or-error (string=? (@ast (@ast ctor :return-type) :name)
                           (@ast c :class-name))
                 "Constructor at " (@:line-col ctor) " defined with invalid name")
       ;; (check-class-exists <-- unnecessary because grammar ensures this -->)
       (map-and (lambda (statement)
                  (let ((vmethod (ast-node method
                                           (:line-col (and (ast-node-attr? ctor ':line-col) (@ast ctor :line-col)))
                                           (:return-type (@ast ctor :return-type))
                                           (:statements (@ast ctor :statements))
                                           (:static? #t)
                                           (:virtual-ctor #t)
                                           (:local-vars (@ast ctor :local-vars))
                                           (:param-list (@ast ctor :param-list)))))
                    (typecheck statement vmethod c env)))
                (@ast ctor :statements))
       (begin (ast-node-attr! ctor ':name (class-get-constructor-method-name c ctor))
              #t)))

(define-genproc (typecheck (r return-statement) (m method) (c class) (env global-symtab))
  (if (ast-node-attr? m ':virtual-ctor)
      (and (or-error (not (ast-node-attr? r ':exp))
                     "Return statement in constructor at " (@:line-col r) " must not return a value.")
           (@ast m :return-type))
      (and 
       (typecheck (@ast r :exp) m c env)
       (or-error 
        (check-computed-type-assignable env
                                        (@ast m :return-type)
                                        (typecheck (@ast r :exp) m c env))
        "The type of the return statement at " (@:line-col r) " '" 
        (typecheck (@ast r :exp) m c env) 
        "' is not compatible with the declared return type of the method "
        (@ast c :class-name) "." (@ast m :name) " which is '" 
        (@ast m :return-type) "'" ))))

(define-genproc (typecheck (r compound-statement) (m method) (c class) (env global-symtab))
  (map-and (lambda (stmt)
             (typecheck stmt m c env))
           (@ast r :statements)))

(define-genproc (typecheck (s if-statement) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast s :condition) m c env)
       (or-error
        (check-computed-type-assignable env
                                        (type-boolean)
                                        (typecheck (@ast s :condition) m c env))
        "'if' statement at " (@:line-col s) " has a condition that is not of type boolean")
       (typecheck (@ast s :consequent) m c env)
       (typecheck (@ast s :antecedent) m c env)))

(define-genproc (typecheck (s while-statement) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast s :condition) m c env)
       (or-error
        (check-computed-type-assignable env
                                        (type-boolean)
                                        (typecheck (@ast s :condition) m c env))
        "'while' statement at " (@:line-col s) " has a condition that is not of type boolean")
       (typecheck (@ast s :body) m c env)))

(define-genproc (typecheck (p print-statement) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast p :exp) m c env)
       (or-error
        (check-computed-type-assignable env
                                        (type-int)
                                        (typecheck (@ast p :exp) m c env))
        "System.out.println statement at line " (@:line-col p) " has a condition that is not of type int")))
           

(define-genproc (typecheck (s assignment-statement) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast s :exp) m c env)
       (or-error 
        (check-ident-assignable (@ast s :ident) m c env)
        "In assignment at " (@:line-col s) "; identifier " (@ast s :ident) " was not declared")
       (or-error
        (check-computed-type-assignable env
                                        (lookup-identifier-type (@ast s :ident) m c env)
                                        (typecheck (@ast s :exp) m c env))
        "In assignment at " (@:line-col s) "; RHS type '" (typecheck (@ast s :exp) m c env)
        "' is not compatible with the type of identifier '" (@ast s :ident) "': '" 
        (lookup-identifier-type (@ast s :ident) m c env) "'")))

(define-genproc (typecheck (s array-assignment-statement) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast s :value-exp) m c env)
       (typecheck (@ast s :index-exp) m c env)
       (or-error
        (check-ident-assignable-array-type (@ast s :ident) m c env)
        "In assignment at " (@:line-col s) "; identifier " (@ast s :ident) " is not an assignable array value")
       (or-error
        (check-computed-type-assignable env
                                        (array-element-type (lookup-identifier-type (@ast s :ident) m c env))
                                        (typecheck (@ast s :value-exp) m c env))
        "In assignment at " (@:line-col s) "; RHS type is not compatible with the element type of array " (@ast s :ident))
       (or-error
        (check-computed-type-assignable env
                                        (type-int)
                                        (typecheck (@ast s :index-exp) m c env))
        "In assignment at " (@:line-col s) "; array index expression is not of type int")))

(define (check-binop exp type m c env)
  (and (typecheck (@ast exp :left) m c env)
       (typecheck (@ast exp :right) m c env)
       (or-error
        (check-computed-type-assignable env type (typecheck (@ast exp :left) m c env))
        "In " (ast-node-type exp) " at " (@:line-col exp) "; LHS expression (of type '"
        (typecheck (@ast exp :left) m c env)"') is not compatible with '" type "'")
       (or-error
        (check-computed-type-assignable env type (typecheck (@ast exp :right) m c env))
        "In " (ast-node-type exp) " at " (@:line-col exp) "; RHS expression (of type '" 
        (typecheck (@ast exp :right) m c env) "') is not compatible with '" type "'")))

(define-genproc (typecheck (exp and-expression) (m method) (c class) (env global-symtab))
  (and (check-binop exp (type-boolean) m c env)
       (type-boolean)))

(define-genproc (typecheck (exp less-than-expression) (m method) (c class) (env global-symtab))
  (and (check-binop exp (type-int) m c env)
       (type-boolean)))

(define-genproc (typecheck (exp equal-to-expression) (m method) (c class) (env global-symtab))
  (let* ((type-left (typecheck (@ast exp :left) m c env))
         (type-right (and type-left (typecheck (@ast exp :right) m c env))))
    (and type-left
         type-right
         (or-error
          (or (check-computed-type-assignable env type-left type-right)
              (check-computed-type-assignable env type-right type-left))
          "In == expression at " (@:line-col exp) "; LHS expression (of type '"
          type-left "') is not compatible with RHS expression (of type '"
          type-right "')")
         (type-boolean))))

(define-genproc (typecheck (exp not-expression) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast exp :exp) m c env)
       (or-error
        (check-computed-type-assignable env
                                        (type-boolean)
                                        (typecheck (@ast exp :exp) m c env))
        "In !-expression at " (@:line-col exp) "; operand is not of type boolean")
       (type-boolean)))

(define-genproc (typecheck (exp int-binop-expression) (m method) (c class) (env global-symtab))
  (and (check-binop exp (type-int) m c env)
       (type-int)))

(define-genproc (typecheck (exp int-unop-expression) (m method) (c class) (env global-symtab))
  (let ((type (typecheck (@ast exp :exp) m c env)))
    (and type
         (or-error
          (check-computed-type-assignable env (type-int) type)
          "In " (ast-node-type exp) " at " (@:line-col exp) "; expression of type '"
          type "' is not compatible with 'int'")
         (type-int))))

(define-genproc (typecheck (exp add-expression) (m method) (c class) (env global-symtab))
  (and (check-binop exp (type-int) m c env)
       (type-int)))

(define-genproc (typecheck (exp subtract-expression) (m method) (c class) (env global-symtab))
  (and (check-binop exp (type-int) m c env)
       (type-int)))

(define-genproc (typecheck (exp multiply-expression) (m method) (c class) (env global-symtab))
  (and (check-binop exp (type-int) m c env)
       (type-int)))

(define-genproc (typecheck (exp array-ref-expression) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast exp :array-exp) m c env)
       (typecheck (@ast exp :index-exp) m c env)
       (or-error
        (check-array-type (typecheck (@ast exp :array-exp) m c env))
        "In array reference at " (@:line-col exp) "; the object is not of a valid array type")
       (or-error
        (check-computed-type-assignable env
                                        (type-int)
                                        (typecheck (@ast exp :index-exp) m c env))
        "In array reference at " (@:line-col exp) "; the index expression is not of type int")
       (array-element-type (typecheck (@ast exp :array-exp) m c env))))

(define-genproc (typecheck (exp length-expression) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast exp :exp) m c env)
       (or-error
        (check-array-type (typecheck (@ast exp :exp) m c env))
        "In .length expression at " (@:line-col exp) "; the operand is not of a valid array type")
       (type-int)))

(define-genproc (typecheck (exp method-call-expression) (m method) (c class) (env global-symtab))
  (and (map-and (lambda (subexp)
                  (typecheck subexp m c env))
                (@ast exp :arg-list))
       (typecheck (@ast exp :object) m c env)
       (let* ((obj-type (typecheck (@ast exp :object) m c env))
              (obj-type-valid? (and obj-type (method-target-type? obj-type)))
              (param-types (map (lambda (subexp)
                                  (typecheck subexp m c env))
                                (@ast exp :arg-list)))
              (target-method (and obj-type
                                  obj-type-valid?
                                  (if (class-reference-type? obj-type)
                                      (lookup-static-method (@ast exp :method-name) 
                                                            param-types
                                                            (lookup-class-type (@ast obj-type :name) env) 
                                                            env)
                                      (lookup-method-type (@ast exp :method-name)
                                                          param-types
                                                          (user-type->class-type obj-type env)
                                                          env))) ))
         (and obj-type
              (or-error
               obj-type-valid?
               "At " (@:line-col exp) "; type '" obj-type "' is not allowed to be the target of a method call")
              param-types
              (or-error
               target-method
               "In method call at " (@:line-col exp) "; method name " (@ast exp :method-name)
               " could not be resolved")
              (or-error 
               (check-correct-number-args (@ast exp :arg-list) target-method)
               "In method call at " (@:line-col exp) "; an incorrect number of arguments was specified")
              (map-and (lambda (x) x)
                       (map (lambda (arg-exp param-type)
                              (or-error
                               (check-computed-type-assignable env
                                                               (@ast param-type :type)
                                                               (typecheck arg-exp m c env))
                               "In method call at " (@:line-col exp) " argument at " (@:line-col arg-exp)
                               " of type '" (typecheck arg-exp m c env) 
                               "' is not compatible with parameter of type '" (@ast param-type :type) "'"))
                            (@ast exp :arg-list)
                            (type-method-param-list target-method)))
              (if (@ast target-method :static?)
                  (begin (ast-node-attr! exp ':static-method-call #t)
                         #t)
                  #t)
              (begin (ast-node-attr! exp ':explicit-method target-method)
                     #t)
              (type-method-return target-method)))))

(define-genproc (typecheck (exp number-constant-expression) (m method) (c class) (env global-symtab))
  (type-int))

(define-genproc (typecheck (exp boolean-constant-expression) (m method) (c class) (env global-symtab))
  (type-boolean))

(define-genproc (typecheck (exp char-constant-expression) (m method) (c class) (env global-symtab))
  (type-char))

(define-genproc (typecheck (exp string-constant-expression) (m method) (c class) (env global-symtab))
  (type-array (type-char)))

(define-genproc (typecheck (exp variable-expression) (m method) (c class) (env global-symtab))
  (or-error (lookup-identifier-type (@ast exp :ident) m c env)
            "Undeclared identifier '" (@ast exp :ident) "' at " (@:line-col exp)))

(define-genproc (typecheck (exp this-expression) (m method) (c class) (env global-symtab))
  (type-class c))

(define-genproc (typecheck (exp new-array-expression) (m method) (c class) (env global-symtab))
  (and (typecheck (@ast exp :length) m c env)
       (or-error
        (check-computed-type-assignable env
                                        (type-int)
                                        (typecheck (@ast exp :length) m c env))
        "In new[] expression at " (@:line-col exp) "; size operand is not of type int")
       (type-array (@ast exp :element-type))))

(define-genproc (typecheck (exp new-object-expression) (m method) (c class) (env global-symtab))
  (let ((exp-class (lookup-class-type (@ast exp :class-name) env)))
    (and (or-error exp-class
                   "In new expression at " (@:line-col exp) "; class '" (@ast exp :class-name) "' is not defined")
         (or (and (null? (@ast exp-class :constructors))
                  (null? (@ast exp :arg-list)))
             (and (or-error
                   (check-class-exists (@ast exp :class-name) env)
                   "In 'new' expression at " (@:line-col exp) "; class identifier " (@ast exp :class-name) " is undeclared")
                  (let ((sig (map (lambda (arg) (typecheck arg m c env))
                                  (@ast exp :arg-list))))
                    (and (all? (lambda (x) x) sig)
                         (let ((target (class-find-ctor env exp-class sig)))
                           (if target
                               (ast-node-attr! exp ':target-ctor target))
                           (or-error target ;; todo: tell user if can't find or ambig
                                     "Error in 'new' expression at " (@:line-col exp)
                                     ": unable to find suitable constructor for type "
                                     (@ast exp-class :class-name)))))))
         (type-class (lookup-class-type (@ast exp :class-name) env)))))

