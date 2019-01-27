;; symtab.scm
;; Symbol-Table-Building AST visitor

(need scheme/genproc)
(need ast)

(define (typecheck-digest a)
  a)

(declare-genproc build-symtab)

(define-genproc (build-symtab (p program))
  (tag 'global-symtab
       (map build-symtab (@ast p :class-list))))

(define-genproc (build-symtab (c class))
  (list (@ast c :class-name)
        (@ast c :parent)
        (map build-symtab (@ast c :instance-vars))
        (map build-symtab (@ast c :methods))
        (typecheck-digest c)))

(define-genproc (build-symtab (v var))
  (cons (@ast v :name) (typecheck-digest (@ast v :type))))

(define-genproc (build-symtab (m method))
  (list (@ast m :name)
        (map build-symtab (@ast m :param-list))
        (map build-symtab (@ast m :local-vars))
        (typecheck-digest (@ast m :return-type))))

(define-genproc (build-symtab (p param))
  (cons (@ast p :name) (typecheck-digest (@ast p :type))))

;; This routine updates the compiler's symbol table using the class-info
;; metadata that is stored in the compiler's SASM output.
(define (augment-symtab-with-class-info old-symtab class-info)
  (define (single-class-info->class info)
    info)
  (let ((classes (contents old-symtab)))
    old-symtab))

; (define (class-info-preamble class-list env context)
;   (define (type-info type)
;     (case (ast-node-type type)
;       ((simple-type) (cadr (assoc (@ast type :name)
;                                   '(("int" int) ("boolean" bool)
;                                     ("void" void) ("char" char)))))
;       ((array-type) `(array ,(type-info (@ast type :element-type))))
;       ((user-type) `(user ,(@ast type :name)))
;       (else (error "Invalid type node" (ast-node-type type)))))
;   (define (var-info var)
;     `(var (name ,(@ast var :name))
;           (type ,(type-info (@ast var :type)))))
;   (define (method-info method)
;     `(method (name ,(@ast method :name))
;              (return-type ,(type-info (@ast method :return-type)))
;              (param-list ,@(map var-info (@ast method :param-list)))))
;   (define (per-class-info class)
;     `(class (name ,(@ast class :class-name))
;             (parent ,(@ast class :parent))
;             (instance-vars ,@(map var-info (@ast class :instance-vars)))
;             (static-vars ,@(map var-info (@ast class :static-vars)))
;             (instance-methods ,@(map method-info (@ast class :methods)))
;             (static-methods ,@(map method-info (@ast class :static-methods)))))
;   (emit-code context
;              `(class-info
;                ,@(map per-class-info class-list))))
