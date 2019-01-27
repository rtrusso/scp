;; types.scm
;; Defines type semantics
;;

(need ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (@:line-col ast)
  (if (and (ast-node-attr? ast ':line-col)
           (@ast ast :line-col))
      (@ast ast :line-col)
      (cons 0 0)))

(define (typecheck-error . msg-args)
  (for-each (lambda (obj) (display obj (current-error-port)))
            msg-args)
  (newline)
  #f)

(define (or-error exp . msg-args)
  (define (disp obj)
    (cond ((and (list? obj) (= 2 (length obj))
                (number? (car obj)) (number? (cadr obj)))
           (for-each (lambda (obj) (display obj (current-error-port)))
                     (list "line " (+ (car obj) 1) ", column " (+ (cadr obj) 1))))
          ((or (tagged? 'user-type obj)
               (tagged? 'simple-type obj))
           (display (@ast obj :name) (current-error-port)))
          ((tagged? 'array-type obj)
           (disp (@ast obj :element-type))
           (display "[]" (current-error-port)))
          (else
           (display obj (current-error-port)))))
  (if (not exp)
      (begin (display "ERROR: " (current-error-port))
             (for-each disp msg-args)
             (newline (current-error-port))))
  exp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type? obj)
  (case (get-tag obj)
    ((user-type simple-type array-type) #t)
    (else #f)))

(define (type-assignable? dest-type src-type env)
  (if (not (and (type? dest-type)
                (type? src-type)))
      (error "Not a type -- type-assignable?" dest-type src-type))
  (if (and (user-type? dest-type)
           (user-type? src-type))
      (type-search-inheritance-hierarchy (@ast src-type :name) (@ast dest-type :name) env)
      (or (and (simple-type? dest-type)
               (simple-type? src-type)
               (equal? "int" (@ast dest-type :name))
               (equal? "char" (@ast src-type :name)))
          (type-equivalent? dest-type src-type))))

(define (type-more-specific? baseline-type test-type env)
  (and (type-assignable? baseline-type test-type env)
       (not (type-assignable? test-type baseline-type env))))

(define (type-search-inheritance-hierarchy from-type to-type env)
  (or (equal? from-type to-type)
      (let ((class-obj (lookup-class-type from-type env)))
        (and class-obj
             (@ast class-obj :parent)
             (let ((parent-class-obj (lookup-class-type (@ast class-obj :parent) env)))
               (and parent-class-obj
                    (type-search-inheritance-hierarchy (@ast parent-class-obj :class-name) to-type env)))))))

(define (type-get-inheritance-hierarchy from-type to-type env)
  (cond ((equal? from-type to-type)
         (list to-type))
        ((not (lookup-class-type from-type env))
         #f)
        ((not (@ast (lookup-class-type from-type env) :parent))
         (list from-type))
        (else
         (let ((parent-class-obj (lookup-class-type (@ast (lookup-class-type from-type env) :parent) env)))
           (and parent-class-obj
                (cons from-type
                      (type-get-inheritance-hierarchy (@ast parent-class-obj :class-name) to-type env)))))))

(define (type-equivalent? type-a type-b)
  (and (equal? (ast-node-type type-a)
               (ast-node-type type-b))
       (case (ast-node-type type-a)
         ((array-type) (type-equivalent? (@ast type-a :element-type)
                                         (@ast type-b :element-type)))
         ((simple-type user-type)
          (equal? (@ast type-a :name)
                  (@ast type-b :name)))
         ((class-type)
          (equal? (@ast type-a :name) (@ast type-b :name)))
         (else #f))))

(define (array-element-type type)
  (and (equal? 'array-type (ast-node-type type))
       (@ast type :element-type)))

(define (type-boolean)
  (ast-node simple-type
            (:name "boolean")))

(define (type-int)
  (ast-node simple-type
            (:name "int")))

(define (type-char)
  (ast-node simple-type
            (:name "char")))

(define (type-string)
  (ast-node user-type
            (:name "String")))

(define (type-array subtype)
  (ast-node array-type
            (:element-type subtype)))

(define (type-class class)
  (ast-node user-type
            (:name (@ast class :class-name))))

(define (type-class-reference class-name)
  (ast-node class-reference-type
            (:name class-name)))

(define (simple-type? type)
  (equal? 'simple-type (ast-node-type type)))

(define (user-type? type)
  (equal? 'user-type (ast-node-type type)))

(define (method-target-type? type)
  (or (class-reference-type? type)
      (user-type? type)))

(define (user-type->class-type type env)
  (lookup-class-type (@ast type :name) env))

(define (class-reference-type? type)
  (equal? 'class-reference-type (ast-node-type type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identifier-assoc-ctx name nodes context)
  (cond ((null? nodes) #f)
        ((equal? name (@ast (car nodes) :name))
         (cons (car nodes) context))
        (else
         (identifier-assoc-ctx name (cdr nodes) context))))

(define (identifier-assoc name nodes)
  (let ((result (identifier-assoc-ctx name nodes '())))
    (and result (car result))))

(define (identifier-offset name nodes)
  (define (iter idx nodes)
    (cond ((null? nodes) #f)
          ((equal? name (@ast (car nodes) :name))
           idx)
          (else
           (iter (+ idx 1) (cdr nodes)))))
  (iter 0 nodes))

(define (resolve-identifier name method class env)
  (or (identifier-assoc-ctx name (@ast method :local-vars) (list 'local class))
      (identifier-assoc-ctx name (@ast method :param-list) (list 'arg class))
      (and (or (not (@ast method :static?))
               (ast-node-attr? method ':virtual-ctor))
           (identifier-assoc-ctx name (@ast class :instance-vars) (list 'member class)))
      (identifier-assoc-ctx name (@ast class :static-vars) (list 'static class))
      (and (@ast class :parent)
           (resolve-identifier name method (lookup-class-type (@ast class :parent) env) env))
      (let ((c (lookup-class-type name env)))
        (and c
             (cons (ast-node class-reference-type
                             (:name name))
                   class))) ))

(define (lookup-identifier name method class env)
  (let ((result (resolve-identifier name method class env)))
    (and result (car result))))

(define (lookup-identifier-type name method class env)
  (let ((obj (lookup-identifier name method class env)))
    (and obj 
         (if (class-reference-type? obj)
             obj
             (@ast obj :type)))))

(define (lookup-method method-name sig class env)
  (or (find-method-by-sig sig
                          (filter (lambda (m) (method-name=? (@ast m :name) method-name))
                                  (@ast class :methods))
                          env)
      (and (@ast class :parent)
           (lookup-method method-name sig (lookup-class-type (@ast class :parent) env) env))))

(define (lookup-static-method method-name sig class env)
  (find-method-by-sig sig
                      (filter (lambda (m) (method-name=? (@ast m :name) method-name))
                              (@ast class :static-methods))
                      env))

(define (lookup-method-type method-name sig class env)
  (lookup-method method-name sig class env))

(define (lookup-static-method-type method-name sig class env)
  (lookup-static-method method-name sig class env))

(define (methods-equivalent? method-a method-b)
  (and (equal? (@ast method-a :static?) (@ast method-b :static?))
       (type-equivalent? (@ast method-a :return-type)
                         (@ast method-b :return-type))
       (= (length (@ast method-a :param-list))
          (length (@ast method-b :param-list)))
       (map-and (lambda (x) x)
                (map (lambda (param-a param-b)
                       (type-equivalent? (@ast param-a :type)
                                         (@ast param-b :type)))
                     (@ast method-a :param-list)
                     (@ast method-b :param-list)))))

(define (method-name=? method-name-1 method-name-2)
  (string=? method-name-1 method-name-2))

(define (find-method-class method-name class env)
  (or (and (member method-name (map (lambda (m) (@ast m :name)) (@ast class :methods)))
           class)
      (and (@ast class :parent)
           (find-method-class method-name (lookup-class-type (@ast class :parent) env) env))))

(define (sig-more-specific? sig-a sig-b env)
  (all? (lambda (x) x)
        (map (lambda (x y)
               (type-more-specific? (@ast x :type) (@ast y :type) env))
             sig-a
             sig-b)))

(define (sig-equivalent? sig-a sig-b env)
  (and (= (length sig-a) (length sig-b))
       (all? (lambda (x) x)
             (map (lambda (x y) (type-equivalent? (@ast x :type)
                                                  (@ast y :type)))
                  sig-a
                  sig-b))))

;; methods - a list of type lists, a "type list" is a parameter list
;; sig - a type list
(define (find-method-by-sig sig methods env)
  (let ((candidates (filter (lambda (method)
                              (and (= (length (@ast method :param-list))
                                      (length sig))
                                   (all? (lambda (x) x)
                                         (map (lambda (x y) (type-assignable? x y env))
                                              (map (lambda (x) (@ast x :type))
                                                   (@ast method :param-list))
                                              sig))))
                            methods)))
    (if (null? candidates)
        #f
        (let loop ((best (car candidates))
                   (clones '())
                   (rest (cdr candidates)))
          (cond ((null? rest)
                 best)
                ((sig-equivalent? (@ast best :param-list)
                                  (@ast (car rest) :param-list)
                                  env)
                 (error "Two candidate methods are equivalent")
                 (loop best (cons (car rest) clone) (cdr rest)))
                ((sig-more-specific? (@ast best :param-list)
                                     (@ast (car rest) :param-list)
                                     env)
                 (loop (car rest) '() (cdr rest)))
                (else
                 (loop best clones (cdr rest))))))))

(define (check-ctor-sigs-unique ctors env)
  (map-and
   (lambda (ctor-a)
     (map-and
      (lambda (ctor-b)
        (or-error (or (eq? ctor-a ctor-b)
                      (not (sig-equivalent? (@ast ctor-a :param-list)
                                            (@ast ctor-b :param-list)
                                            env)))
                  "Constructors defined with equivalent parameter lists; see "
                  (@:line-col ctor-a) " and " (@:line-col ctor-b)))
      ctors))
   ctors))

;(define (class-get-ctors class)
;  (let ((ctors (@ast class :constructors)))
;    (if (null? ctors)
;        (list (ast-node constructor
;                        (:param-list '())
;                        (:static? #t)
;                        (:return-type (ast-node simple-type (:name "void")))
;                        (:local-vars '())
;                        (:statements '())
;                        (:ctor-generated #t)))
;        ctors)))

(define (lookup-class-type class-name env)
  (define (iter classes)
    (cond ((null? classes) #f)
          ((equal? class-name (list-ref (car classes) 0))
           (list-ref (car classes) 4))
          (else
           (iter (cdr classes)))))
  (iter (contents env)))

(define (type-method-return method)
  (@ast method :return-type))

(define (type-method-param-list method)
  (@ast method :param-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-and proc list)
  (or (null? list)
      (and (proc (car list))
           (map-and proc (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-names-unique names nodes)
  (define (pass name node names nodes)
    (cond
     ((null? names) #t)
     ((equal? name (car names))
      (or-error #f "Duplicate identifier; " 
                name 
                " defined at "
                (@:line-col node) 
                " conflicts with definition at "
                (@:line-col (car nodes))))
     (else
      (pass name node (cdr names) (cdr nodes)))))
  (define (iter names nodes)
    (or (null? names)
        (and (pass (car names)
                   (car nodes)
                   (cdr names)
                   (cdr nodes))
             (iter (cdr names)
                   (cdr nodes)))))
  (iter names nodes))

(define (check-class-names-unique classes env)
  (check-names-unique (map (lambda (x) (@ast x :class-name))
                           classes)
                      classes))

(define (check-class-hierarchy classes env)
  (map-and (lambda (class)
             (or (not (@ast class :parent))
                 (or-error (not (type-search-inheritance-hierarchy (@ast class :parent)
                                                                   (@ast class :class-name)
                                                                   env))
                           "Class " (@ast class :class-name) " is part of a circular inheritance hierarchy: "
                           (cons (@ast class :class-name) (type-get-inheritance-hierarchy (@ast class :parent) (@ast class :class-name) env)))))
           classes))

(define (check-instance-var-names-unique instance-vars env)
  (check-names-unique (map (lambda (x) (@ast x :name))
                           instance-vars)
                      instance-vars))

(define (check-method-names-unique methods env)
  (check-names-unique (map (lambda (x) (@ast x :name))
                           methods)
                      methods))

(define (check-method-sigs-orthogonal methods env)
  (or (null? methods)
      (let ((m1 (car methods)))
        (and
         (map-and (lambda (m2)
                    (or-error 
                     (not (sig-equivalent? (@ast m1 :param-list) (@ast m2 :param-list) env))
                     "The signature of method '" (@ast m1 :name) "' at " (@:line-col m1)
                     " is duplicated at " (@:line-col m2)))
                  (cdr methods))
         (check-method-sigs-orthogonal (cdr methods) env)))))

(define (check-overloaded-methods methods class env)
  (map-and (lambda (method)
             (or (not (@ast class :parent))
                 (let ((parent-method (lookup-method (@ast method :name)
                                                     (map (lambda (p) (@ast p :type))
                                                          (@ast method :param-list))
                                                     (lookup-class-type (@ast class :parent) env)
                                                     env)))
                   (or (not parent-method)
                       (or-error
                        (methods-equivalent? method parent-method)
                        "Method '" (@ast method :name) "' of class '"
                        (@ast class :class-name) "' overrides method of class '"
                        (@ast (find-method-class (@ast method :name) (lookup-class-type (@ast class :parent) env) env) :class-name)
                        "' but does not have an equivalent return type")))))
           methods))

(define (check-local-var-names-unique local-vars env)
  (check-names-unique (map (lambda (x) (@ast x :name))
                           local-vars)
                      local-vars))

(define (check-param-names-unique params env)
  (check-names-unique (map (lambda (x) (@ast x :name))
                           params)
                      params))

(define (check-type-exists type loc env)
  (or (not (equal? 'user-type (ast-node-type type)))
      (or-error (lookup-class-type (@ast type :name) env)
                "Undefined type " type "; " (@:line-col loc))))

(define (check-types-exist types locs env)
  (or (null? types)
      (and (check-type-exists (car types) (car locs) env)
           (check-types-exist (cdr types) (cdr locs) env))))

(define (check-param-types-exist params env)
  (check-types-exist (map (lambda (x) (@ast x :type))
                          params)
                     params
                     env))

(define (check-instance-var-type-exists var env)
  (check-type-exists var var env))

(define (check-local-var-types-exist locals env)
  (check-types-exist (map (lambda (x) (@ast x :type))
                          locals)
                     locals
                     env))

(define (check-return-type-exists return-type env)
  (check-type-exists return-type return-type env))

(define (check-computed-type-assignable env return-type exp-type)
  (type-assignable? return-type exp-type env))

(define (check-ident-assignable ident method class env)
  (lookup-identifier-type ident method class env))

(define (check-ident-assignable-array-type ident method class env)
  (let ((t (lookup-identifier-type ident method class env)))
    (and t (check-array-type t))))

(define (check-array-type type )
  (equal? 'array-type (ast-node-type type)))

(define (check-correct-number-args arg-list method)
  (equal? (length arg-list)
          (length (type-method-param-list method))))

(define (check-class-exists class-name env)
  (lookup-class-type class-name env))

(define (check-parent-exists class env)
  (or (not (@ast class :parent))
      (check-class-exists (@ast class :parent) env)))
