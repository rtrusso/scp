(need parse/genparse)

;; Note: By convention, nonterminals are prefixed with '@' and
;; terminals do not have a prefix.

(define csharp-grammar-rules
  '((@goal @program)
    (:rule (lambda (x) x))

    ;; @program

    (@program @main-class)
    (:rule (lambda (x) (ast-node program (:main-class x) (:class-list (list x)))))

    (@program @main-class @class-list)
    (:rule (lambda (x y) (ast-node program (:main-class x) (:class-list (cons x y)))))

    ;; @class-list

    (@class-list @class)
    (:rule (lambda (x) (list x)))

    (@class-list @class-list @class)
    (:rule (lambda (x y) (append x (list y))))

    ;; @main-class

    (@main-class class ident l-curl 
                 public static void ident l-paren string l-brak r-brak ident r-paren 
                 l-curl @statement r-curl r-curl)
    (:rule (lambda (class-1 ident-2 l-curl-3 public-4 static-5 void-6 ident-7 l-paren-8 string-9 l-brak-10 r-brak-11 ident-12 r-paren-13 l-curl-14 @statement-15 r-curl-16 r-curl-17)
             (ast-node class
                       (:class-name (token-value ident-2))
                       (:line-col (token-line-col class-1))
                       (:parent #f)
                       (:constructors '())
                       (:potential-main-class #t)
                       (:instance-vars '()) ; var block
                       (:static-vars '())
                       (:methods '())
                       (:static-methods 
                        (list (ast-node method
                                        (:name (token-value ident-7))
                                        (:line-col (token-line-col public-4))
                                        (:return-type (ast-node simple-type (:name "void")))
                                        (:param-list '())
                                        (:static? #t)
                                        (:param-list-main
                                         (list (ast-node param
                                                         (:type (ast-node array-type
                                                                          (:line-col (token-line-col string-9))
                                                                          (:element-type (ast-node user-type
                                                                                                   (:line-col (token-line-col string-9))
                                                                                                   (:name "String")))))
                                                         (:line-col (token-line-col string-9))
                                                         (:name (token-value ident-12)))))
                                        (:local-vars '())
                                        (:statements (list @statement-15))))))))

    ;; @class

    (@class class ident @class-body)
    (:rule (lambda (class ident @class-body)
             (ast-node class
                       (:line-col (token-line-col class))
                       (:class-name (token-value ident))
                       (:parent #f)
                       (:instance-vars (list-ref @class-body 0))
                       (:static-vars (list-ref @class-body 2))
                       (:methods (list-ref @class-body 1))
                       (:static-methods (list-ref @class-body 3))
                       (:constructors (list-ref @class-body 4)) )))

    (@class class ident colon ident @class-body)
    (:rule (lambda (class ident-name colon ident-parent @class-body)
             (ast-node class
                       (:line-col (token-line-col class))
                       (:class-name (token-value ident-name))
                       (:parent (token-value ident-parent))
                       (:instance-vars (list-ref @class-body 0))
                       (:static-vars (list-ref @class-body 2))
                       (:methods (list-ref @class-body 1))
                       (:static-methods (list-ref @class-body 3))
                       (:constructors (list-ref @class-body 4)) )))

    ;; @class-body

    (@class-body l-curl @class-statement-block r-curl)
    (:rule (lambda (x @class-statement-block y)
             (let ((vars (filter-map (lambda (x) 
                                       (and (eqv? (car x) 'var)
                                            (cdr x)))
                                 @class-statement-block))
                   (s-vars (filter-map (lambda (x) 
                                         (and (eqv? (car x) 'static-var)
                                              (cdr x)))
                                       @class-statement-block))
                   (methods (filter-map (lambda (x) (and (eqv? (car x) 'method)
                                                         (not (@ast (cdr x) :static?))
                                                         (cdr x)))
                                        @class-statement-block))
                   (s-methods (filter-map (lambda (x) (and (eqv? (car x) 'method)
                                                           (@ast (cdr x) :static?)
                                                           (cdr x)))
                                          @class-statement-block))
                   (ctors (filter-map (lambda (x) (and (eqv? 'ctor (car x))
                                                       (cdr x)))
                                      @class-statement-block)))
               (list vars methods s-vars s-methods ctors))))

    (@class-body l-curl r-curl)
    (:rule (lambda (x y)
             (list '() 
                   '() 
                   '() )))

    (@class-statement-block @class-statement)
    (:rule (lambda (x) (list x)))

    (@class-statement-block @class-statement-block @class-statement)
    (:rule (lambda (x y) (append x (list y))))

    (@class-statement @var)
    (:rule (lambda (@var) (cons 'var @var)))

    (@class-statement @static-var)
    (:rule (lambda (@static-var) (cons 'static-var @static-var)))

    (@class-statement @method)
    (:rule (lambda (@method) (cons 'method @method)))

    (@class-statement @constructor)
    (:rule (lambda (@constructor) (cons 'ctor @constructor)))

    ;; @var-block

    (@var-block @var)
    (:rule (lambda (v)
             (list v)))

    (@var-block @var-block @var)
    (:rule (lambda (@var-block @var)
             (append @var-block (list @var))))

    ;; @var

    (@var @type ident s-colon)
    (:rule (lambda (@type ident s-colon)
             (ast-node var
                       (:line-col (@ast @type :line-col))
                       (:name (token-value ident))
                       (:type @type))))

    ;; @static-var

    (@static-var static @type ident s-colon)
    (:rule (lambda (x @type ident s-colon)
             (ast-node static-var
                       (:line-col (token-line-col x))
                       (:name (token-value ident))
                       (:type @type))))

    ;; @constructor

    (@constructor public ident l-paren r-paren @method-body)
    (:rule (lambda (x ident y z @method-body)
             (ast-node constructor
                       (:line-col (list-ref x 2))
                       (:param-list '())
                       (:static? #t)
                       (:return-type (ast-node user-type (:name (token-value ident))))
                       (:local-vars (list-ref @method-body 0))
                       (:statements (list-ref @method-body 1)) ) ))

    (@constructor public ident l-paren @param-list r-paren @method-body)
    (:rule (lambda (--x ident --y @param-list --z @method-body)
             (ast-node constructor
                       (:line-col (list-ref --x 2))
                       (:param-list @param-list)
                       (:static? #t)
                       (:return-type (ast-node user-type (:name (token-value ident))))
                       (:local-vars (list-ref @method-body 0))
                       (:statements (list-ref @method-body 1)) ) ))

    ;; @method

    (@method @method-start r-paren @method-body)
    (:rule (lambda (@method-start r-paren @method-body)
             (ast-node method
                       (:line-col (list-ref @method-start 2))
                       (:name (list-ref @method-start 1))
                       (:param-list '())
                       (:return-type (list-ref @method-start 0))
                       (:static? (list-ref @method-start 3))
                       (:local-vars (list-ref @method-body 0))
                       (:statements (list-ref @method-body 1)))))

    (@method @method-start @param-list r-paren @method-body)
    (:rule (lambda (@method-start @param-list r-paren @method-body)
             (ast-node method
                       (:line-col (list-ref @method-start 2))
                       (:name (list-ref @method-start 1))
                       (:param-list @param-list)
                       (:return-type (list-ref @method-start 0))
                       (:static? (list-ref @method-start 3))
                       (:local-vars (list-ref @method-body 0))
                       (:statements (list-ref @method-body 1)))))

    (@method --sasm-impl public static @type @method-ident l-paren @param-list r-paren s-colon)
    (:rule (lambda (--sasm-impl --public --static @type @method-ident --l-paren @param-list --r-paren --s-colon)
             (ast-node method
                       (:sasm-impl-no-codegen #t)
                       (:line-col (token-line-col --sasm-impl))
                       (:name (token-value @method-ident))
                       (:param-list @param-list)
                       (:return-type @type)
                       (:static? #t)
                       (:local-vars '())
                       (:statements '()))))

    (@method --sasm-impl public static @type @method-ident l-paren @param-list r-paren = ident s-colon)
    (:rule (lambda (--sasm-impl --public --static @type method-name@method-ident --l-paren @param-list --r-paren --= sasm-ident@method-ident --s-colon)
             (ast-node method
                       (:sasm-impl-no-codegen #t)
                       (:static-method-alternate-identifier (token-value sasm-ident@method-ident))
                       (:line-col (token-line-col --sasm-impl))
                       (:name (token-value method-name@method-ident))
                       (:param-list @param-list)
                       (:return-type @type)
                       (:static? #t)
                       (:local-vars '())
                       (:statements '()))))

    ;; @method-start

    (@method-start virtual public @type @method-ident l-paren)
    (:rule (lambda (x y @type ident z)
             (list @type (token-value ident) (token-line-col x) #f)))

    (@method-start override public @type @method-ident l-paren)
    (:rule (lambda (x y @type ident z)
             (list @type (token-value ident) (token-line-col x) #f)))

    (@method-start public static @type @method-ident l-paren)
    (:rule (lambda (x y @type ident z)
             (list @type (token-value ident) (token-line-col x) #t)))

    (@method-ident ident)
    (:rule (lambda (x) x))

    (@method-ident length)
    (:rule (lambda (x) x))

    ;; @param-list

    (@param-list @param)
    (:rule (lambda (x) (list x)))

    (@param-list @param-list comma @param)
    (:rule (lambda (a comma b) (append a (list b))))

    ;; @param

    (@param @type ident)
    (:rule (lambda (@type ident)
             (ast-node param
                       (:line-col (@ast @type :line-col))
                       (:type @type)
                       (:name (token-value ident)))))

    ;; @method-body

    (@method-body l-curl @var-block @statement-block r-curl)
    (:rule (lambda (l-curl @var-block @statement-block r-curl)
             (list @var-block
                   @statement-block)))

    (@method-body l-curl @statement-block r-curl)
    (:rule (lambda (l-curl @statement-block r-curl)
             (list '()
                   @statement-block)))

    (@method-body l-curl r-curl)
    (:rule (lambda (l-curl r-curl)
             (list '() '())))

    ;; @statement-block

    (@statement-block @statement)
    (:rule (lambda (x) (list x)))

    (@statement-block @statement-block @statement)
    (:rule (lambda (x y) (append x (list y))))

    ;; @type

    (@type int l-brak r-brak)
    (:rule (lambda (x y z)
             (ast-node array-type
                       (:line-col (token-line-col x))
                       (:element-type (ast-node simple-type 
                                                (:name "int"))))))

    (@type char l-brak r-brak)
    (:rule (lambda (x y z)
             (ast-node array-type
                       (:line-col (token-line-col x))
                       (:element-type (ast-node simple-type 
                                                (:name "char"))))))

    (@type string l-brak r-brak)
    (:rule (lambda (x y z)
             (ast-node array-type
                       (:line-col (token-line-col x))
                       (:element-type (ast-node user-type
                                                (:name "String"))))))

    (@type ident l-brak r-brak)
    (:rule (lambda (ident y z)
             (ast-node array-type
                       (:line-col (token-line-col ident))
                       (:element-type (ast-node user-type
                                                (:name (token-value ident)))))))

    (@type boolean)
    (:rule (lambda (x)
             (ast-node simple-type 
                       (:line-col (token-line-col x))
                       (:name "boolean"))))

    (@type int)
    (:rule (lambda (x)
             (ast-node simple-type 
                       (:line-col (token-line-col x))
                       (:name "int"))))

    (@type string)
    (:rule (lambda (x)
             (ast-node user-type
                       (:line-col (token-line-col x))
                       (:name "String"))))

    (@type char)
    (:rule (lambda (x)
             (ast-node simple-type
                       (:line-col (token-line-col x))
                       (:name "char"))))

    (@type ident)
    (:rule (lambda (ident)
             (ast-node user-type 
                       (:line-col (token-line-col ident))
                       (:name (token-value ident)))))

    ;; @statement

    (@statement l-curl r-curl)
    (:rule (lambda (x y)
             (ast-node compound-statement
                       (:line-col (token-line-col x))
                       (:statements '()))))

    (@statement l-curl @statement-block r-curl)
    (:rule (lambda (x @statement-block z)
             (ast-node compound-statement
                       (:line-col (token-line-col x))
                       (:statements @statement-block))))

    (@statement if l-paren @exp r-paren @statement else @statement)
    (:rule (lambda (if-tok l-paren @exp r-paren @statement-consequent else @statement-antecedent)
             (ast-node if-statement
                       (:line-col (token-line-col if-tok))
                       (:condition @exp)
                       (:consequent @statement-consequent)
                       (:antecedent @statement-antecedent))))

    (@statement while l-paren @exp r-paren @statement)
    (:rule (lambda (while l-paren @exp r-paren @statement)
             (ast-node while-statement
                       (:line-col (token-line-col while))
                       (:condition @exp)
                       (:body @statement))))

    (@statement system-console-writeline l-paren @exp r-paren s-colon)
    (:rule (lambda (x y @exp z w)
             (ast-node print-statement
                       (:line-col (token-line-col x))
                       (:exp @exp))))

    (@statement ident = @exp s-colon)
    (:rule (lambda (ident x @exp y)
             (ast-node assignment-statement
                       (:line-col (token-line-col ident))
                       (:ident (token-value ident))
                       (:exp @exp))))

    (@statement ident l-brak @exp r-brak = @exp s-colon)
    (:rule (lambda (ident x @index-exp y z @value-exp w)
             (ast-node array-assignment-statement
                       (:line-col (token-line-col ident))
                       (:ident (token-value ident))
                       (:index-exp @index-exp)
                       (:value-exp @value-exp))))

    (@statement return @exp s-colon)
    (:rule (lambda (r @exp s)
             (ast-node return-statement
                       (:line-col (token-line-col r))
                       (:exp @exp))))

    (@statement return s-colon)
    (:rule (lambda (r s)
             (ast-node return-statement
                       (:line-col (token-line-col r)))))

    ;; @exp

    (@exp @exp-logical)
    (:rule (lambda (x) x))

    ;; @exp-logical

    (@exp-logical @exp-comparison)
    (:rule (lambda (x) x))

    (@exp-logical @exp-logical && @exp-comparison)
    (:rule (lambda (@exp-logical foo @exp-comparison)
             (ast-node and-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-logical)
                       (:right @exp-comparison))))

    ;; @exp-comparison

    (@exp-comparison @exp-term)
    (:rule (lambda (x) x))

    (@exp-comparison @exp-term < @exp-term)
    (:rule (lambda (@exp-term-left foo @exp-term-right)
             (ast-node less-than-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term-left)
                       (:right @exp-term-right))))

    (@exp-comparison @exp-term == @exp-term)
    (:rule (lambda (@exp-term-left foo @exp-term-right)
             (ast-node equal-to-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term-left)
                       (:right @exp-term-right))))

    ;; @exp-term

    (@exp-term @exp-factor)
    (:rule (lambda (x) x))

    (@exp-term @exp-term + @exp-factor)
    (:rule (lambda (@exp-term foo @exp-factor)
             (ast-node add-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term)
                       (:right @exp-factor))))

    (@exp-term @exp-term - @exp-factor)
    (:rule (lambda (@exp-term foo @exp-factor)
             (ast-node subtract-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term)
                       (:right @exp-factor))))

    (@exp-factor @exp-factor pipe @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-or)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))

    (@exp-factor @exp-factor & @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-and)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))

    (@exp-factor @exp-factor ^ @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-xor)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))

    ;; @exp-factor

    (@exp-factor @exp-elem)
    (:rule (lambda (x) x))

    (@exp-factor @exp-factor * @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node multiply-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))

    (@exp-factor @exp-factor >> @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-arith-rshift)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))

    (@exp-factor @exp-factor << @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-lshift)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))

    (@exp-factor @exp-factor >>> @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-rshift)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))

    ;; @exp-elem

    (@exp-elem @exp-array)
    (:rule (lambda (x) x))

    (@exp-elem @exp-ref)
    (:rule (lambda (x) x))

    (@exp-elem bang @exp-elem)
    (:rule (lambda (foo @exp-elem)
             (ast-node not-expression
                       (:line-col (token-line-col foo))
                       (:exp @exp-elem))))

    (@exp-elem ~ @exp-elem)
    (:rule (lambda (foo @exp-elem)
             (ast-node int-unop-expression
                       (:op 'bit-not)
                       (:line-col (token-line-col foo))
                       (:exp @exp-elem))))

    ;; @exp-array

    (@exp-array @exp-ref l-brak @exp r-brak)
    (:rule (lambda (@exp-ref foo-1 @exp foo-2)
             (ast-node array-ref-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:array-exp @exp-ref)
                       (:index-exp @exp))))

    ;; @exp-ref

    (@exp-ref @exp-ref dot length)
    (:rule (lambda (@exp-ref foo-1 foo-2)
             (ast-node length-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:exp @exp-ref))))

    (@exp-ref @exp-ref dot @method-ident l-paren r-paren)
    (:rule (lambda (@exp-ref foo-1 ident foo-2 foo-3)
             (ast-node method-call-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:object @exp-ref)
                       (:method-name (token-value ident))
                       (:arg-list '()))))

    (@exp-ref @exp-ref dot @method-ident l-paren @exp-list r-paren)
    (:rule (lambda (@exp-ref foo-1 ident foo-2 @exp-list foo-3)
             (ast-node method-call-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:object @exp-ref)
                       (:method-name (token-value ident))
                       (:arg-list @exp-list))))

    (@exp-ref @exp-trivial)
    (:rule (lambda (x) x))

    ;; @exp-trivial

    (@exp-trivial number)
    (:rule (lambda (x)
             (ast-node number-constant-expression
                       (:line-col (token-line-col x))
                       (:value (token-value x)))))

    (@exp-trivial true)
    (:rule (lambda (x)
             (ast-node boolean-constant-expression
                       (:line-col (token-line-col x))
                       (:value "true")
                       (:scheme-value #t))))

    (@exp-trivial false)
    (:rule (lambda (y)
             (ast-node boolean-constant-expression
                       (:line-col (token-line-col y))
                       (:value "false")
                       (:scheme-value #f))))

    (@exp-trivial char-literal)
    (:rule (lambda (c)
             (ast-node char-constant-expression
                       (:line-col (token-line-col c))
                       (:value (token-value c)))))

    (@exp-trivial string-literal)
    (:rule (lambda (s)
             (ast-node new-object-expression
                       (:line-col (token-line-col s))
                       (:class-name "String")
                       (:arg-list (list 
                                   (ast-node string-constant-expression
                                             (:line-col (token-line-col s))
                                             (:value (token-value s))))))))

    (@exp-trivial ident)
    (:rule (lambda (y)
             (ast-node variable-expression
                       (:line-col (token-line-col y))
                       (:ident (token-value y)))))

    (@exp-trivial this)
    (:rule (lambda (y)
             (ast-node this-expression
                       (:line-col (token-line-col y)))))

    (@exp-trivial new int l-brak @exp r-brak)
    (:rule (lambda (foo-1 foo-2 foo-3 @exp foo-4)
             (ast-node new-array-expression
                       (:line-col (token-line-col foo-1))
                       (:element-type (ast-node simple-type
                                                (:name "int")))
                       (:length @exp))))

    (@exp-trivial new char l-brak @exp r-brak)
    (:rule (lambda (foo-1 foo-2 foo-3 @exp foo-4)
             (ast-node new-array-expression
                       (:line-col (token-line-col foo-1))
                       (:element-type (ast-node simple-type
                                                (:name "char")))
                       (:length @exp))))

    (@exp-trivial new string l-brak @exp r-brak)
    (:rule (lambda (foo-1 foo-2 foo-3 @exp foo-4)
             (ast-node new-array-expression
                       (:line-col (token-line-col foo-1))
                       (:element-type (ast-node user-type
                                                (:name "String")))
                       (:length @exp))))

    (@exp-trivial new ident l-brak @exp r-brak)
    (:rule (lambda (foo-1 @ident foo-3 @exp foo-4)
             (ast-node new-array-expression
                       (:line-col (token-line-col foo-1))
                       (:element-type (ast-node user-type
                                                (:name (token-value @ident))))
                       (:length @exp))))

    (@exp-trivial new ident l-paren r-paren)
    (:rule (lambda (foo-1 ident foo-2 foo-3)
             (ast-node new-object-expression
                       (:line-col (token-line-col foo-1))
                       (:class-name (token-value ident))
                       (:arg-list '()))))
             

    (@exp-trivial new ident l-paren @exp-list r-paren)
    (:rule (lambda (foo:new ident foo:l-paren @exp-list foo:r-paren)
             (ast-node new-object-expression
                       (:line-col (token-line-col foo:new))
                       (:class-name (token-value ident))
                       (:arg-list @exp-list))))

    (@exp-trivial new string l-paren @exp-list r-paren)
    (:rule (lambda (foo:new ident foo:l-paren @exp-list foo:r-paren)
             (ast-node new-object-expression
                       (:line-col (token-line-col foo:new))
                       (:class-name "String")
                       (:arg-list @exp-list))))

    (@exp-trivial l-paren @exp r-paren)
    (:rule (lambda (foo-1 @exp foo-2)
             @exp))

    ;; @exp-list

    (@exp-list @exp)
    (:rule (lambda (x) (list x)))

    (@exp-list @exp-list comma @exp)
    (:rule (lambda (x y z) (append x (list z))))

    ))

(define csharp-grammar 
  (grammar-rules-filter-productions csharp-grammar-rules))

(define csharp-graph (build-lr0-state-transition-graph csharp-grammar))
(define csharp-sr-table (generate-lr0-table csharp-grammar csharp-graph csharp-grammar-rules))
(define csharp-goto-table (generate-lr0-goto-table csharp-grammar csharp-graph))
(define csharp-terminals (grammar-terminals csharp-grammar))
(define csharp-nonterminals (grammar-nonterminals csharp-grammar))
(define csharp-action-table (grammar-rules-build-action-table csharp-grammar-rules))

(if (file-exists? "csharp-parser.scm")
    (delete-file "csharp-parser.scm"))

(call-with-output-file "csharp-parser.scm"
  (lambda (port)
    (for-each
     (lambda (code) (write code port) (newline port))
     `((need parse/parse)
       (define action-table ,csharp-action-table)
       (define (csharp-parse-input input)
         (run-parse (quote ,csharp-grammar)
                    (quote ,(append csharp-terminals (list '$)))
                    (quote ,csharp-nonterminals)
                    (quote ,csharp-sr-table)
                    (quote ,csharp-goto-table)
                    action-table
                    input))
       (define (csharp-parse-port input-port)
         (csharp-parse-input (read input-port)))
       (define (csharp-parse-file filename)
         (call-with-input-file filename
           csharp-parse-port))))))
