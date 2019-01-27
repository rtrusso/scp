(need parse/genparse)

;; Note: By convention, nonterminals are prefixed with '@' and
;; terminals do not have a prefix.

(define minijava-grammar-rules
  '((@goal @program)
    (:rule (lambda (x) x))
    (:java-rule ((program Program))
                Program
                "program")

    ;; @program

    (@program @main-class)
    (:rule (lambda (x) (ast-node program (:main-class x) (:class-list (list x)))))
    (:java-rule ((mainClass MainClass))
                Program
                "new Program(new ClassList(mainClass))")

    (@program @main-class @class-list)
    (:rule (lambda (x y) (ast-node program (:main-class x) (:class-list (cons x y)))))
    (:java-rule ((mainClass MainClas) (classList ClassList))
                Program
                "new Program(new ClassList(mainClass).append(classList))")

    ;; @class-list

    (@class-list @class)
    (:rule (lambda (x) (list x)))
    (:java-rule ((_class _Class))
                ClassList
                "new ClassList(_class)")

    (@class-list @class-list @class)
    (:rule (lambda (x y) (append x (list y))))
    (:java-rule ((classList ClassList) (_class _Class))
                ClassList
                "classList.append(_class)")

    ;; @main-class

    (@main-class class ident l-curl 
                 public static void ident l-paren ident l-brak r-brak ident r-paren 
                 l-curl @statement r-curl r-curl)
    (:rule (lambda (class-1 ident-2 l-curl-3 public-4 static-5 void-6 ident-7 l-paren-8 ident-9 l-brak-10 r-brak-11 ident-12 r-paren-13 l-curl-14 @statement-15 r-curl-16 r-curl-17)
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
                                                                          (:line-col (token-line-col ident-9))
                                                                          (:element-type (ast-node simple-type
                                                                                                   (:line-col (token-line-col ident-9))
                                                                                                   (:name (token-value ident-9))))))
                                                         (:line-col (token-line-col ident-9))
                                                         (:name (token-value ident-12)))))
                                        (:local-vars '())
                                        (:statements (list @statement-15))))))))
    (:java-rule ((classToken_1 Token)
                 (mainClassName_2 Token)
                 (leftCurl_3 Token)
                 (public_4 Token)
                 (static_5 Token)
                 (void_6 Token)
                 (mainMethodIdentifier_7 Token)
                 (leftParen_8 Token)
                 (paramTypeIdentifier_9 Token)
                 (leftBracket_10 Token)
                 (rightBracket_11 Token)
                 (paramIdentifier_12 Token)
                 (rightParen_13 Token)
                 (leftCurl_14 Token)
                 (statement_15 Statement)
                 (rightCurl_16 Token)
                 (rightCurl_17 Token))
                MainClass
                "new MainClass(mainClassName_2,
                               mainMethodIdentifier_7,
                               paramTypeIdentifier_9,
                               paramIdentifier_12,
                               statement_15)")

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
    (:java-rule ((classToken Token)
                 (className Token)
                 (classBody ClassBody))
                _Class
                "new _Class(className, null, classBody)")

    (@class class ident extends ident @class-body)
    (:rule (lambda (class ident-name extends ident-parent @class-body)
             (ast-node class
                       (:line-col (token-line-col class))
                       (:class-name (token-value ident-name))
                       (:parent (token-value ident-parent))
                       (:instance-vars (list-ref @class-body 0))
                       (:static-vars (list-ref @class-body 2))
                       (:methods (list-ref @class-body 1))
                       (:static-methods (list-ref @class-body 3))
                       (:constructors (list-ref @class-body 4)) )))
    (:java-rule ((classToken Token)
                 (className Token)
                 (extendsToken Toke)
                 (parentName Token)
                 (classBody ClassBody))
                _Class
                "new _Class(className, parentName, classBody)")

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
    (:java-rule ((leftCurl Token)
                 (classParts ClassParts)
                 (rightCurl Token))
                ClassBody
                "new ClassBody(classParts)")

    (@class-body l-curl r-curl)
    (:rule (lambda (x y)
             (list '() 
                   '() 
                   '() )))
    (:java-rule ((leftCurl Token)
                 (rightCurl Token))
                ClassBody
                "new ClassBody()")

    (@class-statement-block @class-statement)
    (:rule (lambda (x) (list x)))
    (:java-rule ((classPart ClassPart))
                ClassParts
                "new ClassParts(classPart)")

    (@class-statement-block @class-statement-block @class-statement)
    (:rule (lambda (x y) (append x (list y))))
    (:java-rule ((classParts ClassParts)
                 (classPart ClassPart))
                ClassParts
                "classParts.append(classPart)")

    (@class-statement @var)
    (:rule (lambda (@var) (cons 'var @var)))
    (:java-rule ((v VariableDefinition))
                ClassPart
                "new FieldDefinition(v)")

    (@class-statement @static-var)
    (:rule (lambda (@static-var) (cons 'static-var @static-var)))
    (:java-rule ((sv StaticVariableDefinition))
                ClassPart
                "new StaticFieldDefinition(sv)")

    (@class-statement @method)
    (:rule (lambda (@method) (cons 'method @method)))
    (:java-rule ((method MethodDefition))
                ClassPart
                "method")

    (@class-statement @constructor)
    (:rule (lambda (@constructor) (cons 'ctor @constructor)))
    (:java-rule ((constructor ConstructorDefinition))
                ClassPart
                "constructor")

    ;; @var-block

    (@var-block @var)
    (:rule (lambda (v)
             (list v)))
    (:java-rule ((v VariableDefinition))
                VariableBlock
                "new VariableBlock(v)")

    (@var-block @var-block @var)
    (:rule (lambda (@var-block @var)
             (append @var-block (list @var))))
    (:java-rule ((vb VariableBlock)
                 (v VariableDefinition))
                VariableBlock
                "VariableBlock.append(vb, v)")

    ;; @var

    (@var @type ident s-colon)
    (:rule (lambda (@type ident s-colon)
             (ast-node var
                       (:line-col (@ast @type :line-col))
                       (:name (token-value ident))
                       (:type @type))))
    (:java-rule ((type TypeSpecifier)
                 (identifier Token)
                 (semicolon Token))
                VariableDefinition
                "new VariableDefinition(type, identifier)")

    ;; @static-var

    (@static-var static @type ident s-colon)
    (:rule (lambda (x @type ident s-colon)
             (ast-node static-var
                       (:line-col (token-line-col x))
                       (:name (token-value ident))
                       (:type @type))))
    (:java-rule ((staticToken Token)
                 (type TypeSpecifier)
                 (identifier Token)
                 (semicolon Token))
                VariableDefintion
                "new VariableDefinition(type, identifier)")

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
    (:java-rule ((publicToken Token)
                 (identifier Token)
                 (leftParen Token)
                 (rightParen Token)
                 (body MethodBody))
                ConstructorDefinition
                "new ConstructorDefinition(identifier,
                                           new ParameterList(),
                                           body)")

    (@constructor public ident l-paren @param-list r-paren @method-body)
    (:rule (lambda (--x ident --y @param-list --z @method-body)
             (ast-node constructor
                       (:line-col (list-ref --x 2))
                       (:param-list @param-list)
                       (:static? #t)
                       (:return-type (ast-node user-type (:name (token-value ident))))
                       (:local-vars (list-ref @method-body 0))
                       (:statements (list-ref @method-body 1)) ) ))
    (:java-rule ((publicToken Token)
                 (identifier Token)
                 (leftParen Token)
                 (parameterList ParameterList)
                 (rightParen Token)
                 (body MethodBody))
                ConstructorDefinition
                "new ConstructorDefinition(identifier,
                                           parameterList,
                                           body)")

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
    (:java-rule ((attributes MethodAttributes)
                 (rightParen Token)
                 (body MethodBody))
                MethodDefinition
                "new MethodDefinition(attributes, new ParameterList(), body)")

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
    (:java-rule ((attributes MethodAttributes)
                 (parameters ParameterList)
                 (rightParen Token)
                 (body MethodBody))
                MethodDefinition
                "new MethodDefinition(attributes, parameters, body)")

;;     (@method --sasm-impl public static @type @method-ident l-paren @param-list r-paren s-colon)
;;     (:rule (lambda (--sasm-impl --public --static @type @method-ident --l-paren @param-list --r-paren --s-colon)
;;              (ast-node method
;;                        (:sasm-impl-no-codegen #t)
;;                        (:line-col (token-line-col --sasm-impl))
;;                        (:name (token-value @method-ident))
;;                        (:param-list @param-list)
;;                        (:return-type @type)
;;                        (:static? #t)
;;                        (:local-vars '())
;;                        (:statements '()))))

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
    (:java-rule ((sasmImplToken Token)
                 (publicToken Token)
                 (staticToken Token)
                 (returnType TypeSpecifier)
                 (methodIdentifier Token)
                 (leftParen Token)
                 (parameters ParameterList)
                 (rightParen Token)
                 (equals Token)
                 (sasmIdentifier Token)
                 (semicolon Token))
                MethodDefinition
                "new NativeMethodDefinition(returnType, methodIdentifier, parameters, sasmIdentifier)")

    (@method --sasm-impl public static @type @method-ident l-paren r-paren = ident s-colon)
    (:rule (lambda (--sasm-impl --public --static @type method-name@method-ident --l-paren --r-paren --= sasm-ident@method-ident --s-colon)
             (ast-node method
                       (:sasm-impl-no-codegen #t)
                       (:static-method-alternate-identifier (token-value sasm-ident@method-ident))
                       (:line-col (token-line-col --sasm-impl))
                       (:name (token-value method-name@method-ident))
                       (:param-list '())
                       (:return-type @type)
                       (:static? #t)
                       (:local-vars '())
                       (:statements '()))))
    (:java-rule ((sasmImplToken Token)
                 (publicToken Token)
                 (staticToken Token)
                 (returnType TypeSpecifier)
                 (methodIdentifier Token)
                 (leftParen Token)
                 (rightParen Token)
                 (equals Token)
                 (sasmIdentifier Token)
                 (semicolon Token))
                MethodDefinition
                "new NativeMethodDefinition(returnType, methodIdentifier, new ParameterList(), sasmIdentifier)")

    ;; @method-start

    (@method-start public @type @method-ident l-paren)
    (:rule (lambda (x @type ident y)
             (list @type (token-value ident) (token-line-col x) #f)))
    (:java-rule ((publicToken Token)
                 (returnType TypeSpecifier)
                 (name Token)
                 (leftParen Token))
                MethodAttributes
                "new MethodAttributes(returnType, name)")

    (@method-start public static @type @method-ident l-paren)
    (:rule (lambda (x y @type ident z)
             (list @type (token-value ident) (token-line-col x) #t)))
    (:java-rule ((publicToken Token)
                 (staticToken Token)
                 (returnType TypeSpecifier)
                 (name Token)
                 (leftParen Token))
                MethodAttributes
                "new StaticMethodAttributes(returnType, name)")

    (@method-ident ident)
    (:rule (lambda (x) x))
    (:java-rule ((identifier Token))
                Token
                "identifier")

    (@method-ident length)
    (:rule (lambda (x) x))
    (:java-rule ((identifier Token))
                Token
                "identifier")

    ;; @param-list

    (@param-list @param)
    (:rule (lambda (x) (list x)))
    (:java-rule ((parameter Parameter))
                ParameterList
                "new ParameterList(parameter)")

    (@param-list @param-list comma @param)
    (:rule (lambda (a comma b) (append a (list b))))
    (:java-rule ((parameterList ParameterList)
                 (comma Token)
                 (parameter Parameter))
                ParameterList
                "parameterList.append(parameter)")

    ;; @param

    (@param @type ident)
    (:rule (lambda (@type ident)
             (ast-node param
                       (:line-col (@ast @type :line-col))
                       (:type @type)
                       (:name (token-value ident)))))
    (:java-rule ((type TypeSpecifier)
                 (identifier Token))
                Parameter
                "new Parameter(type, identifier)")

    ;; @method-body

    (@method-body l-curl @var-block @statement-block r-curl)
    (:rule (lambda (l-curl @var-block @statement-block r-curl)
             (list @var-block
                   @statement-block)))
    (:java-rule ((leftCurly Token)
                 (variables VariableBlock)
                 (statements StatementBlock)
                 (rightCurly Token))
                MethodBody
                "new MethodBody(variables, statements)")

    (@method-body l-curl @statement-block r-curl)
    (:rule (lambda (l-curl @statement-block r-curl)
             (list '()
                   @statement-block)))
    (:java-rule ((leftCurly Token)
                 (statements StatementBlock)
                 (rightCurly Token))
                MethodBody
                "new MethodBody(new VariableBlock(), statements)")

    (@method-body l-curl r-curl)
    (:rule (lambda (l-curl r-curl)
             (list '() '())))
    (:java-rule ((leftCurly Token)
                 (rightCurly Token))
                MethodBody
                "new MethodBody(new VariableBlock(), new StatementBlock())")

    ;; @statement-block

    (@statement-block @statement)
    (:rule (lambda (x) (list x)))
    (:java-rule ((statement Statement))
                StatementBlock
                "new StatementBlock(statement)")

    (@statement-block @statement-block @statement)
    (:rule (lambda (x y) (append x (list y))))
    (:java-rule ((statementBlock StatementBlock)
                 (statement Statement))
                "statementBlock.append(statement)")

    ;; @type

    (@type int l-brak r-brak)
    (:rule (lambda (x y z)
             (ast-node array-type
                       (:line-col (token-line-col x))
                       (:element-type (ast-node simple-type 
                                                (:name "int"))))))
    (:java-rule ((name Token)
                 (leftBracket Token)
                 (rightBracket Token))
                TypeSpecifier
                "new ArrayTypeSpecifier(new SimpleTypeSpecifier(name))")

    (@type char l-brak r-brak)
    (:rule (lambda (x y z)
             (ast-node array-type
                       (:line-col (token-line-col x))
                       (:element-type (ast-node simple-type 
                                                (:name "char"))))))
    (:java-rule ((name Token)
                 (leftBracket Token)
                 (rightBracket Token))
                TypeSpecifier
                "new ArrayTypeSpecifier(new SimpleTypeSpecifier(name))")


    (@type ident l-brak r-brak)
    (:rule (lambda (ident y z)
             (ast-node array-type
                       (:line-col (token-line-col ident))
                       (:element-type (ast-node user-type
                                                (:name (token-value ident)))))))
    (:java-rule ((name Token)
                 (leftBracket Token)
                 (rightBracket Token))
                TypeSpecifier
                "new ArrayTypeSpecifier(new SimpleTypeSpecifier(name))")


    (@type boolean)
    (:rule (lambda (x)
             (ast-node simple-type 
                       (:line-col (token-line-col x))
                       (:name "boolean"))))
    (:java-rule ((name Token))
                TypeSpecifier
                "new SimpleTypeSpecifier(name)")

    (@type int)
    (:rule (lambda (x)
             (ast-node simple-type 
                       (:line-col (token-line-col x))
                       (:name "int"))))
    (:java-rule ((name Token))
                TypeSpecifier
                "new SimpleTypeSpecifier(name)")

    (@type char)
    (:rule (lambda (x)
             (ast-node simple-type
                       (:line-col (token-line-col x))
                       (:name "char"))))
    (:java-rule ((name Token))
                TypeSpecifier
                "new SimpleTypeSpecifier(name)")

    (@type ident)
    (:rule (lambda (ident)
             (ast-node user-type 
                       (:line-col (token-line-col ident))
                       (:name (token-value ident)))))
    (:java-rule ((name Token))
                TypeSpecifier
                "new SimpleTypeSpecifier(name)")

    ;; @statement

    (@statement l-curl r-curl)
    (:rule (lambda (x y)
             (ast-node compound-statement
                       (:line-col (token-line-col x))
                       (:statements '()))))
    (:java-rule ((leftCurly Token)
                 (rightCurly Token))
                Statement
                "new CompoundStatement()")

    (@statement l-curl @statement-block r-curl)
    (:rule (lambda (x @statement-block z)
             (ast-node compound-statement
                       (:line-col (token-line-col x))
                       (:statements @statement-block))))
    (:java-rule ((leftCurly Token)
                 (block StatementBlock)
                 (rightCurly Token))
                Statement
                "new CompoundStatement(block)")

    (@statement if l-paren @exp r-paren @statement else @statement)
    (:rule (lambda (if-tok l-paren @exp r-paren @statement-consequent else @statement-antecedent)
             (ast-node if-statement
                       (:line-col (token-line-col if-tok))
                       (:condition @exp)
                       (:consequent @statement-consequent)
                       (:antecedent @statement-antecedent))))
    (:java-rule ((ifToken Token)
                 (leftParen Token)
                 (predicate Expression)
                 (rightParen Token)
                 (consequent Statement)
                 (elseToken Token)
                 (antecedent Statement))
                Statement
                "new ConditionalStatement(predicate, consequent, antecedent)")

    (@statement while l-paren @exp r-paren @statement)
    (:rule (lambda (while l-paren @exp r-paren @statement)
             (ast-node while-statement
                       (:line-col (token-line-col while))
                       (:condition @exp)
                       (:body @statement))))
    (:java-rule ((whileToken Token)
                 (leftParen Token)
                 (predicate Expression)
                 (rightParen Token)
                 (body Statement))
                Statement
                "new WhileLoopStatement(predicate, body)")


    (@statement system-out-println l-paren @exp r-paren s-colon)
    (:rule (lambda (x y @exp z w)
             (ast-node print-statement
                       (:line-col (token-line-col x))
                       (:exp @exp))))
    (:java-rule ((sopToken Token)
                 (leftParen Token)
                 (argument Expression)
                 (rightParen Token)
                 (semicolon Token))
                Statement
                "new PrintStatement(argument)")

    (@statement ident = @exp s-colon)
    (:rule (lambda (ident x @exp y)
             (ast-node assignment-statement
                       (:line-col (token-line-col ident))
                       (:ident (token-value ident))
                       (:exp @exp))))
    (:java-rule ((identifier Token)
                 (equals Token)
                 (value Expression)
                 (semicolon Token))
                Statement
                "new AssignmentStatement(identifier, value)")

    (@statement ident l-brak @exp r-brak = @exp s-colon)
    (:rule (lambda (ident x @index-exp y z @value-exp w)
             (ast-node array-assignment-statement
                       (:line-col (token-line-col ident))
                       (:ident (token-value ident))
                       (:index-exp @index-exp)
                       (:value-exp @value-exp))))
    (:java-rule ((identifier Token)
                 (leftBracket Token)
                 (indexExpression Expression)
                 (rightBracket Token)
                 (equals Token)
                 (value Expression)
                 (semicolon Token))
                Statement
                "new ArrayAssignmentStatement(identifier, indexExpression, value)")

    (@statement return @exp s-colon)
    (:rule (lambda (r @exp s)
             (ast-node return-statement
                       (:line-col (token-line-col r))
                       (:exp @exp))))
    (:java-rule ((returnToken Token)
                 (value Expression)
                 (semicolon Token))
                Statement
                "new ReturnValueStatement(value)")

    (@statement return s-colon)
    (:rule (lambda (r s)
             (ast-node return-statement
                       (:line-col (token-line-col r)))))
    (:java-rule ((returnToken Token)
                 (semicolon Token))
                Statement
                "new EmptyReturnStatement()")

    ;; @exp

    (@exp @exp-logical)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    ;; @exp-logical

    (@exp-logical @exp-comparison)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    (@exp-logical @exp-logical && @exp-comparison)
    (:rule (lambda (@exp-logical foo @exp-comparison)
             (ast-node and-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-logical)
                       (:right @exp-comparison))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new LogicalAndExpression(lhs, rhs)")

    ;; @exp-comparison

    (@exp-comparison @exp-term)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    (@exp-comparison @exp-term < @exp-term)
    (:rule (lambda (@exp-term-left foo @exp-term-right)
             (ast-node less-than-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term-left)
                       (:right @exp-term-right))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new LessThanExpression(lhs, rhs)")

    (@exp-comparison @exp-term == @exp-term)
    (:rule (lambda (@exp-term-left foo @exp-term-right)
             (ast-node equal-to-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term-left)
                       (:right @exp-term-right))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new EqualityExpression(lhs, rhs)")

    ;; @exp-term

    (@exp-term @exp-factor)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    (@exp-term @exp-term + @exp-factor)
    (:rule (lambda (@exp-term foo @exp-factor)
             (ast-node add-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term)
                       (:right @exp-factor))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new AdditionExpression(lhs, rhs)")

    (@exp-term @exp-term - @exp-factor)
    (:rule (lambda (@exp-term foo @exp-factor)
             (ast-node subtract-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-term)
                       (:right @exp-factor))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new SubtractionExpression(lhs, rhs)")

    (@exp-factor @exp-factor pipe @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-or)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new BinaryOrExpression(lhs, rhs)")

    (@exp-factor @exp-factor & @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-and)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new BinaryAndExpression(lhs, rhs)")

    (@exp-factor @exp-factor ^ @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-xor)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new BinaryExclusiveOrExpression(lhs, rhs)")

    ;; @exp-factor

    (@exp-factor @exp-elem)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    (@exp-factor @exp-factor * @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node multiply-expression
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new MultiplicationExpression(lhs, rhs)")

    (@exp-factor @exp-factor >> @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-arith-rshift)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new ShiftRightExpression(lhs, rhs)")

    (@exp-factor @exp-factor << @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-lshift)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new ShiftLeftExpression(lhs, rhs)")

    (@exp-factor @exp-factor >>> @exp-elem)
    (:rule (lambda (@exp-factor foo @exp-elem)
             (ast-node int-binop-expression
                       (:op 'bit-rshift)
                       (:line-col (token-line-col foo))
                       (:left @exp-factor)
                       (:right @exp-elem))))
    (:java-rule ((lhs Expression)
                 (operator Token)
                 (rhs Expression))
                Expression
                "new ArithmeticShiftRightExpression(lhs, rhs)")

    ;; @exp-elem

    (@exp-elem @exp-array)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    (@exp-elem @exp-ref)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    (@exp-elem bang @exp-elem)
    (:rule (lambda (foo @exp-elem)
             (ast-node not-expression
                       (:line-col (token-line-col foo))
                       (:exp @exp-elem))))
    (:java-rule ((notOperator Token)
                 (expression Expression))
                Expression
                "new LogicalNotExpression(expression)")

    (@exp-elem ~ @exp-elem)
    (:rule (lambda (foo @exp-elem)
             (ast-node int-unop-expression
                       (:op 'bit-not)
                       (:line-col (token-line-col foo))
                       (:exp @exp-elem))))
    (:java-rule ((notOperator Token)
                 (expression Expression))
                Expression
                "new BinaryNotExpression(expression)")

    ;; @exp-array

    (@exp-array @exp-ref l-brak @exp r-brak)
    (:rule (lambda (@exp-ref foo-1 @exp foo-2)
             (ast-node array-ref-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:array-exp @exp-ref)
                       (:index-exp @exp))))
    (:java-rule ((arrayExpression Expression)
                 (leftBracket Token)
                 (indexExpression Expression)
                 (rightBracket Token))
                Expression
                "new ArrayReferenceExpression(arrayExpression, indexExpression)")

    ;; @exp-ref

    (@exp-ref @exp-ref dot length)
    (:rule (lambda (@exp-ref foo-1 foo-2)
             (ast-node length-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:exp @exp-ref))))
    (:java-rule ((arrayExpression Expression)
                 (dot Token)
                 (length Token))
                Expression
                "new ArrayLengthExpression(arrayExpression)")

    (@exp-ref @exp-ref dot @method-ident l-paren r-paren)
    (:rule (lambda (@exp-ref foo-1 ident foo-2 foo-3)
             (ast-node method-call-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:object @exp-ref)
                       (:method-name (token-value ident))
                       (:arg-list '()))))
    (:java-rule ((objectExpression Expression)
                 (dot Token)
                 (methodName Token)
                 (leftParen Token)
                 (rightParen Token))
                Expression
                "new MethodCallExpression(objectExpression, methodName, new ExpressionList())")

    (@exp-ref @exp-ref dot @method-ident l-paren @exp-list r-paren)
    (:rule (lambda (@exp-ref foo-1 ident foo-2 @exp-list foo-3)
             (ast-node method-call-expression
                       (:line-col (@ast @exp-ref :line-col))
                       (:object @exp-ref)
                       (:method-name (token-value ident))
                       (:arg-list @exp-list))))
    (:java-rule ((objectExpression Expression)
                 (dot Token)
                 (methodName Token)
                 (leftParen Token)
                 (arguments ExpressionList)
                 (rightParen Token))
                Expression
                "new MethodCallExpression(objectExpression, methodName, arguments)")

    (@exp-ref @exp-trivial)
    (:rule (lambda (x) x))
    (:java-rule ((expression Expression))
                Expression
                "expression")

    ;; @exp-trivial

    (@exp-trivial number)
    (:rule (lambda (x)
             (ast-node number-constant-expression
                       (:line-col (token-line-col x))
                       (:value (token-value x)))))
    (:java-rule ((number Token))
                Expression
                "new NumericLiteralExpression(number)")

    (@exp-trivial true)
    (:rule (lambda (x)
             (ast-node boolean-constant-expression
                       (:line-col (token-line-col x))
                       (:value "true")
                       (:scheme-value #t))))
    (:java-rule ((trueToken Token))
                Expression
                "new BooleanLiteralExpression(true)")

    (@exp-trivial false)
    (:rule (lambda (y)
             (ast-node boolean-constant-expression
                       (:line-col (token-line-col y))
                       (:value "false")
                       (:scheme-value #f))))
    (:java-rule ((falseToken Token))
                Expression
                "new BooleanLiteralExpression(false)")

    (@exp-trivial char-literal)
    (:rule (lambda (c)
             (ast-node char-constant-expression
                       (:line-col (token-line-col c))
                       (:value (token-value c)))))
    (:java-rule ((character Token))
                Expression
                "new CharacterLiteralExpresion(character)")

    (@exp-trivial string-literal)
    (:rule (lambda (s)
             (ast-node new-object-expression
                       (:line-col (token-line-col s))
                       (:class-name "String")
                       (:arg-list (list 
                                   (ast-node string-constant-expression
                                             (:line-col (token-line-col s))
                                             (:value (token-value s))))))))
    (:java-rule ((string Token))
                Expression
                "new StringLiteralExpresion(string)")

    (@exp-trivial ident)
    (:rule (lambda (y)
             (ast-node variable-expression
                       (:line-col (token-line-col y))
                       (:ident (token-value y)))))
    (:java-rule ((identifier Token))
                Expression
                "new VariableReferenceExpresion(ident)")

    (@exp-trivial this)
    (:rule (lambda (y)
             (ast-node this-expression
                       (:line-col (token-line-col y)))))
    (:java-rule ((thisToken Token))
                Expression
                "new ThisReferenceExpresion()")

    (@exp-trivial new int l-brak @exp r-brak)
    (:rule (lambda (foo-1 foo-2 foo-3 @exp foo-4)
             (ast-node new-array-expression
                       (:line-col (token-line-col foo-1))
                       (:element-type (ast-node simple-type
                                                (:name "int")))
                       (:length @exp))))
    (:java-rule ((newToken Token)
                 (type Token)
                 (leftBracket Token)
                 (sizeExpression Expression)
                 (rightBracket Token))
                Expression
                "new NewArrayExpression(type, sizeExpression)")

    (@exp-trivial new char l-brak @exp r-brak)
    (:rule (lambda (foo-1 foo-2 foo-3 @exp foo-4)
             (ast-node new-array-expression
                       (:line-col (token-line-col foo-1))
                       (:element-type (ast-node simple-type
                                                (:name "char")))
                       (:length @exp))))
    (:java-rule ((newToken Token)
                 (type Token)
                 (leftBracket Token)
                 (sizeExpression Expression)
                 (rightBracket Token))
                Expression
                "new NewArrayExpression(type, sizeExpression)")

    (@exp-trivial new ident l-brak @exp r-brak)
    (:rule (lambda (foo-1 @ident foo-3 @exp foo-4)
             (ast-node new-array-expression
                       (:line-col (token-line-col foo-1))
                       (:element-type (ast-node user-type
                                                (:name (token-value @ident))))
                       (:length @exp))))
    (:java-rule ((newToken Token)
                 (typeName Token)
                 (leftBracket Token)
                 (sizeExpression Expression)
                 (rightBracket Token))
                Expression
                "new NewArrayExpression(typeName, sizeExpression)")

    (@exp-trivial new ident l-paren r-paren)
    (:rule (lambda (foo-1 ident foo-2 foo-3)
             (ast-node new-object-expression
                       (:line-col (token-line-col foo-1))
                       (:class-name (token-value ident))
                       (:arg-list '()))))
    (:java-rule ((newToken Token)
                 (typeName Token)
                 (leftParen Token)
                 (rightParen Token))
                Expression
                "new NewObjectExpression(typeName, new ExpressionList())")

    (@exp-trivial new ident l-paren @exp-list r-paren)
    (:rule (lambda (foo:new ident foo:l-paren @exp-list foo:r-paren)
             (ast-node new-object-expression
                       (:line-col (token-line-col foo:new))
                       (:class-name (token-value ident))
                       (:arg-list @exp-list))))
    (:java-rule ((newToken Token)
                 (typeName Token)
                 (leftParen Token)
                 (arguments ExpressionList)
                 (rightParen Token))
                Expression
                "new NewObjectExpression(typeName, arguments)")

    (@exp-trivial l-paren @exp r-paren)
    (:rule (lambda (foo-1 @exp foo-2)
             @exp))
    (:java-rule ((leftParen Token)
                 (expression Expression)
                 (rightParen Token))
                Expression
                "expression")

    ;; @exp-list

    (@exp-list @exp)
    (:rule (lambda (x) (list x)))
    (:java-rule ((expression Expression))
                ExpressionList
                "new ExpressionList(expression)")
                

    (@exp-list @exp-list comma @exp)
    (:rule (lambda (x y z) (append x (list z))))
    (:java-rule ((expressionList ExpressionList)
                 (comma Token)
                 (expression Expression))
                ExpressionList
                "ExpressionList.append(expressionList, expression)")

    ))

(define minijava-grammar 
  (grammar-rules-filter-productions minijava-grammar-rules))

(define minijava-graph (build-lr0-state-transition-graph minijava-grammar))
(define minijava-sr-table (generate-lr0-table minijava-grammar minijava-graph minijava-grammar-rules))
(define minijava-goto-table (generate-lr0-goto-table minijava-grammar minijava-graph))
(define minijava-terminals (grammar-terminals minijava-grammar))
(define minijava-nonterminals (grammar-nonterminals minijava-grammar))
(define minijava-action-table (grammar-rules-build-action-table minijava-grammar-rules))

(if (file-exists? "minijava-parser.scm")
    (delete-file "minijava-parser.scm"))

(call-with-output-file "minijava-parser.scm"
  (lambda (port)
    (for-each
     (lambda (code) (write code port) (newline port))
     `((need parse/parse)
       (need ast)
       (define action-table ,minijava-action-table)
       (define (minijava-parse-input input)
         (run-parse (quote ,minijava-grammar)
                    (quote ,(append minijava-terminals (list '$)))
                    (quote ,minijava-nonterminals)
                    (quote ,minijava-sr-table)
                    (quote ,minijava-goto-table)
                    action-table
                    input))
       (define (minijava-parse-port input-port)
         (minijava-parse-input (read input-port)))
       (define (minijava-parse-file filename)
         (call-with-input-file filename
           minijava-parse-port))))))
