;; sasm-visitor.scm
;;
;; AST visitor infrastructure for SASM.

(need scheme/tag)

(define-syntax sasm-node-visitor
  (syntax-rules (:preorder :postorder)

    ;;
    ;; preorder, postorder both specified
    ;;
    ((_ ((<node-variable> <node-type-1> <node-type-2> ...)
         (<property-variable> <property-name>) ...)
        (:preorder <preorder-body-1> <preorder-body> ...)
        (:postorder <postorder-body-1> <postorder-body> ...))
     (lambda (<node-variable> traversal-pass)
       (if (or (tagged? '<node-type-1> <node-variable>)
               (tagged? '<node-type-2> <node-variable>) ...)
           (apply (lambda (<property-variable> ...)
                    (case traversal-pass
                      ((:preorder) <preorder-body-1> <preorder-body> ...)
                      ((:postorder) <postorder-body-1> <postorder-body> ...)
                      (else (error "Unrecognized traversal pass -- sasm-node-visitor" traversal-pass))))
                  (map (lambda (property-name)
                         (sasm-ast-node-get-attribute <node-variable>
                                                      property-name))
                       '(<property-name> ...)))
           #f)))

    ;;
    ;; only preorder specified
    ;;
    ((_ ((<node-variable> <node-type-1> <node-type-2> ...)
         (<property-variable> <property-name>) ...)
        (:preorder <preorder-body-1> <preorder-body> ...))
     (sasm-node-visitor ((<node-variable> <node-type-1> <node-type-2> ...)
                         (<property-variable> <property-name>) ...)
                        (:preorder <preorder-body-1> <preorder-body> ...)
                        (:postorder #f)))

    ;;
    ;; only postorder specified
    ;;
    ((_ ((<node-variable> <node-type-1> <node-type-2> ...)
         (<property-variable> <property-name>) ...)
        (:postorder <postorder-body-1> <postorder-body> ...))
     (sasm-node-visitor ((<node-variable> <node-type-1> <node-type-2> ...)
                         (<property-variable> <property-name>) ...)
                        (:preorder #f)
                        (:postorder <postorder-body-1> <postorder-body> ...)))

    ;;
    ;; no traversal ordering specified => postorder
    ;;
    ((_ ((<node-variable> <node-type-1> <node-type-2> ...)
         (<property-variable> <property-name>) ...)
        <body-1> <body> ...)
     (begin
       (sasm-node-visitor ((<node-variable> <node-type-1> <node-type-2> ...)
                           (<property-variable> <property-name>) ...)
                          (:preorder #f)
                          (:postorder <body-1> <body> ...))))

    ))


(define-syntax sasm-ast-visitor
  (syntax-rules (:covering)
    ((_ (((<node-variable> <node-type-1> <node-type-2> ...) <args> ...)
         <body> ...)
        ...)
     (let ((<node-type-1> (sasm-node-visitor ((<node-variable> <node-type-1> <node-type-2> ...) <args> ...)
                                             <body> ...))
           ...)
       (lambda (node traversal-pass)
         (case (get-tag node)
           ((<node-type-1> <node-type-2> ...)
            (apply <node-type-1> node traversal-pass '()))
           ...
           (else #f)))))

    ((_ :covering (((<node-variable> <node-type-1> <node-type-2> ...) <args> ...)
         <body> ...)
        ...)
     (let ((<node-type-1> (sasm-node-visitor ((<node-variable> <node-type-1> <node-type-2> ...) <args> ...)
                                            <body> ...))
           ...)
       (lambda (node traversal-pass)
         (case (get-tag node)
           ((<node-type-1> <node-type-2> ...)
            (apply <node-type-1> node traversal-pass '()))
           ...
           (else (error "Unrecognized node type" (get-tag node) node))))))

    ))

(define-syntax define-sasm-ast-visitor
  (syntax-rules (define-case :covering)
    ((_ <visitor-name> 
        (define-case ((<node-variable> <node-type-1> <node-type-2> ...) (<arg-name-1> <arg-type-1>) ...)
          <body> ...)
        ...)
     (define <visitor-name>
       (sasm-ast-visitor (((<node-variable> <node-type-1> <node-type-2> ...) (<arg-name-1> <arg-type-1>) ...)
                          <body> ...)
                         ...)))

    ((_ <visitor-name>
        :covering
        (define-case ((<node-variable> <node-type-1> <node-type-2> ...) (<arg-name-1> <arg-type-1>) ...)
          <body> ...)
        ...)
     (define <visitor-name>
       (sasm-ast-visitor :covering
                         (((<node-variable> <node-type-1> <node-type-2> ...) (<arg-name-1> <arg-type-1>) ...)
                          <body> ...)
                         ...)))

    ))

(define (sasm-visit-ast node visitor)
  (define (recurse current next)
    (meta-visitor next ':postorder))

  (define-sasm-ast-visitor meta-visitor
    :covering

    (define-case ((program <sasm-program>)
                  (statements :statements))
      (visitor program ':preorder)
      (for-each (lambda (statement)
                  (recurse program statement))
                statements)
      (visitor program ':postorder))

    (define-case ((member-function <sasm-member-function>))
      (visitor member-function ':preorder)
      (visitor member-function ':postorder))

    (define-case ((global-data <sasm-global-data-symbol>))
      (visitor global-data ':preorder)
      (visitor global-data ':postorder))

    (define-case ((global-data <sasm-global-data-integer>))
      (visitor global-data ':preorder)
      (visitor global-data ':postorder))

    (define-case ((global-data <sasm-global-data-string>))
      (visitor global-data ':preorder)
      (visitor global-data ':postorder))

    (define-case ((symconst <sasm-integer-symconst>))
      (visitor symconst ':preorder)
      (visitor symconst ':postorder))

    (define-case ((operand <register-reference>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <system-register-reference>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <integer-constant-operand>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <label-constant-operand>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <string-constant-operand>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <numbered-temporary-reference>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <named-temporary-reference>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <argument-reference>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <numbered-local-reference>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <symbolic-constant-operand>))
      (visitor operand ':preorder)
      (visitor operand ':postorder))

    (define-case ((operand <nested-operation-operand>)
                  (nested-operation :operation))
      (visitor operand ':preorder)
      (recurse operand nested-operation)
      (visitor operand ':postorder))

    (define-case ((operation <sasm-operation>)
                  (operands :operands))
      (visitor operation ':preorder)
      (for-each (lambda (operand)
                  (recurse operation operand))
                operands)
      (visitor operation ':postorder))

    (define-case ((instruction <sasm-perform-operation-instruction>)
                  (operation :operation))
      (visitor instruction ':preorder)
      (recurse instruction operation)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-assignment-instruction>)
                  (destination :destination)
                  (operand :operand))
      (visitor instruction ':preorder)
      (recurse instruction destination)
      (recurse instruction operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-operation-assignment-instruction>)
                  (destination :destination)
                  (operation :operation))
      (visitor instruction ':preorder)
      (recurse instruction destination)
      (recurse instruction operation)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-test-instruction>)
                  (operation :operation))
      (visitor instruction ':preorder)
      (recurse instruction operation)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-test-branch-instruction>)
                  (label-operand :label-operand))
      (visitor instruction ':preorder)
      (recurse instruction label-operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-operational-branch-instruction>)
                  (operand :operand)
                  (label-operand :label-operand))
      (visitor instruction ':preorder)
      (recurse instruction label-operand)
      (recurse instruction operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-direct-goto-instruction>)
                  (label-operand :label-operand))
      (visitor instruction ':preorder)
      (recurse instruction label-operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-register-goto-instruction>)
                  (register-operand :register-operand))
      (visitor instruction ':preorder)
      (recurse instruction register-operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-save-register-instruction>)
                  (register-operand :register-operand))
      (visitor instruction ':preorder)
      (recurse instruction register-operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-restore-register-instruction>)
                  (register-operand :register-operand))
      (visitor instruction ':preorder)
      (recurse instruction register-operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-push-instruction>)
                  (operand :operand))
      (visitor instruction ':preorder)
      (recurse instruction operand)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-pop-instruction>)
                  (destination :destination))
      (visitor instruction ':preorder)
      (recurse instruction destination)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-clear-stack-instruction>))
      (visitor instruction ':preorder)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-return-instruction>))
      (visitor instruction ':preorder)
      (visitor instruction ':postorder))

    (define-case ((instruction <sasm-return-and-clear-stack-instruction>))
      (visitor instruction ':preorder)
      (visitor instruction ':postorder))

    (define-case ((directive <sasm-label-definition-directive>))
      (visitor directive ':preorder)
      (visitor directive ':postorder))

    (define-case ((statement <sasm-class>)
                  (member-functions :member-functions))
      (visitor statement ':preorder)
      (for-each (lambda (member-function)
                  (recurse statement member-function))
                member-functions)
      (visitor statement ':postorder))

    (define-case ((statement <sasm-global-data>)
                  (data :global-data))
      (visitor statement ':preorder)
      (for-each (lambda (datum)
                  (recurse statement datum))
                data)
      (visitor statement ':postorder))

    (define-case ((statement <sasm-entry-point>))
      (visitor statement ':preorder)
      (visitor statement ':postorder))

    (define-case ((statement <sasm-symconst-table>)
                  (entries :symconst-entries))
      (visitor statement ':preorder)
      (for-each (lambda (entry)
                  (recurse statement entry))
                entries)
      (visitor statement ':postorder))

    (define-case ((statement <sasm-class-info>))
      (visitor statement ':preorder)
      (visitor statement ':postorder))

    (define-case ((statement <sasm-export>))
      (visitor statement ':preorder)
      (visitor statement ':postorder))

    (define-case ((statement <sasm-extern>))
      (visitor statement ':preorder)
      (visitor statement ':postorder))

    (define-case ((statement <sasm-include>))
      (visitor statement ':preorder)
      (visitor statement ':postorder))

    (define-case ((function <sasm-function>)
                  (instructions :instructions))
      (visitor function ':preorder)
      (for-each (lambda (instruction)
                  (recurse function instruction))
                instructions)
      (visitor function ':postorder))
    )

  (recurse '() node))

;; Syntax which defines a visitor and then runs a visit operation on
;; the specified tree where the body expressions may refer to a
;; specified "accumulate keyword" to accumulate results in a list.
;;
(define-syntax sasm-visit-ast-with-accumulator
  (syntax-rules (define-case)
    ((_ <accumulate-keyword>
        <ast>
        (define-case <args> <body-1> <body-2> ...) ...)
     (let ((accumulate-result '()))
       (define (<accumulate-keyword> x)
         (set! accumulate-result (cons x accumulate-result)))
       (define-sasm-ast-visitor visitor
         (define-case <args> <body-1> <body-2> ...) ...)
       (sasm-visit-ast <ast> visitor)
       (reverse accumulate-result)))))

(define-syntax sasm-filter-ast
  (syntax-rules ()
    ((_ <ast> <type-1> <type-2> ...)
     (sasm-visit-ast-with-accumulator
      accumulate!
      <ast>
      (define-case ((node-1 <type-1>))
        (accumulate! node-1))
      (define-case ((node-2 <type-2>))
        (accumulate! node-2)) ...))))
