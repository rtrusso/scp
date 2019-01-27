;; sasm-ast.scm
;;
;; AST-building routines, syntax, etc. for the SASM language.

(need scheme/tag)
(need util/list)

(define (sasm-ast-node-type-check table node)
  (let ((entry (assoc (get-tag node) table)))
    (if (not entry)
        (error "SASM AST unknown node type "
               (get-tag node))
        (let ((known-attributes (cdr entry))
              (actual-attributes (map car (contents node))))
          (or (and (list-subset? eqv? known-attributes actual-attributes)
                   (list-subset? eqv? actual-attributes known-attributes)
                   (= (length actual-attributes)
                      (length (remove-duplicates eqv? known-attributes))
                      (length (remove-duplicates eqv? actual-attributes))))
              (error "SASM AST schema violation "
                     (get-tag node)
                     known-attributes
                     actual-attributes))))))

;; (define-syntax define-sasm-ast-schema
;;   (syntax-rules (node-definition)
;;     ((_ <schema-checker>
;;         (node-definition <type> <attribute-1> <attribute-2> ...)
;;         ...)
;;      (lambda (node)
;; ;       (let ((table '((<type>) ;<attribute-1> <attribute-2> ...)
;; ;                      ...)))
;;        (sasm-ast-node-type-check '() node)))))

(define-syntax define-sasm-ast-schema
  (syntax-rules (node-definition)
    ((_ <schema-checker>
        (node-definition <type> <attribute-1> <attribute-2> ...) ...)
     (define <schema-checker>
       (lambda (node)
         (let ((table '((<type> <attribute-1> <attribute-2> ...) ...)))
           (sasm-ast-node-type-check table node)))))))

;; (define-syntax define-sasm-ast-schema
;;   (syntax-rules (node-definition)
;;     ((_ <schema-checker>
;;         (node-definition <type> <attribute-1> <attribute-2> ...) ...)
;;      (define <schema-checker>
;;        (let* ((known-types '(<type> ...))
;;               (no-dupes (remove-duplicates eqv? known-types)))
;;          (if (not (equal? known-types no-dupes))
;;              (error "Duplicate SASM AST node types defined!"))
;;          (lambda (node)
;;            (case (get-tag node)
;;              ((<type>)
;;               (let ((known-attributes '(<attribute-1> <attribute-2> ...))
;;                     (actual-attributes (map car (contents node))))
;;                 (or (and (list-subset? eqv? known-attributes actual-attributes)
;;                          (list-subset? eqv? actual-attributes known-attributes)
;;                          (= (length actual-attributes)
;;                             (length (remove-duplicates eqv? known-attributes))
;;                             (length (remove-duplicates eqv? actual-attributes))))
;;                     (error "SASM AST schema violation "
;;                            (get-tag node)
;;                            known-attributes
;;                            actual-attributes))))
;;              ...
;;              (else
;;               (error "Unknown AST node type" (get-tag node))))))))))

(define-sasm-ast-schema sasm-ast-schema
  (node-definition <sasm-program>
                   :statements)

  (node-definition <sasm-member-function>
                   :referenced-symbol
                   :resolved-symbol)

  (node-definition <sasm-global-data-symbol>
                   :referenced-symbol
                   :resolved-symbol)

  (node-definition <sasm-global-data-integer>
                   :integer-value)

  (node-definition <sasm-global-data-string>
                   :string-value
                   :asciiz-tag)

  (node-definition <sasm-integer-symconst>
                   :defined-symconst-symbol
                   :symconst-symbol
                   :symconst-value)

  (node-definition <register-reference>
                   :register-name)

  (node-definition <system-register-reference>
                   :register-name)

  (node-definition <integer-constant-operand>
                   :integer-value)

  (node-definition <label-constant-operand>
                   :label-value
                   :referenced-symbol
                   :resolved-symbol)

  (node-definition <string-constant-operand>
                   :string-value)

  (node-definition <numbered-temporary-reference>
                   :temporary-number)

  (node-definition <named-temporary-reference>
                   :temporary-name)

  (node-definition <argument-reference>
                   :argument-number)

  (node-definition <numbered-local-reference>
                   :local-number)

  (node-definition <nested-operation-operand>
                   :operation)

  (node-definition <symbolic-constant-operand>
                   :symconst-name
                   :referenced-symconst-symbol
                   :resolved-symconst)

  (node-definition <sasm-operation>
                   :operation-name
                   :operands)

  (node-definition <sasm-perform-operation-instruction>
                   :operation)

  (node-definition <sasm-assignment-instruction>
                   :destination
                   :operand)

  (node-definition <sasm-operation-assignment-instruction>
                   :destination
                   :operation)

  (node-definition <sasm-test-instruction>
                   :operation)

  (node-definition <sasm-test-branch-instruction>
                   :label-operand
                   :condition)

  (node-definition <sasm-operational-branch-instruction>
                   :label-operand
                   :condition
                   :operand)

  (node-definition <sasm-direct-goto-instruction>
                   :label-operand)

  (node-definition <sasm-register-goto-instruction>
                   :register-operand)

  (node-definition <sasm-save-register-instruction>
                   :register-operand)

  (node-definition <sasm-restore-register-instruction>
                   :register-operand)

  (node-definition <sasm-push-instruction>
                   :operand)

  (node-definition <sasm-clear-stack-instruction>
                   :element-count)

  (node-definition <sasm-pop-instruction>
                   :destination)

  (node-definition <sasm-return-instruction>
                   :return-instruction)

  (node-definition <sasm-return-and-clear-stack-instruction>
                   :element-count)

  (node-definition <sasm-label-definition-directive>
                   :defined-symbol
                   :qualified-symbol
                   :label-name
                   :resolved-references)

  (node-definition <sasm-class>
                   :defined-symbol
                   :qualified-symbol
                   :resolved-references
                   :size
                   :member-functions)

  (node-definition <sasm-global-data>
                   :defined-symbol
                   :qualified-symbol
                   :resolved-references
                   :global-data-symbol
                   :global-data)

  (node-definition <sasm-entry-point> 
                   :referenced-symbol
                   :resolved-symbol
                   :entry-point-symbol)

  (node-definition <sasm-symconst-table>
                   :symconst-entries)

  (node-definition <sasm-class-info>
                   :class-info-body)

  (node-definition <sasm-extern>
                   :defined-symbol
                   :qualified-symbol
                   :resolved-references
                   :extern-symbol)

  (node-definition <sasm-export>
                   :referenced-symbol
                   :resolved-symbol)

  (node-definition <sasm-include>
                   :include-path)

  (node-definition <sasm-function>
                   :defined-symbol
                   :qualified-symbol
                   :resolved-references
                   :function-symbol
                   :local-count
                   :instructions))

(define-syntax sasm-ast-node
  (syntax-rules ()
    ((_ <type> (:name value) ...)
     (let ((node (tag '<type> (list (cons ':name value) ...))))
       (sasm-ast-schema node)
       node))))

(define-syntax sasm-ast-node-append-attribute!
  (syntax-rules ()
    ((_ <node> <attribute> <value>)
     (let* ((node <node>)
            (current-value (sasm-ast-node-attribute node <attribute>)))
       (sasm-ast-node-attribute! node <attribute>
                                 (append current-value
                                         (list <value>)))))))

(define (sasm-ast-node-get-attribute node attribute)
  (let ((entry (assoc attribute (contents node))))
    (if entry
        (cdr entry)
        (error "sasm-ast-node-get-attribute attribute not found" attribute))))

(define-syntax sasm-ast-node-attribute
  (syntax-rules ()
    ((_ node :attribute)
     (let ((entry (assoc ':attribute (contents node))))
       (if entry
           (cdr entry)
           (error "sasm-ast-node-attribute -- attribute not found" ':attribute))))
    ((_ node <type> :attribute)
     (let ((entry (assoc ':attribute (safe-contents node '<type>))))
       (if entry
           (cdr entry)
           (error "sasm-ast-node-attribute -- attribute not found" ':attribute))))))

(define-syntax sasm-ast-getter
  (syntax-rules ()
    ((_ <attribute>)
     (lambda (node)
       (sasm-ast-node-attribute node <attribute>)))))

(define-syntax sasm-ast-node-attribute!
  (syntax-rules ()
    ((_ node :attribute value)
     (let ((entry (assoc ':attribute (contents node))))
       (if entry
           (begin (set-cdr! entry value)
                  value)
           (error "sasm-ast-node-attribute! -- attribute not found" ':attribute))))
    ((_ node <type> :attribute value)
     (let ((entry (assoc ':attribute (safe-contents node '<type>))))
       (if entry
           (begin (set-cdr! entry value)
                  value)
           (error "sasm-ast-node-attribute! -- attribute not found" ':attribute))))))
