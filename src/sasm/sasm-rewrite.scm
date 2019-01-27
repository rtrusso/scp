;; sasm/sasm-rewrite.scm
;;
;; Rewrite a SASM AST into a SASM source program

(define (sasm-rewrite-ast ast)
  (define *current-list-head* '())
  (define *current-list-tail* '())

  (define *stack* '())

  (define (qualified-symbol node)
    (sasm-ast-node-attribute node :qualified-symbol))

  (define (push!)
    (set! *stack* (cons (cons *current-list-head* *current-list-tail*)
                        *stack*))
    (set! *current-list-head* '())
    (set! *current-list-tail* '()))

  (define (pop!)
    (let ((current-list *current-list-head*)
          (current (car *stack*)))
      (set! *current-list-head* (car current))
      (set! *current-list-tail* (cdr current))
      (set! *stack* (cdr *stack*))
      (emit! current-list)))

  (define (emit! obj)
    (if (pair? *current-list-tail*)
        (begin (set-cdr! *current-list-tail*
                         (cons obj '()))
               (set! *current-list-tail* (cdr *current-list-tail*)))
        (begin (set! *current-list-head* (cons obj '()))
               (set! *current-list-tail* *current-list-head*))))

  (define-sasm-ast-visitor visitor :covering
    (define-case ((program <sasm-program>))
      (:preorder #f)
      (:postorder #f))

    (define-case ((reference-object <sasm-member-function>
                                    <sasm-global-data-symbol>)
                  (symbol :resolved-symbol))
      (:preorder (emit! `(label ,(qualified-symbol symbol)))))

    (define-case ((integer-object <sasm-global-data-integer>)
                  (integer :integer-value))
      (:preorder (emit! `(const ,integer))))

    (define-case ((string-object <sasm-global-data-string>)
                  (asciiz? :asciiz-tag)
                  (value :string-value))
      (:preorder (emit! `(const ,@(if asciiz? (list ':asciiz) '())
                                ,value))))

    (define-case ((symconst <sasm-integer-symconst>)
                  (symbol :symconst-symbol)
                  (value :symconst-value))
      (:preorder (emit! `(,symbol ,value))))

    (define-case ((ref <register-reference>)
                  (name :register-name))
      (:preorder (emit! `(reg ,name))))

    (define-case ((ref <system-register-reference>)
                  (name :register-name))
      (:preorder (emit! `(sys ,name))))

    (define-case ((op <integer-constant-operand>)
                  (value :integer-value))
      (:preorder (emit! `(const ,value))))

    (define-case ((op <label-constant-operand>)
                  (value :label-value)
                  (resolved :resolved-symbol))
      (:preorder (emit! `(label ,(qualified-symbol resolved)))))

    (define-case ((op <string-constant-operand>)
                  (value :string-value))
      (:preorder (emit! `(const ,value))))

    (define-case ((ref <numbered-temporary-reference>)
                  (number :temporary-number))
      (:preorder (emit! `(temp ,number))))

    (define-case ((ref <named-temporary-reference>)
                  (name :temporary-name))
      (:preorder (emit! `(temp ,name))))

    (define-case ((ref <argument-reference>)
                  (number :argument-number))
      (:preorder (emit! `(arg ,number))))

    (define-case ((ref <numbered-local-reference>)
                  (number :local-number))
      (:preorder (emit! `(local ,number))))

    (define-case ((op <nested-operation-operand>)
                  (nested :operation))
      (:preorder (push!)
                 (emit! 'result))
      (:postorder (pop!)))

    (define-case ((op <symbolic-constant-operand>)
                  (name :symconst-name))
      (:preorder (emit! `(symconst ,name))))

    (define-case ((op <sasm-operation>)
                  (name :operation-name))
      (:preorder (emit! `(op ,name))))

    (define-case ((insn <sasm-perform-operation-instruction>))
      (:preorder (push!)
                 (emit! 'perform))
      (:postorder (pop!)))

    (define-case ((insn <sasm-assignment-instruction>
                        <sasm-operation-assignment-instruction>))
      (:preorder (push!)
                 (emit! 'assign))
      (:postorder (pop!)))

    (define-case ((insn <sasm-test-instruction>))
      (:preorder (push!)
                 (emit! 'test))
      (:postorder (pop!)))

    (define-case ((insn <sasm-test-branch-instruction>)
                  (condition :condition))
      (:preorder (push!)
                 (emit! (if condition
                            'branch
                            'branch-false)))
      (:postorder (pop!)))

    (define-case ((insn <sasm-operational-branch-instruction>)
                  (condition :condition))
      (:preorder (push!)
                 (emit! (if condition
                            'branch-nonzero
                            'branch-zero)))
      (:postorder (pop!)))

    (define-case ((insn <sasm-direct-goto-instruction>
                        <sasm-register-goto-instruction>))
      (:preorder (push!)
                 (emit! 'goto))
      (:postorder (pop!)))

    (define-case ((insn <sasm-save-register-instruction>))
      (:preorder (push!)
                 (emit! 'save))
      (:postorder (pop!)))

    (define-case ((insn <sasm-restore-register-instruction>))
      (:preorder (push!)
                 (emit! 'restore))
      (:postorder (pop!)))

    (define-case ((insn <sasm-push-instruction>))
      (:preorder (push!)
                 (emit! 'push))
      (:postorder (pop!)))

    (define-case ((insn <sasm-clear-stack-instruction>)
                  (count :element-count))
      (:preorder (emit! `(pop (const ,count)))))

    (define-case ((insn <sasm-pop-instruction>))
      (:preorder (push!)
                 (emit! 'pop))
      (:postorder (pop!)))

    (define-case ((insn <sasm-return-instruction>))
      (:preorder (emit! `(return))))

    (define-case ((insn <sasm-return-and-clear-stack-instruction>)
                  (count :element-count))
      (:preorder (emit! `(return (const ,count)))))

    (define-case ((def <sasm-label-definition-directive>)
                  (sym :qualified-symbol))
      (:preorder (emit! `(label ,sym))))

    (define-case ((c <sasm-class>)
                  (sym :qualified-symbol)
                  (size :size))
      (:preorder (push!)
                 (emit! 'class)
                 (emit! sym)
                 (emit! `(const ,size)))
      (:postorder (pop!)))

    (define-case ((g <sasm-global-data>)
                  (sym :qualified-symbol))
      (:preorder (push!)
                 (emit! 'global)
                 (emit! sym))
      (:postorder (pop!)))

    (define-case ((e <sasm-entry-point>)
                  (sym :resolved-symbol))
      (:preorder (emit! `(entry ,(qualified-symbol sym)))))

    (define-case ((t <sasm-symconst-table>))
      (:preorder (push!)
                 (emit! `define-symconst)
                 )
      (:postorder (pop!)))

    (define-case ((c <sasm-class-info>)
                  (body :class-info-body))
      (:preorder (emit! (quasiquote (class-info (unquote-splicing body))))))

    (define-case ((e <sasm-extern>)
                  (sym :qualified-symbol))
      (:preorder (emit! `(extern ,sym))))

    (define-case ((e <sasm-export>)
                  (sym :resolved-symbol))
      (:preorder (emit! `(export ,(qualified-symbol sym)))))

    (define-case ((i <sasm-include>)
                  (path :include-path))
      (:preorder (emit! `(include ,path))))

    (define-case ((f <sasm-function>)
                  (sym :qualified-symbol)
                  (locals :local-count))
      (:preorder (push!)
                 (emit! 'function)
                 (emit! `(name ,sym))
                 (emit! `(locals ,locals))
                 (push!)
                 (emit! 'body))
      (:postorder (pop!)
                  (pop!)))

    )

  (sasm-visit-ast ast visitor)
  (if (not (null? *stack*))
      (error "Emit stack is not empty" *stack*))
  *current-list-head*)
