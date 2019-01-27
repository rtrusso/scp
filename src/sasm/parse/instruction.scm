(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-instruction instruction)
  (sasm-parse-by-case
   instruction

   (sasm-syntax-case
    :pattern (perform . (?? ,sasm-parse-operation operation))
    :rewrite (operation)
    (sasm-ast-node <sasm-perform-operation-instruction>
                   (:operation operation)))

   (sasm-syntax-case
    :pattern (assign (?? ,sasm-parse-lvalue-operand destination)
                     (?? ,sasm-parse-operand operand))
    :rewrite (destination operand)
    (sasm-ast-node <sasm-assignment-instruction>
                   (:destination destination)
                   (:operand operand)))

   (sasm-syntax-case
    :pattern (assign (?? ,sasm-parse-lvalue-operand destination)
                     .
                     (?? ,sasm-parse-operation operation))
    :rewrite (destination operation)
    (sasm-ast-node <sasm-operation-assignment-instruction>
                   (:destination destination)
                   (:operation operation)))

   (sasm-syntax-case
    :pattern (test . (?? ,sasm-parse-operation operation))
    :rewrite (operation)
    (sasm-ast-node <sasm-test-instruction>
                   (:operation operation)))

   (sasm-syntax-case
    :pattern (branch (label (,symbol? label-name)))
    :rewrite (label-name)
    (sasm-ast-node <sasm-test-branch-instruction>
                   (:label-operand (sasm-ast-node <label-constant-operand>
                                                  (:label-value label-name)
                                                  (:referenced-symbol label-name)
                                                  (:resolved-symbol #f)))
                   (:condition #t)))

   (sasm-syntax-case
    :pattern (branch-true (label (,symbol? label-name)))
    :rewrite (label-name)
    (sasm-ast-node <sasm-test-branch-instruction>
                   (:label-operand (sasm-ast-node <label-constant-operand>
                                                  (:label-value label-name)
                                                  (:referenced-symbol label-name)
                                                  (:resolved-symbol #f)))
                   (:condition #t)))

   (sasm-syntax-case
    :pattern (branch-false (label (,symbol? label-name)))
    :rewrite (label-name)
    (sasm-ast-node <sasm-test-branch-instruction>
                   (:label-operand (sasm-ast-node <label-constant-operand>
                                                  (:label-value label-name)
                                                  (:referenced-symbol label-name)
                                                  (:resolved-symbol #f)))
                   (:condition #f)))

   (sasm-syntax-case
    :pattern (branch-zero (label (,symbol? label-name))
                          (?? ,sasm-parse-operand operand))
    :rewrite (label-name operand)
    (sasm-ast-node <sasm-operational-branch-instruction>
                   (:label-operand (sasm-ast-node <label-constant-operand>
                                                  (:label-value label-name)
                                                  (:referenced-symbol label-name)
                                                  (:resolved-symbol #f)))
                   (:condition #f)
                   (:operand operand)))

   (sasm-syntax-case
    :pattern (branch-nonzero (label (,symbol? label-name))
                             (?? ,sasm-parse-operand operand))
    :rewrite (label-name operand)
    (sasm-ast-node <sasm-operational-branch-instruction>
                   (:label-operand (sasm-ast-node <label-constant-operand>
                                                  (:label-value label-name)
                                                  (:referenced-symbol label-name)
                                                  (:resolved-symbol #f)))
                   (:condition #t)
                   (:operand operand)))

   (sasm-syntax-case
    :pattern (goto (label (,symbol? label-name)))
    :rewrite (label-name)
    (sasm-ast-node <sasm-direct-goto-instruction>
                   (:label-operand (sasm-ast-node <label-constant-operand>
                                                  (:label-value label-name)
                                                  (:referenced-symbol label-name)
                                                  (:resolved-symbol #f)))
                   ))

   (sasm-syntax-case
    :pattern (goto (reg (,symbol? register-name)))
    :rewrite (register-name)
    (sasm-ast-node <sasm-register-goto-instruction>
                   (:register-operand (sasm-ast-node <register-reference>
                                                     (:register-name register-name)))))

   (sasm-syntax-case
    :pattern (save (?? ,sasm-parse-register-operand register))
    :rewrite (register)
    (sasm-ast-node <sasm-save-register-instruction>
                   (:register-operand register)))

   (sasm-syntax-case
    :pattern (restore (?? ,sasm-parse-register-operand register))
    :rewrite (register)
    (sasm-ast-node <sasm-restore-register-instruction>
                   (:register-operand register)))

   (sasm-syntax-case
    :pattern (push (?? ,sasm-parse-operand operand))
    :rewrite (operand)
    (sasm-ast-node <sasm-push-instruction>
                   (:operand operand)))

   (sasm-syntax-case
    :pattern (pop (const (,integer? count)))
    :rewrite (count)
    (sasm-ast-node <sasm-clear-stack-instruction>
                   (:element-count count)))

   (sasm-syntax-case
    :pattern (pop (?? ,sasm-parse-lvalue-operand destination))
    :rewrite (destination)
    (sasm-ast-node <sasm-pop-instruction>
                   (:destination destination)))

   (sasm-syntax-case
    :pattern (return)
    :rewrite ()
    (sasm-ast-node <sasm-return-instruction>
                   (:return-instruction #t)))

   (sasm-syntax-case
    :pattern (return (const (,integer? count)))
    :rewrite (count)
    (sasm-ast-node <sasm-return-and-clear-stack-instruction>
                   (:element-count count)))

   (sasm-syntax-case
    :pattern (label (,symbol? label-name))
    :rewrite (label-name)
    (sasm-ast-node <sasm-label-definition-directive>
                   (:defined-symbol label-name)
                   (:qualified-symbol #f)
                   (:label-name label-name)
                   (:resolved-references '())))

   (sasm-parse-error
    :error-message "Invalid instruction")

   ))

