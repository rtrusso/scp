(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-operand expression)
  (sasm-parse-by-case
   expression

   (sasm-syntax-case
    :pattern (?? ,sasm-parse-lvalue-operand operand)
    :rewrite (operand)
    operand)

   (sasm-syntax-case
    :pattern (?? ,sasm-parse-constant-operand constant)
    :rewrite (constant)
    constant)

   (sasm-syntax-case
    :pattern (label (,symbol? symbol-name))
    :rewrite (symbol-name)
    (sasm-ast-node <label-constant-operand>
                   (:label-value symbol-name)
                   (:referenced-symbol symbol-name)
                   (:resolved-symbol #f)))

   (sasm-syntax-case
    :pattern (result . (?? ,sasm-parse-operation operation))
    :rewrite (operation)
    (sasm-ast-node <nested-operation-operand>
                   (:operation operation)))

   (sasm-syntax-case
    :pattern (symconst (,symbol? symconst-name))
    :rewrite (symconst-name)
    (sasm-ast-node <symbolic-constant-operand>
                   (:symconst-name symconst-name)
                   (:referenced-symconst-symbol symconst-name)
                   (:resolved-symconst #f)))

   ))

