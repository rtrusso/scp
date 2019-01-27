(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-operation operation)
  (sasm-parse-by-case
   operation

   (sasm-syntax-case
    :pattern ((op (,symbol? operation-symbol))
              .
              (@@ ,sasm-parse-operand operands))
    :rewrite (operation-symbol operands)
    (sasm-ast-node <sasm-operation>
                   (:operation-name operation-symbol)
                   (:operands operands)))

   ))

