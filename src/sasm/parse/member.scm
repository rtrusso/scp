(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-class-member-function expression)
  (sasm-parse-by-case
   expression

   (sasm-syntax-case
    :pattern (label (,symbol? symbol))
    :rewrite (symbol)
    (sasm-ast-node <sasm-member-function>
                   (:referenced-symbol symbol)
                   (:resolved-symbol #f)))

   (sasm-parse-error
    :error-message "Unrecognized class member function declaration:")

   ))

