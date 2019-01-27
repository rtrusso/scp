(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-symconst-entry expression)
  (sasm-parse-by-case
   expression

   (sasm-syntax-case
    :pattern ((,symbol? symconst-symbol) (,integer? symconst-value))
    :rewrite (symconst-symbol symconst-value)
    (sasm-ast-node <sasm-integer-symconst>
                   (:defined-symconst-symbol symconst-symbol)
                   (:symconst-symbol symconst-symbol)
                   (:symconst-value symconst-value)))

   (sasm-parse-error
    :error-message "Unrecognized symconst entry:")

   ))

