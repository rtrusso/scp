(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-constant-operand expression)
  (sasm-parse-by-case
   expression

   (sasm-syntax-case
    :pattern (const (,integer? constant-integer))
    :rewrite (constant-integer)
    (sasm-ast-node <integer-constant-operand>
                   (:integer-value constant-integer)))

   (sasm-syntax-case
    :pattern (const (,symbol? label))
    :rewrite (label)
    (sasm-ast-node <label-constant-operand>
                   (:label-value label)
                   (:referenced-symbol label)
                   (:resolved-symbol #f)))

   (sasm-syntax-case
    :pattern (const (,string? constant-string))
    :rewrite (constant-string)
    (sasm-ast-node <string-constant-operand>
                   (:string-value constant-string)))

   ))

