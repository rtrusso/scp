(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-global-data-entry expression)
  (sasm-parse-by-case
   expression

   (sasm-syntax-case
    :pattern (label (,symbol? symbol))
    :rewrite (symbol)
    (sasm-ast-node <sasm-global-data-symbol>
                   (:referenced-symbol symbol)
                   (:resolved-symbol #f)))

   (sasm-syntax-case
    :pattern (const (,integer? integer-value))
    :rewrite (integer-value)
    (sasm-ast-node <sasm-global-data-integer>
                   (:integer-value integer-value)))

   (sasm-syntax-case
    :pattern (const (,string? string-value))
    :rewrite (string-value)
    (sasm-ast-node <sasm-global-data-string>
                   (:string-value string-value)
                   (:asciiz-tag #f)))

   (sasm-syntax-case
    :pattern (const :asciiz (,string? constant-string))
    :rewrite (constant-string)
    (sasm-ast-node <sasm-global-data-string>
                   (:string-value constant-string)
                   (:asciiz-tag #t)))


   (sasm-parse-error
    :error-message "Unrecognized global data entry:")

   ))

