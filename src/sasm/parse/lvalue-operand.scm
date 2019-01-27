(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-lvalue-operand expression)
  (sasm-parse-by-case
   expression

   (sasm-syntax-case
    :pattern (?? ,sasm-parse-register-operand register)
    :rewrite (register)
    register)

   (sasm-syntax-case
    :pattern (temp (,integer? temp-number))
    :rewrite (temp-number)
    (sasm-ast-node <numbered-temporary-reference>
                   (:temporary-number temp-number)))

   (sasm-syntax-case
    :pattern (temp (,symbol? temp-name))
    :rewrite (temp-name)
    (sasm-ast-node <named-temporary-reference>
                   (:temporary-name temp-name)))

   (sasm-syntax-case
    :pattern (arg (,integer? argument-number))
    :rewrite (argument-number)
    (sasm-ast-node <argument-reference>
                   (:argument-number argument-number)))

   (sasm-syntax-case
    :pattern (local (,integer? local-number))
    :rewrite (local-number)
    (sasm-ast-node <numbered-local-reference>
                   (:local-number local-number)))

   ))


