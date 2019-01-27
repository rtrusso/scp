(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-register-operand expression)
  (sasm-parse-by-case
   expression

   (sasm-syntax-case
    :pattern (reg (,symbol? register-name))
    :rewrite (register-name)
    (sasm-ast-node <register-reference>
                   (:register-name register-name)))

   (sasm-syntax-case
    :pattern (sys frame-pointer)
    :rewrite ()
    (sasm-ast-node <system-register-reference>
                   (:register-name 'frame-pointer)))

   (sasm-syntax-case
    :pattern (sys stack-pointer)
    :rewrite ()
    (sasm-ast-node <system-register-reference>
                   (:register-name 'stack-pointer)))

   (sasm-syntax-case
    :pattern (sys (,symbol? register-name))
    :rewrite (register-name)
    (sasm-ast-node <system-register-reference>
                   (:register-name register-name)))

   ))

