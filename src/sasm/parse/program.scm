(need sasm/parse/syntax)
(need sasm/sasm-ast)

(define (sasm-parse-program program)
  (sasm-parse-by-case
   program

   (sasm-syntax-case
    :pattern (@@ ,sasm-parse-statement statements)
    :rewrite (statements)
    (sasm-ast-node <sasm-program>
                   (:statements statements)))

   ))

