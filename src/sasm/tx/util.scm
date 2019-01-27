(define (sasm-tx-insn-trivial? stmt)
  (or (pattern-match '(assign (? reg) (op side-effect)) stmt)
      (pattern-match '(perform (op use-garbage) (? reg)) stmt)
      (pattern-match '(assign (? reg) (? reg)) stmt)
      (pattern-match '(perform (op reserve-locals) (const 0)) stmt)))

