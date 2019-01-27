(need sasm/sasm-regalloc)
(need sasm/sasm-tx)
(need sasm/sasm-mips)
(need sasm/sasm-opt)

(set! *sasm-enable-regalloc* #t)
(sasm-set-target-mips!)
(sasm-set-mips-hw-params!)

(sasm-invoke-optimizer *argv*)
