(need sasm/sasm-regalloc)
(need sasm/sasm-tx)
(need sasm/sasm-nasmx64)
(need sasm/sasm-opt)

(set! *sasm-enable-regalloc* #t)
(sasm-set-target-x64!)
(sasm-set-x64-hw-params!)

(sasm-invoke-optimizer *argv*)
