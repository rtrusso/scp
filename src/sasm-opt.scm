(need sasm/sasm-regalloc)
(need sasm/sasm-tx)
(need sasm/sasm-nasmx86)
(need sasm/sasm-opt)

;; Set optimizer parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable the register allocator.
(set! *sasm-enable-regalloc* #t)
;; Disable dataflow analysis and related optimizations (currently expensive).
(set! *sasm-enable-only-regalloc* #t)
;; Do not attempt to allocate temporaries in registers; always use locals.
(set! *sasm-all-temps-are-locals* #t)
;; Do not fold linear instructions into trees to improve instruction
;; selection.
(set! *sasm-fold-instruction-tree* #f)

;; Set target parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sasm-set-target-x86!)
(sasm-set-x86-hw-params!)

(sasm-invoke-optimizer *argv*)
