;; sasm-nasmx64.scm
;; SASM Machine Description - x64_64 on the NASM assembler

(need sasm/sasm-tx)
(need sasm/nasmx64/util)
(need sasm/nasmx64/binop)
(need sasm/nasmx64/interp)
(need sasm/nasmx64/labels)
(need sasm/nasmx64/control)
(need sasm/nasmx64/compare)
(need sasm/nasmx64/stack)
(need sasm/nasmx64/store-array)
(need sasm/nasmx64/load-array)
(need sasm/nasmx64/call)
(need sasm/nasmx64/return)
(need sasm/nasmx64/arithmetic)
(need sasm/nasmx64/mul)
(need sasm/nasmx64/bitwise-const)
(need sasm/nasmx64/shift)
(need sasm/nasmx64/data)
(need sasm/nasmx64/preamble)
(need sasm/nasmx64/debug)
(need sasm/nasmx64/base)
(need sasm/nasmx64/machine)

(define (sasm-set-target-x64!)
  (sasm-set-target! 'nasm-x64
                    nasm-x64-registers
                    nasm-x64-sys-registers
                    (nasm-x64-machine)))

(define (sasm-set-x64-hw-params!)
  (sasm-symconst-alist-append '((cells-per-word 8) (shift-cells-per-word 3) (mask-cells-per-word 7)
                                (bits-per-word 64) (shift-bits-per-word 6) (mask-bits-per-word 63))))



