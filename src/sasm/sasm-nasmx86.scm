;; sasm-nasmx86.scm
;; SASM Machine Description - x86 on the NASM assembler

(need sasm/sasm-tx)
(need sasm/nasmx86/util)
(need sasm/nasmx86/binop)
(need sasm/nasmx86/interp)
(need sasm/nasmx86/labels)
(need sasm/nasmx86/control)
(need sasm/nasmx86/compare)
(need sasm/nasmx86/stack)
(need sasm/nasmx86/store-array)
(need sasm/nasmx86/load-array)
(need sasm/nasmx86/call)
(need sasm/nasmx86/return)
(need sasm/nasmx86/arithmetic)
(need sasm/nasmx86/mul)
(need sasm/nasmx86/bitwise-const)
(need sasm/nasmx86/shift)
(need sasm/nasmx86/data)
(need sasm/nasmx86/preamble)
(need sasm/nasmx86/debug)
(need sasm/nasmx86/base)
(need sasm/nasmx86/machine)

(define (sasm-set-target-x86!)
  (sasm-set-target! 'nasm-x86
                    nasm-x86-registers
                    nasm-x86-sys-registers
                    (nasm-x86-machine)))

(define (sasm-set-x86-hw-params!)
  (sasm-symconst-alist-append '((cells-per-word 4) (shift-cells-per-word 2) (mask-cells-per-word 3)
                                (bits-per-word 32) (shift-bits-per-word 5) (mask-bits-per-word 31))))



