(define (sasm-interp-symconst-machine-params)
  (sasm-symconst-alist-append '((cells-per-word 1)
                                (shift-cells-per-word 0)
                                (mask-cells-per-word 0)
                                (bits-per-word 32)
                                (shift-bits-per-word 5)
                                (mask-bits-per-word 31))))

