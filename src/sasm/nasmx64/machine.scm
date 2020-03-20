(define (nasm-x64-machine)
  (append (nasm-x64-machine-base)
          (nasm-x64-standard-twoarg-binop 'add "add")
          (nasm-x64-standard-twoarg-binop 'sub "sub")
          (nasm-x64-standard-twoarg-binop 'bit-xor "xor")
          (nasm-x64-standard-twoarg-binop 'bit-or "or")
          (nasm-x64-standard-twoarg-binop 'bit-and "and")))

