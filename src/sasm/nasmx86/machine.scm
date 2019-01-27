(define (nasm-x86-machine)
  (append (nasm-x86-machine-base)
          (nasm-x86-standard-twoarg-binop 'add "add")
          (nasm-x86-standard-twoarg-binop 'sub "sub")
          (nasm-x86-standard-twoarg-binop 'bit-xor "xor")
          (nasm-x86-standard-twoarg-binop 'bit-or "or")
          (nasm-x86-standard-twoarg-binop 'bit-and "and")))

