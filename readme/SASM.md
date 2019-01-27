# Symbolic Assembly Language

SASM is an abstract assembly language. The operations performed and
the registers and other storage manipulated by those operations are
all represented symbolically.

# Example

Consider the following example instruction:

    (assign (reg operand) (op load-array) (label $scmglobal-char-Eq?) (const 0))

This is an assignment instruction. The target of the assignment
(left-hand side) is an abstract register, named `operand`. The source
of the assignment (right-hand side) is an operation, named
`load-array` which references memory. A label is used for the base
pointer of the load-array operation, and a 0-word offset is specified.

This is an exact instruction emitted by the Scheme frontend for
referencing a global variable. When we run this instruction through
the SASM "optimizer" tool, sasm-opt.exe, it is translated into a more
specific form of SASM instruction:

    (assign (reg ebx) (op load-array) (label $scmglobal-char-Eq?) (const 0))

In this form of the instruction, the symbolic `operand` register is
replaced with a real, physical x86 register `ebx`. The remainder of
the instruction is still exactly the same. In this case, there is a
static mapping of the operand register to ebx, however the
sasm-opt.exe tool is capable of performing register allocation and
transforming other storage such as `(temp my-variable)` and `(local
1)` into actual register references.

Lastly, we run this instruction through the sasm.exe tool, and it is
translated into an actual x86 instruction that is recognized by NASM.

    mov ebx, [___Doscmglobal_char_Eq__Qu]


# Notes

The base syntax for SASM is a Scheme or LISP style s-exp, although
supporting other syntaxes is planned work.

The SASM language was heavily influenced by Wizard Book's [5th chapter](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-30.html#%_chap_5),
Computing With Register Machines.
