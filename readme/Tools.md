# Tools

## schemec.exe

This is the compiler frontend for a useful subset of R5RS Scheme. It supports a dialect of Scheme named "conspiracy" which includes (need x/y/z) syntax for separate compilation and dependency tracking.

The output of schemec is a SASM program (<input>.scm to <input>.sasm) and a separate syntax file (<input>.sasm-syntax) which is used in separate compilation of `define-syntax` and `syntax-rules`.

## sasm-opt.exe

This is the SASM "optimizer" tool which can perform dataflow analysis, instruction elimination, register allocation, and so on. It takes a <input>.sasm file and translates it into a <input>.sasm-opt file.

The sasm-opt.exe tool is responsible for code improvements and also specializing the SASM input into a less abstract form, referencing actual registers instead of abstract register names for the target, and rewriting the original SASM input into a form that is compatible with the target.

It can run in a `--cheap` mode where it performs minimal transformation on the input to save time if the input meets some requirements. The `--cheap` mode is compatible with the output from the Scheme frontend to make bootstrap faster.

The sasm-opt.exe tool is specialized for the x86 backend. A separate tool, sasm-opt-mips, is generated for targeting MIPS.

## sasm.exe

The sasm.exe tool translates the output of sasm-opt.exe into a x86 .asm file for NASM. Similar to the case with sasm-opt.exe, there is a separate tool for the MIPS backend.

# Example

    REM
    set RTL_OBJS=out/bootstrap/c-rtlheap.obj out/bootstrap/gc.obj out/bootstrap/gc-wrapper.obj out/bootstrap/gc-invoke.obj out/bootstrap/gc-stack.obj out/bootstrap/gc-mark.obj out/bootstrap/gc-mark-range.obj out/bootstrap/gc-mark-array.obj out/bootstrap/gc-mark-class.obj out/bootstrap/gc-sweep.obj out/bootstrap/heap.obj out/bootstrap/heapfixed.obj out/bootstrap/heapvar.obj out/bootstrap/mjrtl.obj out/bootstrap/rtlheap.obj out/bootstrap/debug.obj out/bootstrap/r5rs-library.obj out/bootstrap/r5rs-native.obj out/bootstrap/r5rs-wrap.obj out/bootstrap/rtlscheme.obj out/bootstrap/scheme-main.obj
    REM
    schemec.exe test.scm --output test.sasm
    sasm-opt.exe test.sasm --out=test.sasm-opt
    sasm.exe test.sasm-opt --out=test.asm
    nasm -fwin32 test.asm -o test.obj
    gcc -DSCHEME_RTL=1 -Irtl rtl/mjrtl.c rtl/rtlscheme.c %RTL_OBJS% test.obj -o test.exe
    .\test.exe

Two future improvements are planned to make this more convenient:

  - A command-line tool to run the phases of the compiler automatically
  - RTL lib files instead of separate obj files

