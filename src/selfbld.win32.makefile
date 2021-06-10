DEPEND_ALL=\
  out/selfbld/test/bootstrap-tests.done \
  \
  out/selfbld/sasm.exe \
  out/selfbld/sasm-opt.exe \
  out/selfbld/schemec.exe \
  out/selfbld/test/selfbld-tests.done

SELFBLD_DIR=out/selfbld/.exists
SELFBLD_TEST_DIR=out/selfbld/test/.exists

BOOTSTRAP_SASM_OPT=out\bootstrap\sasm-opt.exe
BOOTSTRAP_SASMC=out\bootstrap\sasm.exe
BOOTSTRAP_SCHEMEC=out\bootstrap\schemec.exe

selfbld_all: $(DEPEND_ALL)

.SECONDARY:

# selfbld
$(SELFBLD_DIR): out/.exists
	cmd.exe /c "if not exist out\selfbld mkdir out\selfbld"
	cmd.exe /c "echo exists>out\selfbld\.exists"

$(SELFBLD_TEST_DIR): $(SELFBLD_DIR)
	cmd.exe /c "if not exist out\selfbld\test mkdir out\selfbld\test"
	cmd.exe /c "echo exists>out\selfbld\test\.exists"

DEPEND_SELFBLD_TEST_DONE=\
  out/selfbld/test/util-string.sasm-opt \
  out/selfbld/test/util-string.asm \
  out/selfbld/test/util-string.obj \
  out/selfbld/test/gc-invoke.sasm-opt \
  out/selfbld/test/gc-invoke.asm \
  out/selfbld/test/gc-invoke.obj \
  out/selfbld/test/sasmtest-gc-invoke.asm \
  out/selfbld/test/sasmtest-gc-invoke.asmdiff \
  out/selfbld/test/sasmtest-gc-invoke.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.obj \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asm \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.obj \
  out/selfbld/test/sasmtest-algo-graph.asm \
  out/selfbld/test/sasmtest-algo-graph.asmdiff \
  out/selfbld/test/sasmtest-algo-graph.obj \
  out/selfbld/test/sasmtest-build-rules.asm \
  out/selfbld/test/sasmtest-build-rules.asmdiff \
  out/selfbld/test/sasmtest-build-rules.obj \
  out/selfbld/test/sasmtest-c-rtlheap.asm \
  out/selfbld/test/sasmtest-c-rtlheap.asmdiff \
  out/selfbld/test/sasmtest-c-rtlheap.obj \
  out/selfbld/test/sasmtest-gc-invoke.asm \
  out/selfbld/test/sasmtest-gc-invoke.asmdiff \
  out/selfbld/test/sasmtest-gc-invoke.obj \
  out/selfbld/test/sasmtest-gc-mark-array.asm \
  out/selfbld/test/sasmtest-gc-mark-array.asmdiff \
  out/selfbld/test/sasmtest-gc-mark-array.obj \
  out/selfbld/test/sasmtest-gc-mark-class.asm \
  out/selfbld/test/sasmtest-gc-mark-class.asmdiff \
  out/selfbld/test/sasmtest-gc-mark-class.obj \
  out/selfbld/test/sasmtest-gc-mark-range.asm \
  out/selfbld/test/sasmtest-gc-mark-range.asmdiff \
  out/selfbld/test/sasmtest-gc-mark-range.obj \
  out/selfbld/test/sasmtest-gc-mark.asm \
  out/selfbld/test/sasmtest-gc-mark.asmdiff \
  out/selfbld/test/sasmtest-gc-mark.obj \
  out/selfbld/test/sasmtest-gc-stack.asm \
  out/selfbld/test/sasmtest-gc-stack.asmdiff \
  out/selfbld/test/sasmtest-gc-stack.obj \
  out/selfbld/test/sasmtest-gc-sweep.asm \
  out/selfbld/test/sasmtest-gc-sweep.asmdiff \
  out/selfbld/test/sasmtest-gc-sweep.obj \
  out/selfbld/test/sasmtest-gc-wrapper.asm \
  out/selfbld/test/sasmtest-gc-wrapper.asmdiff \
  out/selfbld/test/sasmtest-gc-wrapper.obj \
  out/selfbld/test/sasmtest-gc.asm \
  out/selfbld/test/sasmtest-gc.asmdiff \
  out/selfbld/test/sasmtest-gc.obj \
  out/selfbld/test/sasmtest-heap.asm \
  out/selfbld/test/sasmtest-heap.asmdiff \
  out/selfbld/test/sasmtest-heap.obj \
  out/selfbld/test/sasmtest-heapfixed.asm \
  out/selfbld/test/sasmtest-heapfixed.asmdiff \
  out/selfbld/test/sasmtest-heapfixed.obj \
  out/selfbld/test/sasmtest-heapvar.asm \
  out/selfbld/test/sasmtest-heapvar.asmdiff \
  out/selfbld/test/sasmtest-heapvar.obj \
  out/selfbld/test/sasmtest-mjrtl.asm \
  out/selfbld/test/sasmtest-mjrtl.asmdiff \
  out/selfbld/test/sasmtest-mjrtl.obj \
  out/selfbld/test/sasmtest-pat-pat.asm \
  out/selfbld/test/sasmtest-pat-pat.asmdiff \
  out/selfbld/test/sasmtest-pat-pat.obj \
  out/selfbld/test/sasmtest-r5rs-library.asm \
  out/selfbld/test/sasmtest-r5rs-library.asmdiff \
  out/selfbld/test/sasmtest-r5rs-library.obj \
  out/selfbld/test/sasmtest-r5rs-native.asm \
  out/selfbld/test/sasmtest-r5rs-native.asmdiff \
  out/selfbld/test/sasmtest-r5rs-native.obj \
  out/selfbld/test/sasmtest-r5rs-wrap.asm \
  out/selfbld/test/sasmtest-r5rs-wrap.asmdiff \
  out/selfbld/test/sasmtest-r5rs-wrap.obj \
  out/selfbld/test/sasmtest-rtlheap.asm \
  out/selfbld/test/sasmtest-rtlheap.asmdiff \
  out/selfbld/test/sasmtest-rtlheap.obj \
  out/selfbld/test/sasmtest-rtlscheme.asm \
  out/selfbld/test/sasmtest-rtlscheme.asmdiff \
  out/selfbld/test/sasmtest-rtlscheme.obj \
  out/selfbld/test/sasmtest-sasm-fastgraph.asm \
  out/selfbld/test/sasmtest-sasm-fastgraph.asmdiff \
  out/selfbld/test/sasmtest-sasm-fastgraph.obj \
  out/selfbld/test/sasmtest-sasm-fastset.asm \
  out/selfbld/test/sasmtest-sasm-fastset.asmdiff \
  out/selfbld/test/sasmtest-sasm-fastset.obj \
  out/selfbld/test/sasmtest-sasm-machdesc.asm \
  out/selfbld/test/sasmtest-sasm-machdesc.asmdiff \
  out/selfbld/test/sasmtest-sasm-machdesc.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-arithmetic.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-arithmetic.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-arithmetic.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-base.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-base.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-base.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-binop.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-binop.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-binop.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-call.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-call.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-call.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-compare.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-compare.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-compare.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-control.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-control.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-control.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-data.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-data.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-data.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-debug.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-debug.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-debug.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-interp.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-interp.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-interp.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-labels.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-labels.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-labels.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-load-array.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-load-array.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-load-array.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-machine.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-machine.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-machine.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-mul.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-mul.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-mul.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-preamble.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-preamble.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-preamble.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-return.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-return.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-return.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-shift.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-shift.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-shift.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-stack.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-stack.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-stack.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-store-array.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-store-array.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-store-array.obj \
  out/selfbld/test/sasmtest-sasm-nasmx86-util.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-util.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-util.obj \
  out/selfbld/test/sasmtest-sasm-opt.asm \
  out/selfbld/test/sasmtest-sasm-opt.asmdiff \
  out/selfbld/test/sasmtest-sasm-opt.obj \
  out/selfbld/test/sasmtest-sasm-parse-constant-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-constant-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-constant-operand.obj \
  out/selfbld/test/sasmtest-sasm-parse-data-entry.asm \
  out/selfbld/test/sasmtest-sasm-parse-data-entry.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-data-entry.obj \
  out/selfbld/test/sasmtest-sasm-parse-extern.asm \
  out/selfbld/test/sasmtest-sasm-parse-extern.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-extern.obj \
  out/selfbld/test/sasmtest-sasm-parse-instruction.asm \
  out/selfbld/test/sasmtest-sasm-parse-instruction.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-instruction.obj \
  out/selfbld/test/sasmtest-sasm-parse-lvalue-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-lvalue-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-lvalue-operand.obj \
  out/selfbld/test/sasmtest-sasm-parse-member.asm \
  out/selfbld/test/sasmtest-sasm-parse-member.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-member.obj \
  out/selfbld/test/sasmtest-sasm-parse-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-operand.obj \
  out/selfbld/test/sasmtest-sasm-parse-operation.asm \
  out/selfbld/test/sasmtest-sasm-parse-operation.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-operation.obj \
  out/selfbld/test/sasmtest-sasm-parse-program.asm \
  out/selfbld/test/sasmtest-sasm-parse-program.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-program.obj \
  out/selfbld/test/sasmtest-sasm-parse-register-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-register-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-register-operand.obj \
  out/selfbld/test/sasmtest-sasm-parse-statement.asm \
  out/selfbld/test/sasmtest-sasm-parse-statement.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-statement.obj \
  out/selfbld/test/sasmtest-sasm-parse-symconst-entry.asm \
  out/selfbld/test/sasmtest-sasm-parse-symconst-entry.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-symconst-entry.obj \
  out/selfbld/test/sasmtest-sasm-parse-syntax.asm \
  out/selfbld/test/sasmtest-sasm-parse-syntax.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-syntax.obj \
  out/selfbld/test/sasmtest-sasm-parse-util.asm \
  out/selfbld/test/sasmtest-sasm-parse-util.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-util.obj \
  out/selfbld/test/sasmtest-sasm-sasm-analyze.asm \
  out/selfbld/test/sasmtest-sasm-sasm-analyze.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-analyze.obj \
  out/selfbld/test/sasmtest-sasm-sasm-ast.asm \
  out/selfbld/test/sasmtest-sasm-sasm-ast.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-ast.obj \
  out/selfbld/test/sasmtest-sasm-sasm-codegen.asm \
  out/selfbld/test/sasmtest-sasm-sasm-codegen.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-codegen.obj \
  out/selfbld/test/sasmtest-sasm-sasm-core.asm \
  out/selfbld/test/sasmtest-sasm-sasm-core.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-core.obj \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asm \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.obj \
  out/selfbld/test/sasmtest-sasm-sasm-insel.asm \
  out/selfbld/test/sasmtest-sasm-sasm-insel.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-insel.obj \
  out/selfbld/test/sasmtest-sasm-sasm-insn.asm \
  out/selfbld/test/sasmtest-sasm-sasm-insn.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-insn.obj \
  out/selfbld/test/sasmtest-sasm-sasm-nasmx86.asm \
  out/selfbld/test/sasmtest-sasm-sasm-nasmx86.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-nasmx86.obj \
  out/selfbld/test/sasmtest-sasm-sasm-opt.asm \
  out/selfbld/test/sasmtest-sasm-sasm-opt.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-opt.obj \
  out/selfbld/test/sasmtest-sasm-sasm-parse.asm \
  out/selfbld/test/sasmtest-sasm-sasm-parse.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-parse.obj \
  out/selfbld/test/sasmtest-sasm-sasm-regalloc.asm \
  out/selfbld/test/sasmtest-sasm-sasm-regalloc.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-regalloc.obj \
  out/selfbld/test/sasmtest-sasm-sasm-tracing.asm \
  out/selfbld/test/sasmtest-sasm-sasm-tracing.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-tracing.obj \
  out/selfbld/test/sasmtest-sasm-sasm-tx.asm \
  out/selfbld/test/sasmtest-sasm-sasm-tx.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-tx.obj \
  out/selfbld/test/sasmtest-sasm-sasm-visitor.asm \
  out/selfbld/test/sasmtest-sasm-sasm-visitor.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-visitor.obj \
  out/selfbld/test/sasmtest-sasm-sasm.asm \
  out/selfbld/test/sasmtest-sasm-sasm.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm.obj \
  out/selfbld/test/sasmtest-sasm-tx-assemble.asm \
  out/selfbld/test/sasmtest-sasm-tx-assemble.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-assemble.obj \
  out/selfbld/test/sasmtest-sasm-tx-config.asm \
  out/selfbld/test/sasmtest-sasm-tx-config.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-config.obj \
  out/selfbld/test/sasmtest-sasm-tx-context.asm \
  out/selfbld/test/sasmtest-sasm-tx-context.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-context.obj \
  out/selfbld/test/sasmtest-sasm-tx-emit.asm \
  out/selfbld/test/sasmtest-sasm-tx-emit.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-emit.obj \
  out/selfbld/test/sasmtest-sasm-tx-globals.asm \
  out/selfbld/test/sasmtest-sasm-tx-globals.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-globals.obj \
  out/selfbld/test/sasmtest-sasm-tx-labels.asm \
  out/selfbld/test/sasmtest-sasm-tx-labels.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-labels.obj \
  out/selfbld/test/sasmtest-sasm-tx-main.asm \
  out/selfbld/test/sasmtest-sasm-tx-main.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-main.obj \
  out/selfbld/test/sasmtest-sasm-tx-read.asm \
  out/selfbld/test/sasmtest-sasm-tx-read.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-read.obj \
  out/selfbld/test/sasmtest-sasm-tx-registers.asm \
  out/selfbld/test/sasmtest-sasm-tx-registers.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-registers.obj \
  out/selfbld/test/sasmtest-sasm-tx-regrule.asm \
  out/selfbld/test/sasmtest-sasm-tx-regrule.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-regrule.obj \
  out/selfbld/test/sasmtest-sasm-tx-rewrite.asm \
  out/selfbld/test/sasmtest-sasm-tx-rewrite.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-rewrite.obj \
  out/selfbld/test/sasmtest-sasm-tx-stmt.asm \
  out/selfbld/test/sasmtest-sasm-tx-stmt.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-stmt.obj \
  out/selfbld/test/sasmtest-sasm-tx-symconst.asm \
  out/selfbld/test/sasmtest-sasm-tx-symconst.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-symconst.obj \
  out/selfbld/test/sasmtest-sasm-tx-util.asm \
  out/selfbld/test/sasmtest-sasm-tx-util.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-util.obj \
  out/selfbld/test/sasmtest-sasm.asm \
  out/selfbld/test/sasmtest-sasm.asmdiff \
  out/selfbld/test/sasmtest-sasm.obj \
  out/selfbld/test/sasmtest-scheme-base-syntax.asm \
  out/selfbld/test/sasmtest-scheme-base-syntax.asmdiff \
  out/selfbld/test/sasmtest-scheme-base-syntax.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-application.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-application.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-application.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-emit.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-emit.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-emit.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-epilogue.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-epilogue.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-epilogue.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-immediate.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-immediate.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-immediate.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-lambda.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-lambda.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-lambda.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-letrec.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-letrec.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-letrec.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-pretty-print.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-pretty-print.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-pretty-print.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-reference.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-reference.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-reference.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-set.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-set.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-set.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-tailcall.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-tailcall.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-tailcall.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-targets.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-targets.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-targets.obj \
  out/selfbld/test/sasmtest-scheme-compiler-codegen.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen.obj \
  out/selfbld/test/sasmtest-scheme-compiler-context.asm \
  out/selfbld/test/sasmtest-scheme-compiler-context.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-context.obj \
  out/selfbld/test/sasmtest-scheme-compiler-cps.asm \
  out/selfbld/test/sasmtest-scheme-compiler-cps.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-cps.obj \
  out/selfbld/test/sasmtest-scheme-compiler-env.asm \
  out/selfbld/test/sasmtest-scheme-compiler-env.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-env.obj \
  out/selfbld/test/sasmtest-scheme-compiler-gc.asm \
  out/selfbld/test/sasmtest-scheme-compiler-gc.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-gc.obj \
  out/selfbld/test/sasmtest-scheme-compiler-labels.asm \
  out/selfbld/test/sasmtest-scheme-compiler-labels.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-labels.obj \
  out/selfbld/test/sasmtest-scheme-compiler-linkage.asm \
  out/selfbld/test/sasmtest-scheme-compiler-linkage.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-linkage.obj \
  out/selfbld/test/sasmtest-scheme-compiler-settings.asm \
  out/selfbld/test/sasmtest-scheme-compiler-settings.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-settings.obj \
  out/selfbld/test/sasmtest-scheme-compiler.asm \
  out/selfbld/test/sasmtest-scheme-compiler.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler.obj \
  out/selfbld/test/sasmtest-scheme-main.asm \
  out/selfbld/test/sasmtest-scheme-main.asmdiff \
  out/selfbld/test/sasmtest-scheme-main.obj \
  out/selfbld/test/sasmtest-scheme-syntax-core-context.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-context.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-context.obj \
  out/selfbld/test/sasmtest-scheme-syntax-core-env.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-env.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-env.obj \
  out/selfbld/test/sasmtest-scheme-syntax-core-expand.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-expand.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-expand.obj \
  out/selfbld/test/sasmtest-scheme-syntax-core-macro.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-macro.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-macro.obj \
  out/selfbld/test/sasmtest-scheme-syntax-core-reference.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-reference.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-reference.obj \
  out/selfbld/test/sasmtest-scheme-syntax-core-specform.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-specform.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-specform.obj \
  out/selfbld/test/sasmtest-scheme-syntax-core.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core.obj \
  out/selfbld/test/sasmtest-scheme-syntax-expand-context.asm \
  out/selfbld/test/sasmtest-scheme-syntax-expand-context.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-expand-context.obj \
  out/selfbld/test/sasmtest-scheme-syntax-expander.asm \
  out/selfbld/test/sasmtest-scheme-syntax-expander.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-expander.obj \
  out/selfbld/test/sasmtest-scheme-syntax-specform.asm \
  out/selfbld/test/sasmtest-scheme-syntax-specform.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-specform.obj \
  out/selfbld/test/sasmtest-scheme-syntax-syntax.asm \
  out/selfbld/test/sasmtest-scheme-syntax-syntax.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-syntax.obj \
  out/selfbld/test/sasmtest-scheme-tag.asm \
  out/selfbld/test/sasmtest-scheme-tag.asmdiff \
  out/selfbld/test/sasmtest-scheme-tag.obj \
  out/selfbld/test/sasmtest-scheme-transforms-cps.asm \
  out/selfbld/test/sasmtest-scheme-transforms-cps.asmdiff \
  out/selfbld/test/sasmtest-scheme-transforms-cps.obj \
  out/selfbld/test/sasmtest-scheme-transforms-internal-defines.asm \
  out/selfbld/test/sasmtest-scheme-transforms-internal-defines.asmdiff \
  out/selfbld/test/sasmtest-scheme-transforms-internal-defines.obj \
  out/selfbld/test/sasmtest-util-counting.asm \
  out/selfbld/test/sasmtest-util-counting.asmdiff \
  out/selfbld/test/sasmtest-util-counting.obj \
  out/selfbld/test/sasmtest-util-filesystem.asm \
  out/selfbld/test/sasmtest-util-filesystem.asmdiff \
  out/selfbld/test/sasmtest-util-filesystem.obj \
  out/selfbld/test/sasmtest-util-format.asm \
  out/selfbld/test/sasmtest-util-format.asmdiff \
  out/selfbld/test/sasmtest-util-format.obj \
  out/selfbld/test/sasmtest-util-io.asm \
  out/selfbld/test/sasmtest-util-io.asmdiff \
  out/selfbld/test/sasmtest-util-io.obj \
  out/selfbld/test/sasmtest-util-list.asm \
  out/selfbld/test/sasmtest-util-list.asmdiff \
  out/selfbld/test/sasmtest-util-list.obj \
  out/selfbld/test/sasmtest-util-matrix.asm \
  out/selfbld/test/sasmtest-util-matrix.asmdiff \
  out/selfbld/test/sasmtest-util-matrix.obj \
  out/selfbld/test/sasmtest-util-output-file.asm \
  out/selfbld/test/sasmtest-util-output-file.asmdiff \
  out/selfbld/test/sasmtest-util-output-file.obj \
  out/selfbld/test/sasmtest-util-readhelp.asm \
  out/selfbld/test/sasmtest-util-readhelp.asmdiff \
  out/selfbld/test/sasmtest-util-readhelp.obj \
  out/selfbld/test/sasmtest-util-rfilact.asm \
  out/selfbld/test/sasmtest-util-rfilact.asmdiff \
  out/selfbld/test/sasmtest-util-rfilact.obj \
  out/selfbld/test/sasmtest-util-string.asm \
  out/selfbld/test/sasmtest-util-string.asmdiff \
  out/selfbld/test/sasmtest-util-string.obj \
  out/selfbld/test/sasmtest-util-symbol.asm \
  out/selfbld/test/sasmtest-util-symbol.asmdiff \
  out/selfbld/test/sasmtest-util-symbol.obj \
  out/selfbld/test/sasmtest-util-vector.asm \
  out/selfbld/test/sasmtest-util-vector.asmdiff \
  out/selfbld/test/sasmtest-util-vector.obj \
  \
  out/selfbld/test/sasmopttest-algo-graph.sasm-opt \
  out/selfbld/test/sasmopttest-algo-graph.sasm-optdiff \
  out/selfbld/test/sasmopttest-build-rules.sasm-opt \
  out/selfbld/test/sasmopttest-build-rules.sasm-optdiff \
  out/selfbld/test/sasmopttest-pat-pat.sasm-opt \
  out/selfbld/test/sasmopttest-pat-pat.sasm-optdiff \
  out/selfbld/test/sasmopttest-r5rs-library.sasm-opt \
  out/selfbld/test/sasmopttest-r5rs-library.sasm-optdiff \
  out/selfbld/test/sasmopttest-r5rs-wrap.sasm-opt \
  out/selfbld/test/sasmopttest-r5rs-wrap.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-fastgraph.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-fastgraph.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-fastset.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-fastset.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-machdesc.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-machdesc.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-arithmetic.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-arithmetic.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-base.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-base.sasm-optdiff \
  \
  out/selfbld/test/sasmopttest-sasm-nasmx86-bitwise-const.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-bitwise-const.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-call.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-call.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-compare.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-compare.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-control.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-control.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-data.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-data.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-debug.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-debug.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-interp.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-interp.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-labels.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-labels.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-load-array.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-load-array.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-machine.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-machine.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-mul.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-mul.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-preamble.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-preamble.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-return.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-return.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-shift.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-shift.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-stack.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-stack.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-store-array.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-store-array.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-util.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-util.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-nasmx86-binop.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-nasmx86-binop.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-opt.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-opt.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-constant-operand.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-constant-operand.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-data-entry.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-data-entry.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-extern.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-extern.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-instruction.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-instruction.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-lvalue-operand.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-lvalue-operand.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-member.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-member.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-operand.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-operand.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-operation.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-operation.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-program.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-program.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-register-operand.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-register-operand.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-statement.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-statement.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-symconst-entry.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-symconst-entry.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-syntax.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-syntax.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-parse-util.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-parse-util.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-analyze.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-analyze.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-ast.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-ast.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-codegen.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-codegen.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-core.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-core.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-dataflow.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-dataflow.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-insel.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-insel.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-insn.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-insn.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-nasmx86.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-nasmx86.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-opt.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-opt.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-parse.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-parse.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-regalloc.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-regalloc.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-tracing.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-tracing.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-tx.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-tx.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm-visitor.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm-visitor.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-sasm.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-sasm.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-assemble.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-assemble.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-config.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-config.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-context.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-context.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-emit.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-emit.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-globals.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-globals.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-labels.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-labels.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-main.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-main.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-read.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-read.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-registers.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-registers.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-regrule.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-regrule.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-rewrite.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-rewrite.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-stmt.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-stmt.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-symconst.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-symconst.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm-tx-util.sasm-opt \
  out/selfbld/test/sasmopttest-sasm-tx-util.sasm-optdiff \
  out/selfbld/test/sasmopttest-sasm.sasm-opt \
  out/selfbld/test/sasmopttest-sasm.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-base-syntax.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-base-syntax.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-application.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-application.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-emit.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-emit.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-epilogue.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-epilogue.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-immediate.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-immediate.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-lambda.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-lambda.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-letrec.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-letrec.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-pretty-print.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-pretty-print.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-reference.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-reference.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-set.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-set.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-tailcall.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-tailcall.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-targets.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen-targets.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-codegen.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-context.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-context.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-cps.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-cps.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-env.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-env.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-gc.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-gc.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-labels.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-labels.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-linkage.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-linkage.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler-settings.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler-settings.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-compiler.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-compiler.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-core-context.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-core-context.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-core-env.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-core-env.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-core-expand.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-core-expand.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-core-macro.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-core-macro.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-core-reference.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-core-reference.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-core-specform.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-core-specform.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-core.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-core.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-expand-context.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-expand-context.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-expander.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-expander.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-specform.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-specform.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-syntax-syntax.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-syntax-syntax.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-tag.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-tag.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-transforms-cps.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-transforms-cps.sasm-optdiff \
  out/selfbld/test/sasmopttest-scheme-transforms-internal-defines.sasm-opt \
  out/selfbld/test/sasmopttest-scheme-transforms-internal-defines.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-counting.sasm-opt \
  out/selfbld/test/sasmopttest-util-counting.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-filesystem.sasm-opt \
  out/selfbld/test/sasmopttest-util-filesystem.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-format.sasm-opt \
  out/selfbld/test/sasmopttest-util-format.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-io.sasm-opt \
  out/selfbld/test/sasmopttest-util-io.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-list.sasm-opt \
  out/selfbld/test/sasmopttest-util-list.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-matrix.sasm-opt \
  out/selfbld/test/sasmopttest-util-matrix.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-output-file.sasm-opt \
  out/selfbld/test/sasmopttest-util-output-file.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-readhelp.sasm-opt \
  out/selfbld/test/sasmopttest-util-readhelp.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-rfilact.sasm-opt \
  out/selfbld/test/sasmopttest-util-rfilact.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-string.sasm-opt \
  out/selfbld/test/sasmopttest-util-string.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-symbol.sasm-opt \
  out/selfbld/test/sasmopttest-util-symbol.sasm-optdiff \
  out/selfbld/test/sasmopttest-util-vector.sasm-opt \
  out/selfbld/test/sasmopttest-util-vector.sasm-optdiff \
  \
  out/selfbld/test/schemectest-tests-apply.sasm \
  out/selfbld/test/schemectest-tests-cseconds.sasm \
  out/selfbld/test/schemectest-tests-disptest.sasm \
  out/selfbld/test/schemectest-tests-disptest2.sasm \
  out/selfbld/test/schemectest-tests-argv.sasm \
  out/selfbld/test/schemectest-tests-rename.sasm \
  out/selfbld/test/schemectest-tests-delete.sasm \
  out/selfbld/test/schemectest-tests-stat.sasm \
  out/selfbld/test/schemectest-tests-getenv.sasm \
  out/selfbld/test/schemectest-tests-outputfile.sasm \
  out/selfbld/test/schemectest-tests-inputfile.sasm \
  out/selfbld/test/schemectest-tests-read2.sasm \
  out/selfbld/test/schemectest-tests-read3.sasm \
  out/selfbld/test/schemectest-tests-peekchar.sasm \
  out/selfbld/test/schemectest-tests-mkvec.sasm \
  out/selfbld/test/schemectest-tests-eqv.sasm \
  out/selfbld/test/schemectest-tests-eq.sasm \
  out/selfbld/test/schemectest-tests-sym1.sasm \
  out/selfbld/test/schemectest-tests-opplus.sasm \
  out/selfbld/test/schemectest-tests-opgt.sasm \
  out/selfbld/test/schemectest-tests-oplt.sasm \
  out/selfbld/test/schemectest-tests-opmul.sasm \
  out/selfbld/test/schemectest-tests-pair.sasm \
  out/selfbld/test/schemectest-tests-mkstring.sasm \
  out/selfbld/test/schemectest-tests-str2num.sasm \
  out/selfbld/test/schemectest-tests-opdiv.sasm \
  out/selfbld/test/schemectest-tests-char.sasm \
  out/selfbld/test/schemectest-tests-pred.sasm \
  out/selfbld/test/schemectest-tests-write.sasm \
  out/selfbld/test/schemectest-tests-r5rs1.sasm \
  out/selfbld/test/schemectest-tests-r5rs3.sasm \
  out/selfbld/test/schemectest-tests-char-case.sasm \
  out/selfbld/test/schemectest-tests-string.sasm \
  out/selfbld/test/schemectest-tests-read6.sasm \
  out/selfbld/test/schemectest-tests-str2num2.sasm \
  out/selfbld/test/schemectest-tests-num2str.sasm \
  out/selfbld/test/schemectest-tests-set1.sasm \
  out/selfbld/test/schemectest-tests-badapply.sasm \
  out/selfbld/test/schemectest-tests-gc1.sasm \
  out/selfbld/test/schemectest-tests-vararg.sasm \
  out/selfbld/test/schemectest-tests-read4.sasm \
  out/selfbld/test/schemectest-tests-read5.sasm \
  out/selfbld/test/schemectest-tests-opeq.sasm \
  out/selfbld/test/schemectest-tests-opminus.sasm \
  out/selfbld/test/schemectest-tests-equal.sasm \
  out/selfbld/test/schemectest-tests-r5rs2.sasm \
  out/selfbld/test/schemectest-tests-read7.sasm \
  out/selfbld/test/schemectest-tests-read8.sasm \
  out/selfbld/test/schemectest-tests-read.sasm \
  \
  out/sasm-selfbld.out \
  out/sasm-opt-selfbld.out \
  out/scheme-compiler-selfbld.out


out/selfbld/test/bootstrap-tests.done: $(DEPEND_SELFBLD_TEST_DONE)
	cmd.exe /c "echo tests.done>out\selfbld\test\bootstrap-tests.done"

out/selfbld/test/util-string.sasm-opt: out/bootstrap/util-string.sasm $(SELFBLD_TEST_DIR) $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

out/selfbld/test/util-string.asm: out/selfbld/test/util-string.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/test/util-string.obj: out/selfbld/test/util-string.asm
	nasm -fwin32 $< -o $@

out/selfbld/test/gc-invoke.sasm-opt: rtl/gc-invoke.sasm $(SELFBLD_TEST_DIR) $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

out/selfbld/test/gc-invoke.asm: out/selfbld/test/gc-invoke.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/test/sasmopttest-%.sasm-opt: out/bootstrap/%.sasm $(BOOTSTRAP_SASM_OPT) $(SELFBLD_TEST_DIR)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

out/selfbld/test/sasmtest-%.asm: out/bootstrap/%.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/test/sasmtest-%.asmdiff: out/selfbld/test/sasmtest-%.asm out/bootstrap/%.asm
	fc.exe  $(subst /,\,$<) $(subst /,\,$(patsubst out/selfbld/test/sasmtest-%.asm,out/bootstrap/%.asm,$<))
	cmd.exe /c "echo same>$(subst /,\,$@)"

# --ignore-case here is suspicious, I'm not clear why the symbols are all coming out lower-case in bootstrap
out/selfbld/test/sasmopttest-%.sasm-optdiff: out/selfbld/test/sasmopttest-%.sasm-opt out/bootstrap/%.sasm-opt
	fc.exe /c $(subst /,\,$<) $(subst /,\,$(patsubst out/selfbld/test/sasmopttest-%.sasm-opt,out/bootstrap/%.sasm-opt,$<))
	cmd.exe /c "echo same>$(subst /,\,$@)"

out/selfbld/test/schemectest-tests-%.sasmdiff: out/selfbld/test/schemectest-tests-%.sasm out/bootstrap/test/%.sasm
	fc.exe /c $(subst /,\,$<) $(subst /,\,$(patsubst out/selfbld/test/schemectest-tests-%.sasm,out/bootstrap/test/%.sasm,$<))
	cmd.exe /c "echo same>$(subst /,\,$@)"

out/selfbld/test/schemectest-tests-%.sasm : tests/%.scm $(BOOTSTRAP_SCHEMEC)
	$(BOOTSTRAP_SCHEMEC) $< --output $@

out/selfbld/test/%.obj: out/selfbld/test/%.asm
	nasm -fwin32 $< -o $@

DEPEND_TOOLSET=\
  $(BOOTSTRAP_SCHEMEC) \
  $(BOOTSTRAP_SASM_OPT) \
  $(BOOTSTRAP_SASMC)

out/sasm-selfbld.out: env-selfbld.cmd out/bootstrap-sasm-ts.cmd $(DEPEND_TOOLSET)
	cmd.exe /c "call env-selfbld.cmd & call out\bootstrap-sasm-ts.cmd"
	cmd.exe /c "echo selfbld>out\sasm-selfbld.out"

out/sasm-opt-selfbld.out: env-selfbld.cmd out/bootstrap-sasm-opt-ts.cmd $(DEPEND_TOOLSET)
	cmd.exe /c "call env-selfbld.cmd & call out\bootstrap-sasm-opt-ts.cmd"
	cmd.exe /c "echo selfbld>out\sasm-opt-selfbld.out"

out/scheme-compiler-selfbld.out: out/bootstrap-scheme-compiler-ts.cmd $(DEPEND_SCHEMEC)
	cmd.exe /c "call env-selfbld.cmd & call out\bootstrap-scheme-compiler-ts.cmd"
	cmd.exe /c "echo selfbld>out\scheme-compiler-selfbld.out"

DEPEND_MJ_RTL_C=\
  rtl/mjrtl.c

DEPEND_RTL_C=\
  $(DEPEND_MJ_RTL_C) \
  rtl/rtlscheme.c

DEPEND_RTL_OBJS=\
 out/selfbld/c-rtlheap.obj \
 out/selfbld/gc.obj \
 out/selfbld/gc-wrapper.obj \
 out/selfbld/gc-invoke.obj \
 out/selfbld/gc-stack.obj \
 out/selfbld/gc-mark.obj \
 out/selfbld/gc-mark-range.obj \
 out/selfbld/gc-mark-array.obj \
 out/selfbld/gc-mark-class.obj \
 out/selfbld/gc-sweep.obj \
 out/selfbld/heap.obj \
 out/selfbld/heapfixed.obj \
 out/selfbld/heapvar.obj \
 out/selfbld/mjrtl.obj \
 out/selfbld/rtlheap.obj \
 out/selfbld/debug.obj

DEPEND_SCHEME_RTL_OMIT_MAIN=\
  out/selfbld/r5rs-library.obj \
  out/selfbld/r5rs-native.obj \
  out/selfbld/r5rs-wrap.obj \
  out/selfbld/rtlscheme.obj \
  out/selfbld/scheme-java.obj \
  out/selfbld/scheme.obj

DEPEND_RTL=\
  $(DEPEND_RTL_C) \
  $(DEPEND_RTL_OBJS) \
  $(DEPEND_SCHEME_RTL_OMIT_MAIN)

# Compile scheme RTL
out/selfbld/r5rs-library.sasm: rtl/r5rs-library.scm $(BOOTSTRAP_SCHEMEC)
	$(BOOTSTRAP_SCHEMEC) rtl/r5rs-library.scm --outdir out\selfbld --conspiracy --no-entry
	cmd.exe /c "copy out\selfbld\rtl-r5rs-library.sasm out\selfbld\r5rs-library.sasm"

out/selfbld/r5rs-library.sasm-opt : out/selfbld/r5rs-library.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

out/selfbld/r5rs-wrap.sasm: rtl/r5rs-wrap.scm $(BOOTSTRAP_SCHEMEC)
	$(BOOTSTRAP_SCHEMEC) rtl/r5rs-wrap.scm --outdir out\selfbld --conspiracy --no-entry
	cmd.exe /c "copy out\selfbld\rtl-r5rs-wrap.sasm out\selfbld\r5rs-wrap.sasm"

out/selfbld/r5rs-wrap.sasm-opt : out/selfbld/r5rs-wrap.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

# out/selfbld/r5rs-native.sasm : rtl/r5rs-native.scm $(DEPEND_GLUEC) $(SELFBLD_DIR)
# 	$(GLUEC) rtl/r5rs-native.scm -o out/selfbld/r5rs-native.sasm

out/selfbld/r5rs-native.sasm: out/bootstrap/r5rs-native.sasm
	cmd.exe /c "copy out\bootstrap\r5rs-native.sasm out\selfbld\r5rs-native.sasm"

out/selfbld/r5rs-native.sasm-opt: out/selfbld/r5rs-native.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

# rtl rules
out/selfbld/debug.asm: rtl/debug.asm
	cmd.exe /c "copy rtl\debug.asm out\selfbld\debug.asm"

out/selfbld/%.asm: out/selfbld/%.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/%.obj: out/selfbld/%.asm
	nasm -fwin32 $< -o $@

out/selfbld/scheme.sasm : out/bootstrap/scheme.sasm
	cmd.exe /c "copy out\bootstrap\scheme.sasm out\selfbld\scheme.sasm"
#	$(BOOTSTRAP_JAVAC) -l --out=$@ rtl/scheme.java

out/selfbld/scheme.sasm-opt: out/selfbld/scheme.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

# RTL rules which have to go under specific rules above
out/selfbld/%.sasm-opt: rtl/%.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

SCHEME_CFLAGS=-DSCHEME_RTL=1

out/selfbld/sasm.exe: $(DEPEND_RTL) out/sasm-selfbld.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/selfbld $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/selfbld/sasm.obj -o $@ -lsasm

out/selfbld/sasm-opt.exe: $(DEPEND_RTL) out/sasm-opt-selfbld.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/selfbld $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/selfbld/sasm-opt.obj -o $@ -lsasm-opt

out/selfbld/schemec.exe: $(DEPEND_RTL) out/scheme-compiler-selfbld.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/selfbld $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/selfbld/scheme-compiler.obj -o $@ -lscheme-compiler

DEPEND_SCHEME_RTL=\
  $(DEPEND_SCHEME_RTL_OMIT_MAIN) \
  out/bootstrap/scheme-main.obj

SELFBLD_SCHEMEC=out/selfbld/schemec.exe
SELFBLD_SASMOPT=out/selfbld/sasm-opt.exe
SELFBLD_SASMC=out/selfbld/sasm.exe

TEST_OUTDIR_NAME=selfbld
TEST_SCHEMEC=$(SELFBLD_SCHEMEC)
TEST_SASMOPT=$(SELFBLD_SASMOPT)
TEST_SASMC=$(SELFBLD_SASMC)
DEPEND_TEST_SCHEMEC=$(TEST_SCHEMEC)
DEPEND_TEST_SASMOPT=$(TEST_SASMOPT)
DEPEND_TEST_SASMC=$(TEST_SASMC)
DEPEND_TEST_OUTPUT_DIR=$(SELFBLD_TEST_DIR)
TEST_SCHEME_CFLAGS=$(SCHEME_CFLAGS)
DEPEND_TEST_RTL=$(DEPEND_RTL)
DEPEND_TEST_RTL_C=$(DEPEND_RTL_C)
DEPEND_TEST_SCHEME_RTL=$(DEPEND_SCHEME_RTL)
DEPEND_TEST_SCHEME_RTL_OMIT_MAIN=$(DEPEND_SCHEME_RTL_OMIT_MAIN)
DEPEND_TEST_RTL=$(DEPEND_RTL)
DEPEND_TEST_RTL_OBJS=$(DEPEND_RTL_OBJS)

include tests.makefile
include tests.win32.makefile

out/selfbld/test/selfbld-tests.done: $(ALL_SCHEME_TESTS)
	cmd.exe /c "echo tests.done>out\selfbld\test\selfbld-tests.done"

