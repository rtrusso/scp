DEPEND_ALL=\
  out/selfbld/test/bootstrap-tests.done \
  \
  out/selfbld/sasm.exe \
  out/selfbld/sasm-opt.exe \
  out/selfbld/schemec.exe \
  out/selfbld/test/selfbld-tests.done

SELFBLD_DIR=out/selfbld/.exists
SELFBLD_TEST_DIR=out/selfbld/test/.exists

BOOTSTRAP_SASM_OPT=out/bootstrap/sasm-opt.exe
BOOTSTRAP_SASMC=out/bootstrap/sasm.exe
BOOTSTRAP_SCHEMEC=out/bootstrap/schemec.exe

selfbld_all: $(DEPEND_ALL)

.SECONDARY:

# selfbld
$(SELFBLD_DIR): out/.exists
	if [ ! -d out/selfbld ]; then mkdir out/selfbld; fi
	touch out/selfbld/.exists

$(SELFBLD_TEST_DIR): $(SELFBLD_DIR)
	if [ ! -d out/selfbld/test ]; then mkdir out/selfbld/test; fi
	touch out/selfbld/test/.exists

DEPEND_SELFBLD_TEST_DONE=\
  out/selfbld/test/util-string.sasm-opt \
  out/selfbld/test/util-string.asm \
  out/selfbld/test/util-string.o \
  out/selfbld/test/gc-invoke.sasm-opt \
  out/selfbld/test/gc-invoke.asm \
  out/selfbld/test/gc-invoke.o \
  out/selfbld/test/sasmtest-gc-invoke.asm \
  out/selfbld/test/sasmtest-gc-invoke.asmdiff \
  out/selfbld/test/sasmtest-gc-invoke.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.o \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asm \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.o \
  out/selfbld/test/sasmtest-algo-graph.asm \
  out/selfbld/test/sasmtest-algo-graph.asmdiff \
  out/selfbld/test/sasmtest-algo-graph.o \
  out/selfbld/test/sasmtest-build-rules.asm \
  out/selfbld/test/sasmtest-build-rules.asmdiff \
  out/selfbld/test/sasmtest-build-rules.o \
  out/selfbld/test/sasmtest-c-rtlheap.asm \
  out/selfbld/test/sasmtest-c-rtlheap.asmdiff \
  out/selfbld/test/sasmtest-c-rtlheap.o \
  out/selfbld/test/sasmtest-gc-invoke.asm \
  out/selfbld/test/sasmtest-gc-invoke.asmdiff \
  out/selfbld/test/sasmtest-gc-invoke.o \
  out/selfbld/test/sasmtest-gc-mark-array.asm \
  out/selfbld/test/sasmtest-gc-mark-array.asmdiff \
  out/selfbld/test/sasmtest-gc-mark-array.o \
  out/selfbld/test/sasmtest-gc-mark-class.asm \
  out/selfbld/test/sasmtest-gc-mark-class.asmdiff \
  out/selfbld/test/sasmtest-gc-mark-class.o \
  out/selfbld/test/sasmtest-gc-mark-range.asm \
  out/selfbld/test/sasmtest-gc-mark-range.asmdiff \
  out/selfbld/test/sasmtest-gc-mark-range.o \
  out/selfbld/test/sasmtest-gc-mark.asm \
  out/selfbld/test/sasmtest-gc-mark.asmdiff \
  out/selfbld/test/sasmtest-gc-mark.o \
  out/selfbld/test/sasmtest-gc-stack.asm \
  out/selfbld/test/sasmtest-gc-stack.asmdiff \
  out/selfbld/test/sasmtest-gc-stack.o \
  out/selfbld/test/sasmtest-gc-sweep.asm \
  out/selfbld/test/sasmtest-gc-sweep.asmdiff \
  out/selfbld/test/sasmtest-gc-sweep.o \
  out/selfbld/test/sasmtest-gc-wrapper.asm \
  out/selfbld/test/sasmtest-gc-wrapper.asmdiff \
  out/selfbld/test/sasmtest-gc-wrapper.o \
  out/selfbld/test/sasmtest-gc.asm \
  out/selfbld/test/sasmtest-gc.asmdiff \
  out/selfbld/test/sasmtest-gc.o \
  out/selfbld/test/sasmtest-heap.asm \
  out/selfbld/test/sasmtest-heap.asmdiff \
  out/selfbld/test/sasmtest-heap.o \
  out/selfbld/test/sasmtest-heapfixed.asm \
  out/selfbld/test/sasmtest-heapfixed.asmdiff \
  out/selfbld/test/sasmtest-heapfixed.o \
  out/selfbld/test/sasmtest-heapvar.asm \
  out/selfbld/test/sasmtest-heapvar.asmdiff \
  out/selfbld/test/sasmtest-heapvar.o \
  out/selfbld/test/sasmtest-mjrtl.asm \
  out/selfbld/test/sasmtest-mjrtl.asmdiff \
  out/selfbld/test/sasmtest-mjrtl.o \
  out/selfbld/test/sasmtest-pat-pat.asm \
  out/selfbld/test/sasmtest-pat-pat.asmdiff \
  out/selfbld/test/sasmtest-pat-pat.o \
  out/selfbld/test/sasmtest-r5rs-library.asm \
  out/selfbld/test/sasmtest-r5rs-library.asmdiff \
  out/selfbld/test/sasmtest-r5rs-library.o \
  out/selfbld/test/sasmtest-r5rs-native.asm \
  out/selfbld/test/sasmtest-r5rs-native.asmdiff \
  out/selfbld/test/sasmtest-r5rs-native.o \
  out/selfbld/test/sasmtest-r5rs-wrap.asm \
  out/selfbld/test/sasmtest-r5rs-wrap.asmdiff \
  out/selfbld/test/sasmtest-r5rs-wrap.o \
  out/selfbld/test/sasmtest-rtlheap.asm \
  out/selfbld/test/sasmtest-rtlheap.asmdiff \
  out/selfbld/test/sasmtest-rtlheap.o \
  out/selfbld/test/sasmtest-rtlscheme.asm \
  out/selfbld/test/sasmtest-rtlscheme.asmdiff \
  out/selfbld/test/sasmtest-rtlscheme.o \
  out/selfbld/test/sasmtest-sasm-fastgraph.asm \
  out/selfbld/test/sasmtest-sasm-fastgraph.asmdiff \
  out/selfbld/test/sasmtest-sasm-fastgraph.o \
  out/selfbld/test/sasmtest-sasm-fastset.asm \
  out/selfbld/test/sasmtest-sasm-fastset.asmdiff \
  out/selfbld/test/sasmtest-sasm-fastset.o \
  out/selfbld/test/sasmtest-sasm-machdesc.asm \
  out/selfbld/test/sasmtest-sasm-machdesc.asmdiff \
  out/selfbld/test/sasmtest-sasm-machdesc.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-arithmetic.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-arithmetic.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-arithmetic.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-base.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-base.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-base.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-binop.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-binop.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-binop.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-bitwise-const.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-call.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-call.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-call.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-compare.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-compare.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-compare.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-control.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-control.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-control.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-data.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-data.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-data.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-debug.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-debug.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-debug.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-interp.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-interp.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-interp.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-labels.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-labels.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-labels.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-load-array.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-load-array.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-load-array.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-machine.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-machine.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-machine.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-mul.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-mul.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-mul.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-preamble.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-preamble.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-preamble.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-return.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-return.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-return.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-shift.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-shift.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-shift.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-stack.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-stack.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-stack.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-store-array.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-store-array.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-store-array.o \
  out/selfbld/test/sasmtest-sasm-nasmx86-util.asm \
  out/selfbld/test/sasmtest-sasm-nasmx86-util.asmdiff \
  out/selfbld/test/sasmtest-sasm-nasmx86-util.o \
  out/selfbld/test/sasmtest-sasm-opt.asm \
  out/selfbld/test/sasmtest-sasm-opt.asmdiff \
  out/selfbld/test/sasmtest-sasm-opt.o \
  out/selfbld/test/sasmtest-sasm-parse-constant-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-constant-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-constant-operand.o \
  out/selfbld/test/sasmtest-sasm-parse-data-entry.asm \
  out/selfbld/test/sasmtest-sasm-parse-data-entry.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-data-entry.o \
  out/selfbld/test/sasmtest-sasm-parse-extern.asm \
  out/selfbld/test/sasmtest-sasm-parse-extern.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-extern.o \
  out/selfbld/test/sasmtest-sasm-parse-instruction.asm \
  out/selfbld/test/sasmtest-sasm-parse-instruction.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-instruction.o \
  out/selfbld/test/sasmtest-sasm-parse-lvalue-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-lvalue-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-lvalue-operand.o \
  out/selfbld/test/sasmtest-sasm-parse-member.asm \
  out/selfbld/test/sasmtest-sasm-parse-member.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-member.o \
  out/selfbld/test/sasmtest-sasm-parse-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-operand.o \
  out/selfbld/test/sasmtest-sasm-parse-operation.asm \
  out/selfbld/test/sasmtest-sasm-parse-operation.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-operation.o \
  out/selfbld/test/sasmtest-sasm-parse-program.asm \
  out/selfbld/test/sasmtest-sasm-parse-program.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-program.o \
  out/selfbld/test/sasmtest-sasm-parse-register-operand.asm \
  out/selfbld/test/sasmtest-sasm-parse-register-operand.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-register-operand.o \
  out/selfbld/test/sasmtest-sasm-parse-statement.asm \
  out/selfbld/test/sasmtest-sasm-parse-statement.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-statement.o \
  out/selfbld/test/sasmtest-sasm-parse-symconst-entry.asm \
  out/selfbld/test/sasmtest-sasm-parse-symconst-entry.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-symconst-entry.o \
  out/selfbld/test/sasmtest-sasm-parse-syntax.asm \
  out/selfbld/test/sasmtest-sasm-parse-syntax.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-syntax.o \
  out/selfbld/test/sasmtest-sasm-parse-util.asm \
  out/selfbld/test/sasmtest-sasm-parse-util.asmdiff \
  out/selfbld/test/sasmtest-sasm-parse-util.o \
  out/selfbld/test/sasmtest-sasm-sasm-analyze.asm \
  out/selfbld/test/sasmtest-sasm-sasm-analyze.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-analyze.o \
  out/selfbld/test/sasmtest-sasm-sasm-ast.asm \
  out/selfbld/test/sasmtest-sasm-sasm-ast.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-ast.o \
  out/selfbld/test/sasmtest-sasm-sasm-codegen.asm \
  out/selfbld/test/sasmtest-sasm-sasm-codegen.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-codegen.o \
  out/selfbld/test/sasmtest-sasm-sasm-core.asm \
  out/selfbld/test/sasmtest-sasm-sasm-core.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-core.o \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asm \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-dataflow.o \
  out/selfbld/test/sasmtest-sasm-sasm-insel.asm \
  out/selfbld/test/sasmtest-sasm-sasm-insel.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-insel.o \
  out/selfbld/test/sasmtest-sasm-sasm-insn.asm \
  out/selfbld/test/sasmtest-sasm-sasm-insn.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-insn.o \
  out/selfbld/test/sasmtest-sasm-sasm-nasmx86.asm \
  out/selfbld/test/sasmtest-sasm-sasm-nasmx86.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-nasmx86.o \
  out/selfbld/test/sasmtest-sasm-sasm-opt.asm \
  out/selfbld/test/sasmtest-sasm-sasm-opt.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-opt.o \
  out/selfbld/test/sasmtest-sasm-sasm-parse.asm \
  out/selfbld/test/sasmtest-sasm-sasm-parse.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-parse.o \
  out/selfbld/test/sasmtest-sasm-sasm-regalloc.asm \
  out/selfbld/test/sasmtest-sasm-sasm-regalloc.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-regalloc.o \
  out/selfbld/test/sasmtest-sasm-sasm-tracing.asm \
  out/selfbld/test/sasmtest-sasm-sasm-tracing.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-tracing.o \
  out/selfbld/test/sasmtest-sasm-sasm-tx.asm \
  out/selfbld/test/sasmtest-sasm-sasm-tx.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-tx.o \
  out/selfbld/test/sasmtest-sasm-sasm-visitor.asm \
  out/selfbld/test/sasmtest-sasm-sasm-visitor.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm-visitor.o \
  out/selfbld/test/sasmtest-sasm-sasm.asm \
  out/selfbld/test/sasmtest-sasm-sasm.asmdiff \
  out/selfbld/test/sasmtest-sasm-sasm.o \
  out/selfbld/test/sasmtest-sasm-tx-assemble.asm \
  out/selfbld/test/sasmtest-sasm-tx-assemble.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-assemble.o \
  out/selfbld/test/sasmtest-sasm-tx-config.asm \
  out/selfbld/test/sasmtest-sasm-tx-config.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-config.o \
  out/selfbld/test/sasmtest-sasm-tx-context.asm \
  out/selfbld/test/sasmtest-sasm-tx-context.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-context.o \
  out/selfbld/test/sasmtest-sasm-tx-emit.asm \
  out/selfbld/test/sasmtest-sasm-tx-emit.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-emit.o \
  out/selfbld/test/sasmtest-sasm-tx-globals.asm \
  out/selfbld/test/sasmtest-sasm-tx-globals.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-globals.o \
  out/selfbld/test/sasmtest-sasm-tx-labels.asm \
  out/selfbld/test/sasmtest-sasm-tx-labels.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-labels.o \
  out/selfbld/test/sasmtest-sasm-tx-main.asm \
  out/selfbld/test/sasmtest-sasm-tx-main.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-main.o \
  out/selfbld/test/sasmtest-sasm-tx-read.asm \
  out/selfbld/test/sasmtest-sasm-tx-read.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-read.o \
  out/selfbld/test/sasmtest-sasm-tx-registers.asm \
  out/selfbld/test/sasmtest-sasm-tx-registers.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-registers.o \
  out/selfbld/test/sasmtest-sasm-tx-regrule.asm \
  out/selfbld/test/sasmtest-sasm-tx-regrule.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-regrule.o \
  out/selfbld/test/sasmtest-sasm-tx-rewrite.asm \
  out/selfbld/test/sasmtest-sasm-tx-rewrite.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-rewrite.o \
  out/selfbld/test/sasmtest-sasm-tx-stmt.asm \
  out/selfbld/test/sasmtest-sasm-tx-stmt.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-stmt.o \
  out/selfbld/test/sasmtest-sasm-tx-symconst.asm \
  out/selfbld/test/sasmtest-sasm-tx-symconst.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-symconst.o \
  out/selfbld/test/sasmtest-sasm-tx-util.asm \
  out/selfbld/test/sasmtest-sasm-tx-util.asmdiff \
  out/selfbld/test/sasmtest-sasm-tx-util.o \
  out/selfbld/test/sasmtest-sasm.asm \
  out/selfbld/test/sasmtest-sasm.asmdiff \
  out/selfbld/test/sasmtest-sasm.o \
  out/selfbld/test/sasmtest-scheme-base-syntax.asm \
  out/selfbld/test/sasmtest-scheme-base-syntax.asmdiff \
  out/selfbld/test/sasmtest-scheme-base-syntax.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-application.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-application.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-application.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-emit.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-emit.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-emit.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-epilogue.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-epilogue.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-epilogue.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-immediate.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-immediate.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-immediate.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-lambda.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-lambda.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-lambda.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-letrec.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-letrec.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-letrec.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-pretty-print.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-pretty-print.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-pretty-print.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-reference.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-reference.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-reference.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-set.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-set.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-set.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-tailcall.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-tailcall.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-tailcall.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-targets.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-targets.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen-targets.o \
  out/selfbld/test/sasmtest-scheme-compiler-codegen.asm \
  out/selfbld/test/sasmtest-scheme-compiler-codegen.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-codegen.o \
  out/selfbld/test/sasmtest-scheme-compiler-context.asm \
  out/selfbld/test/sasmtest-scheme-compiler-context.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-context.o \
  out/selfbld/test/sasmtest-scheme-compiler-cps.asm \
  out/selfbld/test/sasmtest-scheme-compiler-cps.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-cps.o \
  out/selfbld/test/sasmtest-scheme-compiler-env.asm \
  out/selfbld/test/sasmtest-scheme-compiler-env.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-env.o \
  out/selfbld/test/sasmtest-scheme-compiler-gc.asm \
  out/selfbld/test/sasmtest-scheme-compiler-gc.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-gc.o \
  out/selfbld/test/sasmtest-scheme-compiler-labels.asm \
  out/selfbld/test/sasmtest-scheme-compiler-labels.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-labels.o \
  out/selfbld/test/sasmtest-scheme-compiler-linkage.asm \
  out/selfbld/test/sasmtest-scheme-compiler-linkage.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-linkage.o \
  out/selfbld/test/sasmtest-scheme-compiler-settings.asm \
  out/selfbld/test/sasmtest-scheme-compiler-settings.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler-settings.o \
  out/selfbld/test/sasmtest-scheme-compiler.asm \
  out/selfbld/test/sasmtest-scheme-compiler.asmdiff \
  out/selfbld/test/sasmtest-scheme-compiler.o \
  out/selfbld/test/sasmtest-scheme-main.asm \
  out/selfbld/test/sasmtest-scheme-main.asmdiff \
  out/selfbld/test/sasmtest-scheme-main.o \
  out/selfbld/test/sasmtest-scheme-syntax-core-context.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-context.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-context.o \
  out/selfbld/test/sasmtest-scheme-syntax-core-env.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-env.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-env.o \
  out/selfbld/test/sasmtest-scheme-syntax-core-expand.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-expand.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-expand.o \
  out/selfbld/test/sasmtest-scheme-syntax-core-macro.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-macro.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-macro.o \
  out/selfbld/test/sasmtest-scheme-syntax-core-reference.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-reference.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-reference.o \
  out/selfbld/test/sasmtest-scheme-syntax-core-specform.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core-specform.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core-specform.o \
  out/selfbld/test/sasmtest-scheme-syntax-core.asm \
  out/selfbld/test/sasmtest-scheme-syntax-core.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-core.o \
  out/selfbld/test/sasmtest-scheme-syntax-expand-context.asm \
  out/selfbld/test/sasmtest-scheme-syntax-expand-context.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-expand-context.o \
  out/selfbld/test/sasmtest-scheme-syntax-expander.asm \
  out/selfbld/test/sasmtest-scheme-syntax-expander.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-expander.o \
  out/selfbld/test/sasmtest-scheme-syntax-specform.asm \
  out/selfbld/test/sasmtest-scheme-syntax-specform.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-specform.o \
  out/selfbld/test/sasmtest-scheme-syntax-syntax.asm \
  out/selfbld/test/sasmtest-scheme-syntax-syntax.asmdiff \
  out/selfbld/test/sasmtest-scheme-syntax-syntax.o \
  out/selfbld/test/sasmtest-scheme-tag.asm \
  out/selfbld/test/sasmtest-scheme-tag.asmdiff \
  out/selfbld/test/sasmtest-scheme-tag.o \
  out/selfbld/test/sasmtest-scheme-transforms-cps.asm \
  out/selfbld/test/sasmtest-scheme-transforms-cps.asmdiff \
  out/selfbld/test/sasmtest-scheme-transforms-cps.o \
  out/selfbld/test/sasmtest-scheme-transforms-internal-defines.asm \
  out/selfbld/test/sasmtest-scheme-transforms-internal-defines.asmdiff \
  out/selfbld/test/sasmtest-scheme-transforms-internal-defines.o \
  out/selfbld/test/sasmtest-util-counting.asm \
  out/selfbld/test/sasmtest-util-counting.asmdiff \
  out/selfbld/test/sasmtest-util-counting.o \
  out/selfbld/test/sasmtest-util-filesystem.asm \
  out/selfbld/test/sasmtest-util-filesystem.asmdiff \
  out/selfbld/test/sasmtest-util-filesystem.o \
  out/selfbld/test/sasmtest-util-format.asm \
  out/selfbld/test/sasmtest-util-format.asmdiff \
  out/selfbld/test/sasmtest-util-format.o \
  out/selfbld/test/sasmtest-util-io.asm \
  out/selfbld/test/sasmtest-util-io.asmdiff \
  out/selfbld/test/sasmtest-util-io.o \
  out/selfbld/test/sasmtest-util-list.asm \
  out/selfbld/test/sasmtest-util-list.asmdiff \
  out/selfbld/test/sasmtest-util-list.o \
  out/selfbld/test/sasmtest-util-matrix.asm \
  out/selfbld/test/sasmtest-util-matrix.asmdiff \
  out/selfbld/test/sasmtest-util-matrix.o \
  out/selfbld/test/sasmtest-util-output-file.asm \
  out/selfbld/test/sasmtest-util-output-file.asmdiff \
  out/selfbld/test/sasmtest-util-output-file.o \
  out/selfbld/test/sasmtest-util-readhelp.asm \
  out/selfbld/test/sasmtest-util-readhelp.asmdiff \
  out/selfbld/test/sasmtest-util-readhelp.o \
  out/selfbld/test/sasmtest-util-rfilact.asm \
  out/selfbld/test/sasmtest-util-rfilact.asmdiff \
  out/selfbld/test/sasmtest-util-rfilact.o \
  out/selfbld/test/sasmtest-util-string.asm \
  out/selfbld/test/sasmtest-util-string.asmdiff \
  out/selfbld/test/sasmtest-util-string.o \
  out/selfbld/test/sasmtest-util-symbol.asm \
  out/selfbld/test/sasmtest-util-symbol.asmdiff \
  out/selfbld/test/sasmtest-util-symbol.o \
  out/selfbld/test/sasmtest-util-vector.asm \
  out/selfbld/test/sasmtest-util-vector.asmdiff \
  out/selfbld/test/sasmtest-util-vector.o \
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
	touch out/selfbld/test/bootstrap-tests.done

out/selfbld/test/util-string.sasm-opt: out/bootstrap/util-string.sasm $(SELFBLD_TEST_DIR) $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

out/selfbld/test/util-string.asm: out/selfbld/test/util-string.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/test/util-string.o: out/selfbld/test/util-string.asm
	nasm -felf32 $< -o $@

out/selfbld/test/gc-invoke.sasm-opt: rtl/gc-invoke.sasm $(SELFBLD_TEST_DIR) $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

out/selfbld/test/gc-invoke.asm: out/selfbld/test/gc-invoke.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/test/sasmopttest-%.sasm-opt: out/bootstrap/%.sasm $(BOOTSTRAP_SASM_OPT) $(SELFBLD_TEST_DIR)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

out/selfbld/test/sasmtest-%.asm: out/bootstrap/%.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/test/sasmtest-%.asmdiff: out/selfbld/test/sasmtest-%.asm out/bootstrap/%.asm
	diff --strip-trailing-cr $< $(patsubst out/selfbld/test/sasmtest-%.asm,out/bootstrap/%.asm,$<)
	touch $@

# --ignore-case here is suspicious, I'm not clear why the symbols are all coming out lower-case in bootstrap
out/selfbld/test/sasmopttest-%.sasm-optdiff: out/selfbld/test/sasmopttest-%.sasm-opt out/bootstrap/%.sasm-opt
	diff --ignore-case --strip-trailing-cr $< $(patsubst out/selfbld/test/sasmopttest-%.sasm-opt,out/bootstrap/%.sasm-opt,$<)
	touch $@

out/selfbld/test/schemectest-tests-%.sasmdiff: out/selfbld/test/schemectest-tests-%.sasm out/bootstrap/test/%.sasm
	diff --ignore-case --strip-trailing-cr $< $(patsubst out/selfbld/test/schemectest-tests-%.sasm,out/bootstrap/test/%.sasm,$<)
	touch $@

out/selfbld/test/schemectest-tests-%.sasm : tests/%.scm $(BOOTSTRAP_SCHEMEC)
	$(BOOTSTRAP_SCHEMEC) $< --output $@

out/selfbld/test/%.o: out/selfbld/test/%.asm
	nasm -felf32 $< -o $@

DEPEND_TOOLSET=\
  $(BOOTSTRAP_SCHEMEC) \
  $(BOOTSTRAP_SASM_OPT) \
  $(BOOTSTRAP_SASMC)

out/sasm-selfbld.out: out/bootstrap-sasm-ts.sh env-selfbld.sh $(DEPEND_TOOLSET)
	. ./env-selfbld.sh; ./out/bootstrap-sasm-ts.sh

out/sasm-opt-selfbld.out: out/bootstrap-sasm-opt-ts.sh env-selfbld.sh $(DEPEND_TOOLSET)
	. ./env-selfbld.sh; ./out/bootstrap-sasm-opt-ts.sh

out/scheme-compiler-selfbld.out: out/bootstrap-scheme-compiler-ts.sh env-selfbld.sh $(DEPEND_SCHEMEC)
	. ./env-selfbld.sh; ./out/bootstrap-scheme-compiler-ts.sh

DEPEND_MJ_RTL_C=\
  rtl/mjrtl.c

DEPEND_RTL_C=\
  $(DEPEND_MJ_RTL_C) \
  rtl/rtlscheme.c

DEPEND_RTL_OBJS=\
 out/selfbld/c-rtlheap.o \
 out/selfbld/gc.o \
 out/selfbld/gc-wrapper.o \
 out/selfbld/gc-invoke.o \
 out/selfbld/gc-stack.o \
 out/selfbld/gc-mark.o \
 out/selfbld/gc-mark-range.o \
 out/selfbld/gc-mark-array.o \
 out/selfbld/gc-mark-class.o \
 out/selfbld/gc-sweep.o \
 out/selfbld/heap.o \
 out/selfbld/heapfixed.o \
 out/selfbld/heapvar.o \
 out/selfbld/mjrtl.o \
 out/selfbld/rtlheap.o \
 out/selfbld/debug.o

DEPEND_SCHEME_RTL_OMIT_MAIN=\
  out/selfbld/r5rs-library.o \
  out/selfbld/r5rs-native.o \
  out/selfbld/r5rs-wrap.o \
  out/selfbld/rtlscheme.o \
  out/selfbld/scheme-java.o \
  out/selfbld/scheme.o

DEPEND_RTL=\
  $(DEPEND_RTL_C) \
  $(DEPEND_RTL_OBJS) \
  $(DEPEND_SCHEME_RTL_OMIT_MAIN)

# Compile scheme RTL
out/selfbld/r5rs-library.sasm: rtl/r5rs-library.scm $(BOOTSTRAP_SCHEMEC)
	$(BOOTSTRAP_SCHEMEC) rtl/r5rs-library.scm --outdir out/selfbld --conspiracy --no-entry
	cp out/selfbld/rtl-r5rs-library.sasm out/selfbld/r5rs-library.sasm

out/selfbld/r5rs-library.sasm-opt : out/selfbld/r5rs-library.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

out/selfbld/r5rs-wrap.sasm: rtl/r5rs-wrap.scm $(BOOTSTRAP_SCHEMEC)
	$(BOOTSTRAP_SCHEMEC) rtl/r5rs-wrap.scm --outdir out/selfbld --conspiracy --no-entry
	cp out/selfbld/rtl-r5rs-wrap.sasm out/selfbld/r5rs-wrap.sasm

out/selfbld/r5rs-wrap.sasm-opt : out/selfbld/r5rs-wrap.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@ --cheap

# out/selfbld/r5rs-native.sasm : rtl/r5rs-native.scm $(DEPEND_GLUEC) $(SELFBLD_DIR)
# 	$(GLUEC) rtl/r5rs-native.scm -o out/selfbld/r5rs-native.sasm

out/selfbld/r5rs-native.sasm: out/bootstrap/r5rs-native.sasm
	cp out/bootstrap/r5rs-native.sasm out/selfbld/r5rs-native.sasm

out/selfbld/r5rs-native.sasm-opt: out/selfbld/r5rs-native.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

# rtl rules
out/selfbld/debug.asm: rtl/debug.asm
	cp rtl/debug.asm out/selfbld/debug.asm

out/selfbld/%.asm: out/selfbld/%.sasm-opt $(BOOTSTRAP_SASMC)
	$(BOOTSTRAP_SASMC) $< --out=$@

out/selfbld/%.o: out/selfbld/%.asm
	nasm -felf32 $< -o $@

out/selfbld/scheme.sasm : out/bootstrap/scheme.sasm
	cp out/bootstrap/scheme.sasm out/selfbld/scheme.sasm
#	$(BOOTSTRAP_JAVAC) -l --out=$@ rtl/scheme.java

out/selfbld/scheme.sasm-opt: out/selfbld/scheme.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

# RTL rules which have to go under specific rules above
out/selfbld/%.sasm-opt: rtl/%.sasm $(BOOTSTRAP_SASM_OPT)
	$(BOOTSTRAP_SASM_OPT) $< --out=$@

CFLAGS=-g -fno-pie -no-pie -m32
SCHEME_CFLAGS=-DSCHEME_RTL=1 $(CFLAGS)

out/selfbld/sasm.exe: $(DEPEND_RTL) out/sasm-selfbld.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/selfbld $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/selfbld/sasm.o -o $@ -lsasm

out/selfbld/sasm-opt.exe: $(DEPEND_RTL) out/sasm-opt-selfbld.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/selfbld $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/selfbld/sasm-opt.o -o $@ -lsasm-opt

out/selfbld/schemec.exe: $(DEPEND_RTL) out/scheme-compiler-selfbld.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/selfbld $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/selfbld/scheme-compiler.o -o $@ -lscheme-compiler

DEPEND_SCHEME_RTL=\
  $(DEPEND_SCHEME_RTL_OMIT_MAIN) \
  out/bootstrap/scheme-main.o

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
include tests.linux.makefile

out/selfbld/test/selfbld-tests.done: $(ALL_SCHEME_TESTS)
	touch out/selfbld/test/selfbld-tests.done

