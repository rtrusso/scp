include sasm-x64.dep
include sasm-opt-x64.dep
include scheme-compiler.dep
include java-compiler.dep

# Depends on the following environment variables which are expected to be set by running go.sh or env.sh:
#
# TINYSCHEMEINIT=../bootstrap/tinyscheme-1.41/init.scm
# SCHEME=../bootstrap/tinyscheme-1.41/scheme.exe -1
# SASMOPT=$(SCHEME) out/sasm-opt-x64-flat-ts.scm
# SASMC=$(SCHEME) out/sasm-x64-flat-ts.scm
# SCHEMEC=$(SCHEME) $(SCHEMEC_FLAT_TS)
# OUTDIR=out/bootstrap

ifndef TINYSCHEMEINIT
$(error TINYSCHEMEINIT not defined, run go or env)
endif

ifndef SCHEME
$(error SCHEME not defined, run go or env)
endif

ifndef SASMOPT
$(error SASMOPT not defined, run go or env)
endif

ifndef SASMC
$(error SASMC not defined, run go or env)
endif

ifndef SCHEMEC
$(error SCHEMEC not defined, run go or env)
endif

ifndef OUTDIR
$(error OUTDIR not defined, run go or env)
endif

OUT_DIR=out/.exists
BOOTSTRAP_DIR=out/bootstrap-x64/.exists
BOOTSTRAP_TEST_DIR=out/bootstrap-x64/test/.exists
BOOTSTRAP_TEST_JAVA_DIR=out/bootstrap-x64/test/java/.exists
BOOTSTRAP_TEST_JAVA_GC_DIR=out/bootstrap-x64/test/java/gc/.exists

SCHEMEC_FLAT_TS=out/scheme-compiler-flat-ts.scm

JAVAC_FLAT_TS=out/java-compiler-flat-ts.scm
JAVAC=$(SCHEME) $(JAVAC_FLAT_TS)

GLUEC_FLAT_TS=out/scheme-gluec-flat-ts.scm
GLUEC=$(SCHEME) $(GLUEC_FLAT_TS)
CFLAGS=-g -m64 -no-pie -fno-pie
SCHEME_CFLAGS=-DSCHEME_RTL=1 $(CFLAGS)
NASM_FLAGS=-felf64

SCHEME_RTL=\
  out/bootstrap-x64/r5rs-library.sasm \
  out/bootstrap-x64/r5rs-library.sasm-opt-x64 \
  out/bootstrap-x64/r5rs-library.asm \
  out/bootstrap-x64/r5rs-library.o \
  out/bootstrap-x64/r5rs-native.sasm \
  out/bootstrap-x64/r5rs-native.sasm-opt-x64 \
  out/bootstrap-x64/r5rs-native.asm \
  out/bootstrap-x64/r5rs-native.o \
  out/bootstrap-x64/r5rs-wrap.sasm \
  out/bootstrap-x64/r5rs-wrap.sasm-opt-x64 \
  out/bootstrap-x64/r5rs-wrap.asm \
  out/bootstrap-x64/r5rs-wrap.o \
  out/bootstrap-x64/rtlscheme.sasm-opt-x64 \
  out/bootstrap-x64/rtlscheme.asm \
  out/bootstrap-x64/rtlscheme.o

DEPEND_OUT_DIRS=\
  $(OUT_DIR) \
  $(BOOTSTRAP_DIR) \
  $(BOOTSTRAP_TEST_DIR) \
  $(BOOTSTRAP_TEST_JAVA_DIR)

DEPEND_EXPAND_TESTS=\
  out/bootstrap-x64/test/apply-expanded.scm \
  out/bootstrap-x64/test/syntax1-expanded.scm \
  out/bootstrap-x64/test/count.sasm

DEPEND_SCHEMEC=\
  $(SCHEMEC_FLAT_TS)

DEPEND_JAVAC=\
  $(JAVAC_FLAT_TS)

DEPEND_SASMOPT=\
  out/sasm-opt-x64-flat-ts.scm

DEPEND_SASMC=\
  out/sasm-x64-flat-ts.scm

DEPEND_GLUEC=\
  $(GLUEC_FLAT_TS)

DEPEND_SASM_SIMPLE_TEST=\
  out/bootstrap-x64/test/call.sasm-opt-x64 \
  out/bootstrap-x64/test/call.asm \
  out/bootstrap-x64/test/call.o \
  out/bootstrap-x64/test/call.exe \
  out/bootstrap-x64/test/call.out \
  out/bootstrap-x64/test/call.diff

DEPEND_SCHEMEC_SIMPLE_TEST=\
  out/bootstrap-x64/test/apply.sasm \
  out/bootstrap-x64/test/apply.sasm-opt-x64 \
  out/bootstrap-x64/test/apply.asm \
  out/bootstrap-x64/test/apply.o \
  out/bootstrap-x64/test/apply.exe \
  out/bootstrap-x64/test/apply.out \
  out/bootstrap-x64/test/apply.diff \
  out/bootstrap-x64/test/cseconds.out \
  out/bootstrap-x64/test/disptest.out \
  out/bootstrap-x64/test/disptest.diff \
  out/bootstrap-x64/test/disptest2.out \
  out/bootstrap-x64/test/disptest2.diff \
  out/bootstrap-x64/test/argv.out \
  out/bootstrap-x64/test/argv.diff \
  out/bootstrap-x64/test/rename.out \
  out/bootstrap-x64/test/rename.diff \
  out/bootstrap-x64/test/delete.out \
  out/bootstrap-x64/test/delete.diff \
  out/bootstrap-x64/test/stat.out \
  out/bootstrap-x64/test/stat.diff \
  out/bootstrap-x64/test/getenv.out \
  out/bootstrap-x64/test/getenv.diff \
  out/bootstrap-x64/test/outputfile.out \
  out/bootstrap-x64/test/outputfile.diff \
  out/bootstrap-x64/test/inputfile.out \
  out/bootstrap-x64/test/inputfile.diff \
  out/bootstrap-x64/test/read.out \
  out/bootstrap-x64/test/read.diff \
  out/bootstrap-x64/test/read2.out \
  out/bootstrap-x64/test/read2.diff \
  out/bootstrap-x64/test/read3.out \
  out/bootstrap-x64/test/read3.diff \
  out/bootstrap-x64/test/peekchar.out \
  out/bootstrap-x64/test/peekchar.diff \
  out/bootstrap-x64/test/mkvec.out \
  out/bootstrap-x64/test/mkvec.diff \
  out/bootstrap-x64/test/eqv.out \
  out/bootstrap-x64/test/eqv.diff \
  out/bootstrap-x64/test/eq.out \
  out/bootstrap-x64/test/eq.diff \
  out/bootstrap-x64/test/vararg.out \
  out/bootstrap-x64/test/vararg.diff \
  out/bootstrap-x64/test/read4.out \
  out/bootstrap-x64/test/read4.diff \
  out/bootstrap-x64/test/read5.out \
  out/bootstrap-x64/test/read5.diff \
  out/bootstrap-x64/test/sym1.out \
  out/bootstrap-x64/test/sym1.diff \
  out/bootstrap-x64/test/opeq.out \
  out/bootstrap-x64/test/opeq.diff \
  out/bootstrap-x64/test/opminus.out \
  out/bootstrap-x64/test/opminus.diff \
  out/bootstrap-x64/test/opplus.out \
  out/bootstrap-x64/test/opplus.diff \
  out/bootstrap-x64/test/opgt.out \
  out/bootstrap-x64/test/opgt.diff \
  out/bootstrap-x64/test/oplt.out \
  out/bootstrap-x64/test/oplt.diff \
  out/bootstrap-x64/test/opmul.out \
  out/bootstrap-x64/test/opmul.diff \
  out/bootstrap-x64/test/pair.out \
  out/bootstrap-x64/test/pair.diff \
  out/bootstrap-x64/test/mkstring.out \
  out/bootstrap-x64/test/mkstring.diff \
  out/bootstrap-x64/test/str2num.out \
  out/bootstrap-x64/test/str2num.diff \
  out/bootstrap-x64/test/opdiv.out \
  out/bootstrap-x64/test/opdiv.diff \
  out/bootstrap-x64/test/char.out \
  out/bootstrap-x64/test/char.diff \
  out/bootstrap-x64/test/pred.out \
  out/bootstrap-x64/test/pred.diff \
  out/bootstrap-x64/test/write.out \
  out/bootstrap-x64/test/write.diff \
  out/bootstrap-x64/test/equal.out \
  out/bootstrap-x64/test/equal.diff \
  out/bootstrap-x64/test/r5rs1.out \
  out/bootstrap-x64/test/r5rs1.diff \
  out/bootstrap-x64/test/r5rs2.out \
  out/bootstrap-x64/test/r5rs2.diff \
  out/bootstrap-x64/test/r5rs3.out \
  out/bootstrap-x64/test/r5rs3.diff \
  out/bootstrap-x64/test/char-case.out \
  out/bootstrap-x64/test/char-case.diff \
  out/bootstrap-x64/test/string.out \
  out/bootstrap-x64/test/string.diff \
  out/bootstrap-x64/test/string2.out \
  out/bootstrap-x64/test/string2.diff \
  out/bootstrap-x64/test/string3.out \
  out/bootstrap-x64/test/string3.diff \
  out/bootstrap-x64/test/read6.out \
  out/bootstrap-x64/test/read6.diff \
  out/bootstrap-x64/test/str2num2.out \
  out/bootstrap-x64/test/str2num2.diff \
  out/bootstrap-x64/test/num2str.out \
  out/bootstrap-x64/test/num2str.diff \
  out/bootstrap-x64/test/set1.out \
  out/bootstrap-x64/test/set1.diff \
  out/bootstrap-x64/test/read7.out \
  out/bootstrap-x64/test/read7.diff \
  out/bootstrap-x64/test/read8.out \
  out/bootstrap-x64/test/read8.diff \
  out/bootstrap-x64/test/strref.out \
  out/bootstrap-x64/test/strref.diff \
  out/bootstrap-x64/test/stringset.out \
  out/bootstrap-x64/test/stringset.diff \
  out/bootstrap-x64/test/order-eval.out \
  out/bootstrap-x64/test/order-eval.diff \
  out/bootstrap-x64/test/read9.out \
  out/bootstrap-x64/test/read9.diff \
  out/bootstrap-x64/test/read10.out \
  out/bootstrap-x64/test/read10.diff \
  out/bootstrap-x64/test/read11.out \
  out/bootstrap-x64/test/read11.diff \
  \
  out/bootstrap-x64/test/badapply.out \
  out/bootstrap-x64/test/badapply.diff \
  out/bootstrap-x64/test/badvrhi.out \
  out/bootstrap-x64/test/badvrhi.diff \
  out/bootstrap-x64/test/badvrlo.out \
  out/bootstrap-x64/test/badvrlo.diff \
  out/bootstrap-x64/test/badvshi.out \
  out/bootstrap-x64/test/badvshi.diff \
  out/bootstrap-x64/test/badvslo.out \
  out/bootstrap-x64/test/badvslo.diff \
  out/bootstrap-x64/test/badsrhi.out \
  out/bootstrap-x64/test/badsrhi.diff \
  out/bootstrap-x64/test/badsrlo.out \
  out/bootstrap-x64/test/badsrlo.diff \
  out/bootstrap-x64/test/badsshi.out \
  out/bootstrap-x64/test/badsshi.diff \
  out/bootstrap-x64/test/badsslo.out \
  out/bootstrap-x64/test/badsslo.diff \
  out/bootstrap-x64/test/gc1.out

DEPEND_RTL_OBJS=\
 out/bootstrap-x64/c-rtlheap.o \
 out/bootstrap-x64/gc.o \
 out/bootstrap-x64/gc-wrapper.o \
 out/bootstrap-x64/gc-invoke.o \
 out/bootstrap-x64/gc-stack.o \
 out/bootstrap-x64/gc-mark.o \
 out/bootstrap-x64/gc-mark-range.o \
 out/bootstrap-x64/gc-mark-array.o \
 out/bootstrap-x64/gc-mark-class.o \
 out/bootstrap-x64/gc-sweep.o \
 out/bootstrap-x64/heap.o \
 out/bootstrap-x64/heapfixed.o \
 out/bootstrap-x64/heapvar.o \
 out/bootstrap-x64/mjrtl.o \
 out/bootstrap-x64/rtlheap.o \
 out/bootstrap-x64/debug.o

DEPEND_MJ_RTL_OBJS=\
  $(DEPEND_RTL_OBJS)

DEPEND_MJ_RTL_C=\
  rtl/mjrtl.c

DEPEND_MJ_RTL=\
  $(DEPEND_MJ_RTL_C) \
  $(DEPEND_RTL_OBJS)

DEPEND_RTL_C=\
  $(DEPEND_MJ_RTL_C) \
  rtl/rtlscheme.c

DEPEND_RTL=\
  out/bootstrap-x64/c-rtlheap.sasm-opt-x64 \
  out/bootstrap-x64/c-rtlheap.asm \
  out/bootstrap-x64/gc.sasm-opt-x64 \
  out/bootstrap-x64/gc.asm \
  out/bootstrap-x64/heap.sasm-opt-x64 \
  out/bootstrap-x64/heap.asm \
  out/bootstrap-x64/heapfixed.sasm-opt-x64 \
  out/bootstrap-x64/heapfixed.asm \
  out/bootstrap-x64/heapvar.sasm-opt-x64 \
  out/bootstrap-x64/heapvar.asm \
  out/bootstrap-x64/mjrtl.sasm-opt-x64 \
  out/bootstrap-x64/mjrtl.asm \
  out/bootstrap-x64/rtlheap.sasm-opt-x64 \
  out/bootstrap-x64/rtlheap.asm \
  $(DEPEND_RTL_C) \
  $(DEPEND_RTL_OBJS)

DEPEND_SCHEME_RTL_OMIT_MAIN=\
  out/bootstrap-x64/r5rs-library.o \
  out/bootstrap-x64/r5rs-native.o \
  out/bootstrap-x64/r5rs-wrap.o \
  out/bootstrap-x64/rtlscheme.o

DEPEND_SCHEME_RTL=\
  $(DEPEND_SCHEME_RTL_OMIT_MAIN) \
  out/bootstrap-x64/scheme-main.o

DEPEND_NEEDC=\
   needc-ts.scm \
   needc.scm

DEPEND_JAVA_TEST_OUTPUT_FILES=\
  out/bootstrap-x64/test/java/Arrays.out \
  out/bootstrap-x64/test/java/BinarySearch.out \
  out/bootstrap-x64/test/java/BinaryTree.out \
  out/bootstrap-x64/test/java/Bitwise.out \
  out/bootstrap-x64/test/java/BubbleSort.out \
  out/bootstrap-x64/test/java/CharString.out \
  out/bootstrap-x64/test/java/Count.out \
  out/bootstrap-x64/test/java/CtorTest.out \
  out/bootstrap-x64/test/java/Factorial.out \
  out/bootstrap-x64/test/java/LinearSearch.out \
  out/bootstrap-x64/test/java/LinkedList.out \
  out/bootstrap-x64/test/java/Messy.out \
  out/bootstrap-x64/test/java/MyFactorial.out \
  out/bootstrap-x64/test/java/NumberToString.out \
  out/bootstrap-x64/test/java/ObjArray.out \
  out/bootstrap-x64/test/java/OpEquals.out \
  out/bootstrap-x64/test/java/OverrideTest.out \
  out/bootstrap-x64/test/java/QuickSort.out \
  out/bootstrap-x64/test/java/Rectangles.out \
  out/bootstrap-x64/test/java/StaticMembers.out \
  out/bootstrap-x64/test/java/StaticMethods.out \
  out/bootstrap-x64/test/java/SubExp.out \
  out/bootstrap-x64/test/java/TreeVisitor.out \
  out/bootstrap-x64/test/java/TwoArgs.out

DEPEND_JAVA_TEST_EXE_FILES=\
  $(subst %.out,%.exe,$(DEPEND_JAVA_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_OUTPUT_FILES=\
  out/bootstrap-x64/test/java/gc/GCTest1.out \
  out/bootstrap-x64/test/java/gc/GCTest2.out \
  out/bootstrap-x64/test/java/gc/GCTest3.out \
  out/bootstrap-x64/test/java/gc/GCTest4.out \
  out/bootstrap-x64/test/java/gc/GCStress.out \
  out/bootstrap-x64/test/java/gc/GCStress2.out \
  out/bootstrap-x64/test/java/gc/GCStress3.out

DEPEND_JAVA_GC_TEST_EXE_FILES=\
  $(subst %.out,%.exe,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_ASM_FILES=\
  $(subst %.out,%.asm,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_SASM_FILES=\
  $(subst %.out,%.sasm,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_SASM_OPT_FILES=\
  $(subst %.out,%.sasm-opt-x64,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_FILES=\
  $(DEPEND_JAVA_GC_TEST_OUTPUT_FILES) \
  $(DEPEND_JAVA_GC_TEST_EXE_FILES) \
  $(DEPEND_JAVA_GC_TEST_ASM_FILES) \
  $(DEPEND_JAVA_GC_TEST_SASM_FILES) \
  $(DEPEND_JAVA_GC_TEST_SASM_OPT_FILES)

DEPEND_JAVA_TEST_MARKER=\
  out/bootstrap-x64/test/java/tests.done

DEPEND_JAVA_GC_TEST_MARKER=\
  out/bootstrap-x64/test/java/gc/tests.done

DEPEND_SCHEMEC_TEST_MARKER=\
  out/bootstrap-x64/test/schemec-tests.done

DEPEND_ALL=\
  $(DEPEND_OUT_DIRS) \
  $(JAVAC_FLAT_TS) \
  $(SCHEMEC_FLAT_TS) \
  $(DEPEND_EXPAND_TESTS) \
  $(DEPEND_SASMOPT) \
  $(DEPEND_SASMC) \
  $(DEPEND_RTL) \
  $(SCHEME_RTL) \
  $(DEPEND_SASM_SIMPLE_TEST) \
  $(DEPEND_SCHEMEC_SIMPLE_TEST) \
  $(DEPEND_JAVA_TEST_MARKER) \
  $(DEPEND_JAVA_GC_TEST_MARKER) \
  $(DEPEND_JAVA_GC_TEST_FILES) \
  $(DEPEND_SCHEMEC_TEST_MARKER) \
  \
  out/bootstrap-sasm-x64-ts.sh \
  out/sasm-x64-bootstrap.out \
  out/bootstrap-x64/sasm-x64.exe \
  \
  out/bootstrap-sasm-opt-x64-ts.sh \
  out/sasm-opt-bootstrap.out \
  out/bootstrap-x64/sasm-x64-opt.exe \
  \
  out/bootstrap-scheme-compiler-ts.sh \
  out/scheme-compiler-bootstrap.out \
  out/bootstrap-x64/schemec.exe

bootstrap_all: $(DEPEND_ALL)

.SECONDARY:

out/.exists:
	if [ -d out ]; then echo foo>/dev/null; else mkdir out; fi
	touch out/.exists

out/bootstrap-x64/.exists : out/.exists
	if [ -d out/bootstrap-x64 ]; then echo foo>/dev/null; else mkdir out/bootstrap-x64; fi
	touch out/bootstrap-x64/.exists

out/bootstrap-x64/test/.exists : out/bootstrap-x64/.exists
	if [ -d out/bootstrap-x64/test ]; then echo foo>/dev/null; else mkdir out/bootstrap-x64/test; fi
	touch out/bootstrap-x64/test/.exists

out/bootstrap-x64/test/java/.exists : out/bootstrap-x64/test/.exists
	if [ -d out/bootstrap-x64/test/java ]; then echo foo>/dev/null; else mkdir out/bootstrap-x64/test/java; fi
	touch out/bootstrap-x64/test/java/.exists

out/bootstrap-x64/test/java/gc/.exists : out/bootstrap-x64/test/java/.exists
	if [ -d out/bootstrap-x64/test/java/gc ]; then echo foo>/dev/null; else mkdir out/bootstrap-x64/test/java/gc; fi
	touch out/bootstrap-x64/test/java/gc/.exists

out/scheme-compiler-flat-ts.scm : $(OUT_DIR) needc-ts.scm $(deps_of_scheme_compiler)
	$(SCHEME) needc-ts.scm --output out/scheme-compiler-flat-ts.scm scheme-compiler-ts

out/scheme-gluec-flat-ts.scm : $(OUT_DIR) needc-ts.scm scheme-gluec.scm scheme-gluec-ts.scm
	$(SCHEME) needc-ts.scm --output out/scheme-gluec-flat-ts.scm scheme-gluec-ts

out/bootstrap-x64/test/apply-expanded.scm : tests/apply.scm $(BOOTSTRAP_TEST_DIR) $(DEPEND_SCHEMEC)
	$(SCHEMEC) tests/apply.scm --output out/bootstrap-x64/test/apply-expanded.scm --expand-only

out/bootstrap-x64/test/syntax1-expanded.scm : $(SCHEMEC_FLAT_TS) tests/syntax1.scm $(BOOTSTRAP_TEST_DIR) $(DEPEND_SCHEMEC)
	$(SCHEMEC) tests/syntax1.scm --output out/bootstrap-x64/test/syntax1-expanded.scm --expand-only

# Expand, flatten minijava compiler
out/bootstrap-expand-java-compiler-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_java_compiler)
	$(SCHEME) needc-ts.scm --script-mode --expand-only --output out/bootstrap-expand-java-compiler-ts.sh java-compiler-ts

out/java-compiler-expanded.out: out/bootstrap-expand-java-compiler-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-expand-java-compiler-ts.sh
	./out/bootstrap-expand-java-compiler-ts.sh
	touch out/java-compiler-expanded.out

out/java-compiler-flat-ts.scm : out/java-compiler-expanded.out
	$(SCHEME) needc-ts.scm --root out/bootstrap-x64 --flat-names --output out/java-compiler-flat-ts.scm java-compiler-ts

# Expand, flatten sasm-opt-x64-ts tool
out/bootstrap-expand-sasm-opt-x64-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm_opt_x64)
	$(SCHEME) needc-ts.scm --script-mode --expand-only --output out/bootstrap-expand-sasm-opt-x64-ts.sh sasm-opt-x64-ts

out/sasm-opt-x64-ts-expanded.out: out/bootstrap-expand-sasm-opt-x64-ts.sh env.sh  $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-expand-sasm-opt-x64-ts.sh
	./out/bootstrap-expand-sasm-opt-x64-ts.sh
	touch out/sasm-opt-x64-ts-expanded.out

out/sasm-opt-x64-flat-ts.scm : out/sasm-opt-x64-ts-expanded.out
	$(SCHEME) needc-ts.scm --root out/bootstrap-x64 --flat-names --output out/sasm-opt-x64-flat-ts.scm sasm-opt-x64-ts

# Expand, flatten sasm-x64-ts tool
out/bootstrap-expand-sasm-x64-ts.sh : sasm.scm $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm_x64)
	$(SCHEME) needc-ts.scm --script-mode --expand-only --output out/bootstrap-expand-sasm-x64-ts.sh sasm-x64-ts

out/sasm-x64-ts-expanded.out: out/bootstrap-expand-sasm-x64-ts.sh env.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-expand-sasm-x64-ts.sh
	./out/bootstrap-expand-sasm-x64-ts.sh
	touch out/sasm-x64-ts-expanded.out

out/sasm-x64-flat-ts.scm: out/sasm-x64-ts-expanded.out
	$(SCHEME) needc-ts.scm --root out/bootstrap-x64 --flat-names --output out/sasm-x64-flat-ts.scm sasm-x64-ts

# test minijava compiler
out/bootstrap-x64/test/java/%.sasm : tests/%.java $(BOOTSTRAP_TEST_JAVA_DIR) $(DEPEND_JAVAC)
	$(JAVAC) --main=$(patsubst tests/%.java,%,$<) --out=$@ rtl/JavaRtl.java $<

out/bootstrap-x64/test/java/%.sasm-opt-x64: out/bootstrap-x64/test/java/%.sasm $(DEPEND_SASMOPT) $(BOOTSTRAP_TEST_JAVA_DIR)
	$(SASMOPT) $< --out=$@

out/bootstrap-x64/test/java/%.asm: out/bootstrap-x64/test/java/%.sasm-opt-x64 $(DEPEND_SASMC) $(BOOTSTRAP_TEST_JAVA_DIR)
	$(SASMC) $< --out=$@

out/bootstrap-x64/test/java/%.o: out/bootstrap-x64/test/java/%.asm
	nasm $(NASM_FLAGS) $< -o $@

out/bootstrap-x64/test/java/%.exe: out/bootstrap-x64/test/java/%.o rtl/mjrtl.c $(DEPEND_MJ_RTL)
	gcc $(CFLAGS) -Irtl rtl/mjrtl.c $(DEPEND_MJ_RTL_OBJS) $< -o $@

out/bootstrap-x64/test/java/%.out: out/bootstrap-x64/test/java/%.exe
	$< >$@.tmp
	mv $@.tmp $@

$(DEPEND_JAVA_TEST_MARKER): $(BOOTSTRAP_TEST_JAVA_DIR) $(DEPEND_JAVA_TEST_OUTPUT_FILES) $(DEPEND_JAVA_TEST_EXE_FILES)
	touch out/bootstrap-x64/test/java/tests.done

# java GC "stress" tests
out/bootstrap-x64/test/java/gc/%.sasm : tests/gc/%.java $(BOOTSTRAP_TEST_JAVA_GC_DIR) $(DEPEND_JAVAC)
	$(JAVAC) --main=$(patsubst tests/gc/%.java,%,$<) --out=$@ rtl/JavaRtl.java $<

out/bootstrap-x64/test/java/gc/%.sasm-opt-x64: out/bootstrap-x64/test/java/gc/%.sasm $(DEPEND_SASMOPT) $(BOOTSTRAP_TEST_JAVA_GC_DIR)
	$(SASMOPT) $< --out=$@

out/bootstrap-x64/test/java/gc/%.asm: out/bootstrap-x64/test/java/gc/%.sasm-opt-x64 $(DEPEND_SASMC) $(BOOTSTRAP_TEST_JAVA_GC_DIR)
	$(SASMC) $< --out=$@

out/bootstrap-x64/test/java/gc/%.o: out/bootstrap-x64/test/java/gc/%.asm
	nasm $(NASM_FLAGS) $< -o $@

out/bootstrap-x64/test/java/gc/%.exe: out/bootstrap-x64/test/java/gc/%.o rtl/mjrtl.c $(DEPEND_MJ_RTL)
	gcc $(CFLAGS) -Irtl rtl/mjrtl.c $(DEPEND_MJ_RTL_OBJS) $< -o $@

out/bootstrap-x64/test/java/gc/%.out: out/bootstrap-x64/test/java/gc/%.exe
	$< >$@.tmp
	mv $@.tmp $@

$(DEPEND_JAVA_GC_TEST_MARKER): $(BOOTSTRAP_TEST_JAVA_GC_DIR) $(DEPEND_JAVA_GC_TEST_FILES)
	touch out/bootstrap-x64/test/java/gc/tests.done

$(DEPEND_SCHEMEC_TEST_MARKER): $(BOOTSTRAP_TEST_DIR) $(DEPEND_SCHEMEC_SIMPLE_TEST)
	touch out/bootstrap-x64/test/schemec-tests.done

# Test sasmopt, sasmc - compile and run call.sasm
out/bootstrap-x64/test/call.sasm-opt-x64: tests/call.sasm $(DEPEND_SASMOPT) $(BOOTSTRAP_TEST_DIR)
	$(SASMOPT) tests/call.sasm --out=out/bootstrap-x64/test/call.sasm-opt-x64

out/bootstrap-x64/test/call.asm: out/bootstrap-x64/test/call.sasm-opt-x64 $(DEPEND_SASMC) $(BOOTSTRAP_TEST_DIR)
	$(SASMC) out/bootstrap-x64/test/call.sasm-opt-x64 --out=out/bootstrap-x64/test/call.asm

out/bootstrap-x64/test/call.o: out/bootstrap-x64/test/call.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/call.asm -o out/bootstrap-x64/test/call.o

out/bootstrap-x64/test/call.exe: out/bootstrap-x64/test/call.o rtl/mjrtl.c $(DEPEND_RTL_OBJS)
	gcc $(CFLAGS) -Irtl rtl/mjrtl.c $(DEPEND_RTL_OBJS) $< -o $@

out/bootstrap-x64/test/call.out: out/bootstrap-x64/test/call.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/call.diff: out/bootstrap-x64/test/call.out tests/baseline/call.sasm-interp.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/call.out tests/baseline/call.sasm-interp.actual
	touch out/bootstrap-x64/test/call.diff

# Test scheme-compiler: apply
out/bootstrap-x64/test/apply.sasm: tests/apply.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/apply.scm --output out/bootstrap-x64/test/apply.sasm

out/bootstrap-x64/test/apply.sasm-opt-x64: out/bootstrap-x64/test/apply.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/apply.sasm --out=out/bootstrap-x64/test/apply.sasm-opt-x64

out/bootstrap-x64/test/apply.asm: out/bootstrap-x64/test/apply.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/apply.sasm-opt-x64 --out=out/bootstrap-x64/test/apply.asm

out/bootstrap-x64/test/apply.o: out/bootstrap-x64/test/apply.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/apply.asm -o out/bootstrap-x64/test/apply.o

out/bootstrap-x64/test/apply.exe: out/bootstrap-x64/test/apply.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/apply.out: out/bootstrap-x64/test/apply.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/apply.diff: out/bootstrap-x64/test/apply.out tests/baseline/apply-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/apply.out tests/baseline/apply-s.actual
	touch out/bootstrap-x64/test/apply.diff

# Test scheme-compiler: getenv
out/bootstrap-x64/test/getenv.sasm: tests/getenv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/getenv.scm --output out/bootstrap-x64/test/getenv.sasm

out/bootstrap-x64/test/getenv.sasm-opt-x64: out/bootstrap-x64/test/getenv.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/getenv.sasm --out=out/bootstrap-x64/test/getenv.sasm-opt-x64

out/bootstrap-x64/test/getenv.asm: out/bootstrap-x64/test/getenv.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/getenv.sasm-opt-x64 --out=out/bootstrap-x64/test/getenv.asm

out/bootstrap-x64/test/getenv.o: out/bootstrap-x64/test/getenv.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/getenv.asm -o out/bootstrap-x64/test/getenv.o

out/bootstrap-x64/test/getenv.exe: out/bootstrap-x64/test/getenv.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/getenv.out: out/bootstrap-x64/test/getenv.exe
	export SCHEME_TEST=SCHEME_TEST;$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/getenv.diff: out/bootstrap-x64/test/getenv.out tests/baseline/getenv-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/getenv.out tests/baseline/getenv-s.actual
	touch out/bootstrap-x64/test/getenv.diff

# Test scheme-compiler: cseconds
out/bootstrap-x64/test/cseconds.sasm: tests/cseconds.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/cseconds.scm --output out/bootstrap-x64/test/cseconds.sasm

out/bootstrap-x64/test/cseconds.sasm-opt-x64: out/bootstrap-x64/test/cseconds.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/cseconds.sasm --out=out/bootstrap-x64/test/cseconds.sasm-opt-x64 --cheap

out/bootstrap-x64/test/cseconds.asm: out/bootstrap-x64/test/cseconds.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/cseconds.sasm-opt-x64 --out=out/bootstrap-x64/test/cseconds.asm

out/bootstrap-x64/test/cseconds.o: out/bootstrap-x64/test/cseconds.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/cseconds.asm -o out/bootstrap-x64/test/cseconds.o

out/bootstrap-x64/test/cseconds.exe: out/bootstrap-x64/test/cseconds.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/cseconds.out: out/bootstrap-x64/test/cseconds.exe
	$< >$@.tmp
	mv $@.tmp $@

# Test scheme-compiler: stat
out/bootstrap-x64/test/stat.sasm: tests/stat.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/stat.scm --output out/bootstrap-x64/test/stat.sasm

out/bootstrap-x64/test/stat.sasm-opt-x64: out/bootstrap-x64/test/stat.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/stat.sasm --out=out/bootstrap-x64/test/stat.sasm-opt-x64 --cheap

out/bootstrap-x64/test/stat.asm: out/bootstrap-x64/test/stat.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/stat.sasm-opt-x64 --out=out/bootstrap-x64/test/stat.asm

out/bootstrap-x64/test/stat.o: out/bootstrap-x64/test/stat.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/stat.asm -o out/bootstrap-x64/test/stat.o

out/bootstrap-x64/test/stat.exe: out/bootstrap-x64/test/stat.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/stat.out: out/bootstrap-x64/test/stat.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/stat.diff: out/bootstrap-x64/test/stat.out tests/baseline/stat-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/stat.out tests/baseline/stat-s.actual
	touch out/bootstrap-x64/test/stat.diff

# Test scheme-compiler: delete
out/bootstrap-x64/test/delete.sasm: tests/delete.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/delete.scm --output out/bootstrap-x64/test/delete.sasm

out/bootstrap-x64/test/delete.sasm-opt-x64: out/bootstrap-x64/test/delete.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/delete.sasm --out=out/bootstrap-x64/test/delete.sasm-opt-x64 --cheap

out/bootstrap-x64/test/delete.asm: out/bootstrap-x64/test/delete.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/delete.sasm-opt-x64 --out=out/bootstrap-x64/test/delete.asm

out/bootstrap-x64/test/delete.o: out/bootstrap-x64/test/delete.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/delete.asm -o out/bootstrap-x64/test/delete.o

out/bootstrap-x64/test/delete.exe: out/bootstrap-x64/test/delete.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/delete.out: out/bootstrap-x64/test/delete.exe
	touch deleteme.file
	$< >$@.tmp
	if [ -f deleteme.file ]; then exit 128; fi
	mv $@.tmp $@

out/bootstrap-x64/test/delete.diff: out/bootstrap-x64/test/delete.out tests/baseline/delete-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/delete.out tests/baseline/delete-s.actual
	touch out/bootstrap-x64/test/delete.diff

# Test scheme-compiler: rename
out/bootstrap-x64/test/rename.sasm: tests/rename.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/rename.scm --output out/bootstrap-x64/test/rename.sasm

out/bootstrap-x64/test/rename.sasm-opt-x64: out/bootstrap-x64/test/rename.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/rename.sasm --out=out/bootstrap-x64/test/rename.sasm-opt-x64 --cheap

out/bootstrap-x64/test/rename.asm: out/bootstrap-x64/test/rename.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/rename.sasm-opt-x64 --out=out/bootstrap-x64/test/rename.asm

out/bootstrap-x64/test/rename.o: out/bootstrap-x64/test/rename.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/rename.asm -o out/bootstrap-x64/test/rename.o

out/bootstrap-x64/test/rename.exe: out/bootstrap-x64/test/rename.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/rename.out: out/bootstrap-x64/test/rename.exe
	touch from.file
	$< >$@.tmp
	if [ -f from.file ]; then exit 128; fi
	if [ ! -f to.file ]; then exit 128; fi
	rm to.file
	mv $@.tmp $@

out/bootstrap-x64/test/rename.diff: out/bootstrap-x64/test/rename.out tests/baseline/rename-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/rename.out tests/baseline/rename-s.actual
	touch out/bootstrap-x64/test/rename.diff

# Test scheme-compiler: argv
out/bootstrap-x64/test/argv.sasm: tests/argv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/argv.scm --output out/bootstrap-x64/test/argv.sasm

out/bootstrap-x64/test/argv.sasm-opt-x64: out/bootstrap-x64/test/argv.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/argv.sasm --out=out/bootstrap-x64/test/argv.sasm-opt-x64 --cheap

out/bootstrap-x64/test/argv.asm: out/bootstrap-x64/test/argv.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/argv.sasm-opt-x64 --out=out/bootstrap-x64/test/argv.asm

out/bootstrap-x64/test/argv.o: out/bootstrap-x64/test/argv.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/argv.asm -o out/bootstrap-x64/test/argv.o

out/bootstrap-x64/test/argv.exe: out/bootstrap-x64/test/argv.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/argv.out: out/bootstrap-x64/test/argv.exe
	$< 1 2 3 4 five six 7 8 9 ten>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/argv.diff: out/bootstrap-x64/test/argv.out tests/baseline/argv-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/argv.out tests/baseline/argv-s.actual
	touch out/bootstrap-x64/test/argv.diff

# Test scheme-compiler: disptest
out/bootstrap-x64/test/disptest.sasm: tests/disptest.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/disptest.scm --output out/bootstrap-x64/test/disptest.sasm --debug

out/bootstrap-x64/test/disptest.sasm-opt-x64: out/bootstrap-x64/test/disptest.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/disptest.sasm --out=out/bootstrap-x64/test/disptest.sasm-opt-x64 --cheap

out/bootstrap-x64/test/disptest.asm: out/bootstrap-x64/test/disptest.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/disptest.sasm-opt-x64 --out=out/bootstrap-x64/test/disptest.asm

out/bootstrap-x64/test/disptest.o: out/bootstrap-x64/test/disptest.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/disptest.asm -o out/bootstrap-x64/test/disptest.o

out/bootstrap-x64/test/disptest.exe: out/bootstrap-x64/test/disptest.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/disptest.out: out/bootstrap-x64/test/disptest.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/disptest.diff: out/bootstrap-x64/test/disptest.out tests/baseline/disptest-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/disptest.out tests/baseline/disptest-s.actual
	touch out/bootstrap-x64/test/disptest.diff

# Test scheme-compiler: disptest2
out/bootstrap-x64/test/disptest2.sasm: tests/disptest2.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/disptest2.scm --output out/bootstrap-x64/test/disptest2.sasm

out/bootstrap-x64/test/disptest2.sasm-opt-x64: out/bootstrap-x64/test/disptest2.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/disptest2.sasm --out=out/bootstrap-x64/test/disptest2.sasm-opt-x64 --cheap

out/bootstrap-x64/test/disptest2.asm: out/bootstrap-x64/test/disptest2.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/disptest2.sasm-opt-x64 --out=out/bootstrap-x64/test/disptest2.asm

out/bootstrap-x64/test/disptest2.o: out/bootstrap-x64/test/disptest2.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/disptest2.asm -o out/bootstrap-x64/test/disptest2.o

out/bootstrap-x64/test/disptest2.exe: out/bootstrap-x64/test/disptest2.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/disptest2.out: out/bootstrap-x64/test/disptest2.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/disptest2.diff: out/bootstrap-x64/test/disptest2.out tests/baseline/disptest2-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/disptest2.out tests/baseline/disptest2-s.actual
	touch out/bootstrap-x64/test/disptest2.diff

# Test scheme-compiler: outputfile
out/bootstrap-x64/test/outputfile.sasm: tests/outputfile.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/outputfile.scm --output out/bootstrap-x64/test/outputfile.sasm

out/bootstrap-x64/test/outputfile.sasm-opt-x64: out/bootstrap-x64/test/outputfile.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/outputfile.sasm --out=out/bootstrap-x64/test/outputfile.sasm-opt-x64 --cheap

out/bootstrap-x64/test/outputfile.asm: out/bootstrap-x64/test/outputfile.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/outputfile.sasm-opt-x64 --out=out/bootstrap-x64/test/outputfile.asm

out/bootstrap-x64/test/outputfile.o: out/bootstrap-x64/test/outputfile.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/outputfile.asm -o out/bootstrap-x64/test/outputfile.o

out/bootstrap-x64/test/outputfile.exe: out/bootstrap-x64/test/outputfile.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/outputfile.out: out/bootstrap-x64/test/outputfile.exe
	$< out/bootstrap-x64/test/outputfile.dat>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/outputfile.diff: out/bootstrap-x64/test/outputfile.out tests/baseline/outputfile-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/outputfile.out tests/baseline/outputfile-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/outputfile.dat tests/baseline/outputfile.dat
	touch out/bootstrap-x64/test/outputfile.diff

# Test scheme-compiler: inputfile
out/bootstrap-x64/test/inputfile.sasm: tests/inputfile.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/inputfile.scm --output out/bootstrap-x64/test/inputfile.sasm

out/bootstrap-x64/test/inputfile.sasm-opt-x64: out/bootstrap-x64/test/inputfile.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/inputfile.sasm --out=out/bootstrap-x64/test/inputfile.sasm-opt-x64 --cheap

out/bootstrap-x64/test/inputfile.asm: out/bootstrap-x64/test/inputfile.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/inputfile.sasm-opt-x64 --out=out/bootstrap-x64/test/inputfile.asm

out/bootstrap-x64/test/inputfile.o: out/bootstrap-x64/test/inputfile.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/inputfile.asm -o out/bootstrap-x64/test/inputfile.o

out/bootstrap-x64/test/inputfile.exe: out/bootstrap-x64/test/inputfile.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/inputfile.out: out/bootstrap-x64/test/inputfile.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/inputfile.diff: out/bootstrap-x64/test/inputfile.out tests/baseline/inputfile-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/inputfile.out tests/baseline/inputfile-s.actual
	touch out/bootstrap-x64/test/inputfile.diff

# Test scheme-compiler: read
out/bootstrap-x64/test/read.sasm: tests/read.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read.scm --output out/bootstrap-x64/test/read.sasm

out/bootstrap-x64/test/read-expanded.scm: tests/read.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read.scm --output out/bootstrap-x64/test/read-expanded.scm --expand-only

out/bootstrap-x64/test/read.sasm-opt-x64: out/bootstrap-x64/test/read.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/read.sasm --out=out/bootstrap-x64/test/read.sasm-opt-x64 --cheap

out/bootstrap-x64/test/read.asm: out/bootstrap-x64/test/read.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/read.sasm-opt-x64 --out=out/bootstrap-x64/test/read.asm

out/bootstrap-x64/test/read.o: out/bootstrap-x64/test/read.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/read.asm -o out/bootstrap-x64/test/read.o

out/bootstrap-x64/test/read.exe: out/bootstrap-x64/test/read.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/read.out: out/bootstrap-x64/test/read.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/read.diff: out/bootstrap-x64/test/read.out tests/baseline/read-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/read.out tests/baseline/read-s.actual
	touch out/bootstrap-x64/test/read.diff

# Test scheme-compiler: read2
out/bootstrap-x64/test/read2.sasm: tests/read2.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read2.scm --output out/bootstrap-x64/test/read2.sasm

out/bootstrap-x64/test/read2-expanded.scm: tests/read2.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read2.scm --output out/bootstrap-x64/test/read2-expanded.scm --expand-only

out/bootstrap-x64/test/read2.sasm-opt-x64: out/bootstrap-x64/test/read2.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/read2.sasm --out=out/bootstrap-x64/test/read2.sasm-opt-x64 --cheap

out/bootstrap-x64/test/read2.asm: out/bootstrap-x64/test/read2.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/read2.sasm-opt-x64 --out=out/bootstrap-x64/test/read2.asm

out/bootstrap-x64/test/read2.o: out/bootstrap-x64/test/read2.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/read2.asm -o out/bootstrap-x64/test/read2.o

out/bootstrap-x64/test/read2.exe: out/bootstrap-x64/test/read2.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/read2.out: out/bootstrap-x64/test/read2.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/read2.diff: out/bootstrap-x64/test/read2.out tests/baseline/read2-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/read2.out tests/baseline/read2-s.actual
	touch out/bootstrap-x64/test/read2.diff

# Test scheme-compiler: read3
out/bootstrap-x64/test/read3.sasm: tests/read3.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read3.scm --output out/bootstrap-x64/test/read3.sasm

out/bootstrap-x64/test/read3-expanded.scm: tests/read3.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read3.scm --output out/bootstrap-x64/test/read3-expanded.scm --expand-only

out/bootstrap-x64/test/read3.sasm-opt-x64: out/bootstrap-x64/test/read3.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/read3.sasm --out=out/bootstrap-x64/test/read3.sasm-opt-x64 --cheap

out/bootstrap-x64/test/read3.asm: out/bootstrap-x64/test/read3.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/read3.sasm-opt-x64 --out=out/bootstrap-x64/test/read3.asm

out/bootstrap-x64/test/read3.o: out/bootstrap-x64/test/read3.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/read3.asm -o out/bootstrap-x64/test/read3.o

out/bootstrap-x64/test/read3.exe: out/bootstrap-x64/test/read3.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/read3.out: out/bootstrap-x64/test/read3.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/read3.diff: out/bootstrap-x64/test/read3.out tests/baseline/read3-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/read3.out tests/baseline/read3-s.actual
	touch out/bootstrap-x64/test/read3.diff


# Test scheme-compiler: peekchar
out/bootstrap-x64/test/peekchar.sasm: tests/peekchar.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/peekchar.scm --output out/bootstrap-x64/test/peekchar.sasm

out/bootstrap-x64/test/peekchar-expanded.scm: tests/peekchar.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/peekchar.scm --output out/bootstrap-x64/test/peekchar-expanded.scm --expand-only

out/bootstrap-x64/test/peekchar.sasm-opt-x64: out/bootstrap-x64/test/peekchar.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/peekchar.sasm --out=out/bootstrap-x64/test/peekchar.sasm-opt-x64 --cheap

out/bootstrap-x64/test/peekchar.asm: out/bootstrap-x64/test/peekchar.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/peekchar.sasm-opt-x64 --out=out/bootstrap-x64/test/peekchar.asm

out/bootstrap-x64/test/peekchar.o: out/bootstrap-x64/test/peekchar.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/peekchar.asm -o out/bootstrap-x64/test/peekchar.o

out/bootstrap-x64/test/peekchar.exe: out/bootstrap-x64/test/peekchar.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/peekchar.out: out/bootstrap-x64/test/peekchar.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/peekchar.diff: out/bootstrap-x64/test/peekchar.out tests/baseline/peekchar-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/peekchar.out tests/baseline/peekchar-s.actual
	touch out/bootstrap-x64/test/peekchar.diff

# Test scheme-compiler: mkvec
out/bootstrap-x64/test/mkvec.sasm: tests/mkvec.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/mkvec.scm --output out/bootstrap-x64/test/mkvec.sasm

out/bootstrap-x64/test/mkvec.sasm-opt-x64: out/bootstrap-x64/test/mkvec.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/mkvec.sasm --out=out/bootstrap-x64/test/mkvec.sasm-opt-x64 --cheap

out/bootstrap-x64/test/mkvec.asm: out/bootstrap-x64/test/mkvec.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/mkvec.sasm-opt-x64 --out=out/bootstrap-x64/test/mkvec.asm

out/bootstrap-x64/test/mkvec.o: out/bootstrap-x64/test/mkvec.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/mkvec.asm -o out/bootstrap-x64/test/mkvec.o

out/bootstrap-x64/test/mkvec.exe: out/bootstrap-x64/test/mkvec.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/mkvec.out: out/bootstrap-x64/test/mkvec.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/mkvec.diff: out/bootstrap-x64/test/mkvec.out tests/baseline/mkvec-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/mkvec.out tests/baseline/mkvec-s.actual
	touch out/bootstrap-x64/test/mkvec.diff

# Test scheme-compiler: eqv
out/bootstrap-x64/test/eqv.sasm: tests/eqv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eqv.scm --output out/bootstrap-x64/test/eqv.sasm

out/bootstrap-x64/test/eqv-expanded.scm: tests/eqv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eqv.scm --output out/bootstrap-x64/test/eqv-expanded.scm --expand-only

out/bootstrap-x64/test/eqv.sasm-opt-x64: out/bootstrap-x64/test/eqv.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/eqv.sasm --out=out/bootstrap-x64/test/eqv.sasm-opt-x64 --cheap

out/bootstrap-x64/test/eqv.asm: out/bootstrap-x64/test/eqv.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/eqv.sasm-opt-x64 --out=out/bootstrap-x64/test/eqv.asm

out/bootstrap-x64/test/eqv.o: out/bootstrap-x64/test/eqv.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/eqv.asm -o out/bootstrap-x64/test/eqv.o

out/bootstrap-x64/test/eqv.exe: out/bootstrap-x64/test/eqv.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/eqv.out: out/bootstrap-x64/test/eqv.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/eqv.diff: out/bootstrap-x64/test/eqv.out tests/baseline/eqv-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/eqv.out tests/baseline/eqv-s.actual
	touch out/bootstrap-x64/test/eqv.diff

# Test scheme-compiler: eq
out/bootstrap-x64/test/eq.sasm: tests/eq.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eq.scm --output out/bootstrap-x64/test/eq.sasm

out/bootstrap-x64/test/eq-expanded.scm: tests/eq.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eq.scm --output out/bootstrap-x64/test/eq-expanded.scm --expand-only

out/bootstrap-x64/test/eq.sasm-opt-x64: out/bootstrap-x64/test/eq.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/eq.sasm --out=out/bootstrap-x64/test/eq.sasm-opt-x64 --cheap

out/bootstrap-x64/test/eq.asm: out/bootstrap-x64/test/eq.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/eq.sasm-opt-x64 --out=out/bootstrap-x64/test/eq.asm

out/bootstrap-x64/test/eq.o: out/bootstrap-x64/test/eq.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/eq.asm -o out/bootstrap-x64/test/eq.o

out/bootstrap-x64/test/eq.exe: out/bootstrap-x64/test/eq.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/eq.out: out/bootstrap-x64/test/eq.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/eq.diff: out/bootstrap-x64/test/eq.out tests/baseline/eq-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/eq.out tests/baseline/eq-s.actual
	touch out/bootstrap-x64/test/eq.diff

# Test scheme-compiler: vararg
out/bootstrap-x64/test/vararg.sasm: tests/vararg.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/vararg.scm --output out/bootstrap-x64/test/vararg.sasm

out/bootstrap-x64/test/vararg-expanded.scm: tests/vararg.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/vararg.scm --output out/bootstrap-x64/test/vararg-expanded.scm --expand-only

out/bootstrap-x64/test/vararg.sasm-opt-x64: out/bootstrap-x64/test/vararg.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/vararg.sasm --out=out/bootstrap-x64/test/vararg.sasm-opt-x64 --cheap

out/bootstrap-x64/test/vararg.asm: out/bootstrap-x64/test/vararg.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/vararg.sasm-opt-x64 --out=out/bootstrap-x64/test/vararg.asm

out/bootstrap-x64/test/vararg.o: out/bootstrap-x64/test/vararg.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/vararg.asm -o out/bootstrap-x64/test/vararg.o

out/bootstrap-x64/test/vararg.exe: out/bootstrap-x64/test/vararg.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/vararg.out: out/bootstrap-x64/test/vararg.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/vararg.diff: out/bootstrap-x64/test/vararg.out tests/baseline/vararg-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/vararg.out tests/baseline/vararg-s.actual
	touch out/bootstrap-x64/test/vararg.diff

# Test scheme-compiler: letrec
out/bootstrap-x64/test/letrec.sasm: tests/letrec.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/letrec.scm --output out/bootstrap-x64/test/letrec.sasm

out/bootstrap-x64/test/letrec-expanded.scm: tests/letrec.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/letrec.scm --output out/bootstrap-x64/test/letrec-expanded.scm --expand-only

out/bootstrap-x64/test/letrec.sasm-opt-x64: out/bootstrap-x64/test/letrec.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/letrec.sasm --out=out/bootstrap-x64/test/letrec.sasm-opt-x64 --cheap

out/bootstrap-x64/test/letrec.asm: out/bootstrap-x64/test/letrec.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/letrec.sasm-opt-x64 --out=out/bootstrap-x64/test/letrec.asm

out/bootstrap-x64/test/letrec.o: out/bootstrap-x64/test/letrec.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/letrec.asm -o out/bootstrap-x64/test/letrec.o

out/bootstrap-x64/test/letrec.exe: out/bootstrap-x64/test/letrec.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/letrec.out: out/bootstrap-x64/test/letrec.exe
	$< >$@.tmp
	mv $@.tmp $@

# Test scheme-compiler: read4
out/bootstrap-x64/test/read4.sasm: tests/read4.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read4.scm --output out/bootstrap-x64/test/read4.sasm

out/bootstrap-x64/test/read4-expanded.scm: tests/read4.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read4.scm --output out/bootstrap-x64/test/read4-expanded.scm --expand-only

out/bootstrap-x64/test/read4.sasm-opt-x64: out/bootstrap-x64/test/read4.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/read4.sasm --out=out/bootstrap-x64/test/read4.sasm-opt-x64 --cheap

out/bootstrap-x64/test/read4.asm: out/bootstrap-x64/test/read4.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/read4.sasm-opt-x64 --out=out/bootstrap-x64/test/read4.asm

out/bootstrap-x64/test/read4.o: out/bootstrap-x64/test/read4.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/read4.asm -o out/bootstrap-x64/test/read4.o

out/bootstrap-x64/test/read4.exe: out/bootstrap-x64/test/read4.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/read4.out: out/bootstrap-x64/test/read4.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/read4.diff: out/bootstrap-x64/test/read4.out tests/baseline/read4-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/read4.out tests/baseline/read4-s.actual
	touch out/bootstrap-x64/test/read4.diff

# Test scheme-compiler: read5
out/bootstrap-x64/test/read5.sasm: tests/read5.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read5.scm --output out/bootstrap-x64/test/read5.sasm

out/bootstrap-x64/test/read5-expanded.scm: tests/read5.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read5.scm --output out/bootstrap-x64/test/read5-expanded.scm --expand-only

out/bootstrap-x64/test/read5.sasm-opt-x64: out/bootstrap-x64/test/read5.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/read5.sasm --out=out/bootstrap-x64/test/read5.sasm-opt-x64 --cheap

out/bootstrap-x64/test/read5.asm: out/bootstrap-x64/test/read5.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/read5.sasm-opt-x64 --out=out/bootstrap-x64/test/read5.asm

out/bootstrap-x64/test/read5.o: out/bootstrap-x64/test/read5.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/read5.asm -o out/bootstrap-x64/test/read5.o

out/bootstrap-x64/test/read5.exe: out/bootstrap-x64/test/read5.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/read5.out: out/bootstrap-x64/test/read5.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/read5.diff: out/bootstrap-x64/test/read5.out tests/baseline/read5-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/read5.out tests/baseline/read5-s.actual
	touch out/bootstrap-x64/test/read5.diff

# Test scheme-compiler: sym1
out/bootstrap-x64/test/sym1.sasm: tests/sym1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/sym1.scm --output out/bootstrap-x64/test/sym1.sasm

out/bootstrap-x64/test/sym1-expanded.scm: tests/sym1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/sym1.scm --output out/bootstrap-x64/test/sym1-expanded.scm --expand-only

out/bootstrap-x64/test/sym1.sasm-opt-x64: out/bootstrap-x64/test/sym1.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/sym1.sasm --out=out/bootstrap-x64/test/sym1.sasm-opt-x64 --cheap

out/bootstrap-x64/test/sym1.asm: out/bootstrap-x64/test/sym1.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/sym1.sasm-opt-x64 --out=out/bootstrap-x64/test/sym1.asm

out/bootstrap-x64/test/sym1.o: out/bootstrap-x64/test/sym1.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/sym1.asm -o out/bootstrap-x64/test/sym1.o

out/bootstrap-x64/test/sym1.exe: out/bootstrap-x64/test/sym1.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/sym1.out: out/bootstrap-x64/test/sym1.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/sym1.diff: out/bootstrap-x64/test/sym1.out tests/baseline/sym1-s.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/sym1.out tests/baseline/sym1-s.actual
	touch out/bootstrap-x64/test/sym1.diff

# Test scheme-compiler: badapply
out/bootstrap-x64/test/badapply.sasm: tests/badapply.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/badapply.scm --output out/bootstrap-x64/test/badapply.sasm

out/bootstrap-x64/test/badapply-expanded.scm: tests/badapply.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/badapply.scm --output out/bootstrap-x64/test/badapply-expanded.scm --expand-only

out/bootstrap-x64/test/badapply.sasm-opt-x64: out/bootstrap-x64/test/badapply.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/badapply.sasm --out=out/bootstrap-x64/test/badapply.sasm-opt-x64 --cheap

out/bootstrap-x64/test/badapply.asm: out/bootstrap-x64/test/badapply.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/badapply.sasm-opt-x64 --out=out/bootstrap-x64/test/badapply.asm

out/bootstrap-x64/test/badapply.o: out/bootstrap-x64/test/badapply.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/badapply.asm -o out/bootstrap-x64/test/badapply.o

out/bootstrap-x64/test/badapply.exe: out/bootstrap-x64/test/badapply.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/badapply.out: out/bootstrap-x64/test/badapply.exe tests/badapply.sh
	./tests/badapply.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badapply.diff: out/bootstrap-x64/test/badapply.out tests/baseline/badapply-s-linux.actual
	diff --strip-trailing-cr out/bootstrap-x64/test/badapply.out tests/baseline/badapply-s-linux.actual
	touch out/bootstrap-x64/test/badapply.diff

out/bootstrap-x64/test/badvrhi.out: out/bootstrap-x64/test/badvrhi.exe tests/badvrhi.sh
	./tests/badvrhi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badvrlo.out: out/bootstrap-x64/test/badvrlo.exe tests/badvrlo.sh
	./tests/badvrlo.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badvshi.out: out/bootstrap-x64/test/badvshi.exe tests/badvshi.sh
	./tests/badvshi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badvslo.out: out/bootstrap-x64/test/badvslo.exe tests/badvslo.sh
	./tests/badvslo.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badsrhi.out: out/bootstrap-x64/test/badsrhi.exe tests/badsrhi.sh
	./tests/badsrhi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badsrlo.out: out/bootstrap-x64/test/badsrlo.exe tests/badsrlo.sh
	./tests/badsrlo.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badsshi.out: out/bootstrap-x64/test/badsshi.exe tests/badsshi.sh
	./tests/badsshi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/badsslo.out: out/bootstrap-x64/test/badsslo.exe tests/badsslo.sh
	./tests/badsslo.sh>$@.tmp
	mv $@.tmp $@


# Test scheme-compiler: gc1
out/bootstrap-x64/test/tests-printer.sasm: tests/printer.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/printer.scm --outdir out/bootstrap-x64/test --conspiracy --no-entry

out/bootstrap-x64/test/tests-printer-helper.sasm: tests/printer-helper.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR) out/bootstrap-x64/test/tests-printer.sasm
	$(SCHEMEC) tests/printer-helper.scm --outdir out/bootstrap-x64/test --conspiracy --no-entry

out/bootstrap-x64/test/gc1.sasm: tests/gc1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR) out/bootstrap-x64/test/tests-printer-helper.sasm out/bootstrap-x64/test/tests-printer.sasm
	$(SCHEMEC) tests/gc1.scm --outdir out/bootstrap-x64/test --conspiracy
	cp out/bootstrap-x64/test/tests-gc1.sasm out/bootstrap-x64/test/gc1.sasm

out/bootstrap-x64/test/gc1-expanded.scm: tests/gc1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/gc1.scm --output out/bootstrap-x64/test/gc1-expanded.scm --expand-only

out/bootstrap-x64/test/gc1.sasm-opt-x64: out/bootstrap-x64/test/gc1.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/gc1.sasm --out=out/bootstrap-x64/test/gc1.sasm-opt-x64 --cheap

out/bootstrap-x64/test/tests-printer.sasm-opt-x64: out/bootstrap-x64/test/tests-printer.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/tests-printer.sasm --out=out/bootstrap-x64/test/tests-printer.sasm-opt-x64 --cheap

out/bootstrap-x64/test/tests-printer-helper.sasm-opt-x64: out/bootstrap-x64/test/tests-printer-helper.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap-x64/test/tests-printer-helper.sasm --out=out/bootstrap-x64/test/tests-printer-helper.sasm-opt-x64 --cheap

out/bootstrap-x64/test/gc1.asm: out/bootstrap-x64/test/gc1.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/gc1.sasm-opt-x64 --out=out/bootstrap-x64/test/gc1.asm

out/bootstrap-x64/test/tests-printer.asm: out/bootstrap-x64/test/tests-printer.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/tests-printer.sasm-opt-x64 --out=out/bootstrap-x64/test/tests-printer.asm

out/bootstrap-x64/test/tests-printer-helper.asm: out/bootstrap-x64/test/tests-printer-helper.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) out/bootstrap-x64/test/tests-printer-helper.sasm-opt-x64 --out=out/bootstrap-x64/test/tests-printer-helper.asm

out/bootstrap-x64/test/gc1.o: out/bootstrap-x64/test/gc1.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/gc1.asm -o out/bootstrap-x64/test/gc1.o

out/bootstrap-x64/test/tests-printer.o: out/bootstrap-x64/test/tests-printer.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/tests-printer.asm -o out/bootstrap-x64/test/tests-printer.o

out/bootstrap-x64/test/tests-printer-helper.o: out/bootstrap-x64/test/tests-printer-helper.asm
	nasm $(NASM_FLAGS) out/bootstrap-x64/test/tests-printer-helper.asm -o out/bootstrap-x64/test/tests-printer-helper.o

out/bootstrap-x64/test/gc1.exe: out/bootstrap-x64/test/gc1.o out/bootstrap-x64/test/tests-printer.o out/bootstrap-x64/test/tests-printer-helper.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) $< out/bootstrap-x64/test/tests-printer.o out/bootstrap-x64/test/tests-printer-helper.o -o $@

out/bootstrap-x64/test/gc1.out: out/bootstrap-x64/test/gc1.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/r5rs3.out: out/bootstrap-x64/test/r5rs3.exe
	$< out/bootstrap-x64/test/r5rs3-cwof.dat>$@.tmp
	mv $@.tmp $@

# Test scheme-compiler: general case
out/bootstrap-x64/test/%.sasm: tests/%.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) $< --output $@

out/bootstrap-x64/test/%-expanded.scm: tests/%.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) $< --output $@ --expand-only

out/bootstrap-x64/test/%.sasm-opt-x64: out/bootstrap-x64/test/%.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

out/bootstrap-x64/test/%.asm: out/bootstrap-x64/test/%.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) $< --out=$@

out/bootstrap-x64/test/%.o: out/bootstrap-x64/test/%.asm
	nasm $(NASM_FLAGS) $< -o $@

out/bootstrap-x64/test/%.exe: out/bootstrap-x64/test/%.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap-x64/test/%.out: out/bootstrap-x64/test/%.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap-x64/test/%.diff: out/bootstrap-x64/test/%.out tests/baseline/%-s.actual
	diff --strip-trailing-cr $< $(patsubst out/bootstrap-x64/test/%.out,tests/baseline/%-s.actual,$<)
	touch $@



# Compile scheme RTL
out/bootstrap-x64/r5rs-library.sasm: rtl/r5rs-library.scm $(DEPEND_SCHEMEC)
	$(SCHEMEC) rtl/r5rs-library.scm --outdir out/bootstrap-x64 --conspiracy --no-entry
	cp out/bootstrap-x64/rtl-r5rs-library.sasm out/bootstrap-x64/r5rs-library.sasm

out/bootstrap-x64/r5rs-library.sasm-opt-x64 : out/bootstrap-x64/r5rs-library.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

out/bootstrap-x64/r5rs-wrap.sasm: rtl/r5rs-wrap.scm $(DEPEND_SCHEMEC)
	$(SCHEMEC) rtl/r5rs-wrap.scm --outdir out/bootstrap-x64 --conspiracy --no-entry
	cp out/bootstrap-x64/rtl-r5rs-wrap.sasm out/bootstrap-x64/r5rs-wrap.sasm

out/bootstrap-x64/r5rs-wrap.sasm-opt-x64 : out/bootstrap-x64/r5rs-wrap.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

out/bootstrap-x64/r5rs-native.sasm : rtl/r5rs-native.scm $(DEPEND_GLUEC) $(BOOTSTRAP_DIR)
	$(GLUEC) rtl/r5rs-native.scm -o out/bootstrap-x64/r5rs-native.sasm

out/bootstrap-x64/r5rs-native.sasm-opt-x64: out/bootstrap-x64/r5rs-native.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@

# sasm-sasm-nasmx86 override, disable-optimizations

out/bootstrap-x64/sasm-sasm-nasmx86.sasm-opt-x64: out/bootstrap-x64/sasm-sasm-nasmx86.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

# rtl rules
out/bootstrap-x64/%.o: out/bootstrap-x64/%.asm
	nasm $(NASM_FLAGS) $< -o $@

out/bootstrap-x64/debug.asm: rtl/debug.asm
	cp rtl/debug.asm out/bootstrap-x64/debug.asm

out/bootstrap-x64/%.asm: out/bootstrap-x64/%.sasm-opt-x64 $(DEPEND_SASMC)
	$(SASMC) $< --out=$@

# bootstrap sasm-x64 tool
out/bootstrap-sasm-x64-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm_x64)
	$(SCHEME) needc-ts.scm --script-mode --output out/bootstrap-sasm-x64-ts.sh sasm-x64

out/sasm-x64-bootstrap.out: out/bootstrap-sasm-x64-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-sasm-x64-ts.sh
	./out/bootstrap-sasm-x64-ts.sh
	touch out/sasm-x64-bootstrap.out

out/bootstrap-x64/sasm-x64.exe: $(DEPEND_RTL) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/sasm-x64-bootstrap.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/bootstrap-x64 $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/bootstrap-x64/sasm.o -o $@ -lsasm


# bootstrap sasm-opt-x64 tool
out/bootstrap-sasm-opt-x64-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm_opt_x64)
	$(SCHEME) needc-ts.scm --script-mode --output out/bootstrap-sasm-opt-x64-ts.sh sasm-opt-x64

out/sasm-opt-x64-bootstrap.out: out/bootstrap-sasm-opt-x64-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-sasm-opt-x64-ts.sh
	./out/bootstrap-sasm-opt-x64-ts.sh
	touch out/sasm-opt-x64-bootstrap.out

out/bootstrap-x64/sasm-opt-x64.exe: $(DEPEND_RTL) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/sasm-opt-x64-bootstrap.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/bootstrap-x64 $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/bootstrap-x64/sasm-opt-x64.o -o $@ -lsasm-opt-x64


# bootstrap schemec
out/bootstrap-scheme-compiler-x64-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_scheme_compiler)
	$(SCHEME) needc-ts.scm --script-mode --output out/bootstrap-scheme-compiler-x64-ts.sh scheme-compiler

out/scheme-compiler-x64-bootstrap.out: out/bootstrap-scheme-compiler-x64-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-scheme-compiler-x64-ts.sh
	./out/bootstrap-scheme-compiler-x64-ts.sh
	touch out/scheme-compiler-x64-bootstrap.out

out/bootstrap-x64/schemec.exe: $(DEPEND_RTL) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/scheme-compiler-x64-bootstrap.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/bootstrap-x64 $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/bootstrap-x64/scheme-compiler.o -o $@ -lscheme-compiler

# RTL rules which have to go under specific rules above
out/bootstrap-x64/%.sasm-opt-x64: rtl/%.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@

