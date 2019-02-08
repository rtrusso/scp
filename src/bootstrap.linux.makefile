include sasm.dep
include sasm-opt.dep
include scheme-compiler.dep
include java-compiler.dep

# Depends on the following environment variables which are expected to be set by running go.cmd or env.cmd:
#
# TINYSCHEMEINIT=../bootstrap/tinyscheme-1.41/init.scm
# SCHEME=../bootstrap/tinyscheme-1.41/scheme.exe -1
# SASMOPT=$(SCHEME) out/sasm-opt-flat-ts.scm
# SASMC=$(SCHEME) out/sasm-flat-ts.scm
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
BOOTSTRAP_DIR=out/bootstrap/.exists
BOOTSTRAP_TEST_DIR=out/bootstrap/test/.exists
BOOTSTRAP_TEST_JAVA_DIR=out/bootstrap/test/java/.exists
BOOTSTRAP_TEST_JAVA_GC_DIR=out/bootstrap/test/java/gc/.exists

SCHEMEC_FLAT_TS=out/scheme-compiler-flat-ts.scm

JAVAC_FLAT_TS=out/java-compiler-flat-ts.scm
JAVAC=$(SCHEME) $(JAVAC_FLAT_TS)

GLUEC_FLAT_TS=out/scheme-gluec-flat-ts.scm
GLUEC=$(SCHEME) $(GLUEC_FLAT_TS)
SCHEME_CFLAGS=-DSCHEME_RTL=1

SCHEME_RTL=\
  out/bootstrap/r5rs-library.sasm \
  out/bootstrap/r5rs-library.sasm-opt \
  out/bootstrap/r5rs-library.asm \
  out/bootstrap/r5rs-library.o \
  out/bootstrap/r5rs-native.sasm \
  out/bootstrap/r5rs-native.sasm-opt \
  out/bootstrap/r5rs-native.asm \
  out/bootstrap/r5rs-native.o \
  out/bootstrap/r5rs-wrap.sasm \
  out/bootstrap/r5rs-wrap.sasm-opt \
  out/bootstrap/r5rs-wrap.asm \
  out/bootstrap/r5rs-wrap.o \
  out/bootstrap/rtlscheme.sasm-opt \
  out/bootstrap/rtlscheme.asm \
  out/bootstrap/rtlscheme.o

DEPEND_OUT_DIRS=\
  $(OUT_DIR) \
  $(BOOTSTRAP_DIR) \
  $(BOOTSTRAP_TEST_DIR) \
  $(BOOTSTRAP_TEST_JAVA_DIR)

DEPEND_EXPAND_TESTS=\
  out/bootstrap/test/apply-expanded.scm \
  out/bootstrap/test/syntax1-expanded.scm \
  out/bootstrap/test/count.sasm

DEPEND_SCHEMEC=\
  $(SCHEMEC_FLAT_TS)

DEPEND_JAVAC=\
  $(JAVAC_FLAT_TS)

DEPEND_SASMOPT=\
  out/sasm-opt-flat-ts.scm

DEPEND_SASMC=\
  out/sasm-flat-ts.scm

DEPEND_GLUEC=\
  $(GLUEC_FLAT_TS)

DEPEND_SASM_SIMPLE_TEST=\
  out/bootstrap/test/call.sasm-opt \
  out/bootstrap/test/call.asm \
  out/bootstrap/test/call.o \
  out/bootstrap/test/call.exe \
  out/bootstrap/test/call.out \
  out/bootstrap/test/call.diff

DEPEND_SCHEMEC_SIMPLE_TEST=\
  out/bootstrap/test/apply.sasm \
  out/bootstrap/test/apply.sasm-opt \
  out/bootstrap/test/apply.asm \
  out/bootstrap/test/apply.o \
  out/bootstrap/test/apply.exe \
  out/bootstrap/test/apply.out \
  out/bootstrap/test/apply.diff \
  out/bootstrap/test/cseconds.out \
  out/bootstrap/test/disptest.out \
  out/bootstrap/test/disptest.diff \
  out/bootstrap/test/disptest2.out \
  out/bootstrap/test/disptest2.diff \
  out/bootstrap/test/argv.out \
  out/bootstrap/test/argv.diff \
  out/bootstrap/test/rename.out \
  out/bootstrap/test/rename.diff \
  out/bootstrap/test/delete.out \
  out/bootstrap/test/delete.diff \
  out/bootstrap/test/stat.out \
  out/bootstrap/test/stat.diff \
  out/bootstrap/test/getenv.out \
  out/bootstrap/test/getenv.diff \
  out/bootstrap/test/outputfile.out \
  out/bootstrap/test/outputfile.diff \
  out/bootstrap/test/inputfile.out \
  out/bootstrap/test/inputfile.diff \
  out/bootstrap/test/read.out \
  out/bootstrap/test/read.diff \
  out/bootstrap/test/read2.out \
  out/bootstrap/test/read2.diff \
  out/bootstrap/test/read3.out \
  out/bootstrap/test/read3.diff \
  out/bootstrap/test/peekchar.out \
  out/bootstrap/test/peekchar.diff \
  out/bootstrap/test/mkvec.out \
  out/bootstrap/test/mkvec.diff \
  out/bootstrap/test/eqv.out \
  out/bootstrap/test/eqv.diff \
  out/bootstrap/test/eq.out \
  out/bootstrap/test/eq.diff \
  out/bootstrap/test/vararg.out \
  out/bootstrap/test/vararg.diff \
  out/bootstrap/test/read4.out \
  out/bootstrap/test/read4.diff \
  out/bootstrap/test/read5.out \
  out/bootstrap/test/read5.diff \
  out/bootstrap/test/sym1.out \
  out/bootstrap/test/sym1.diff \
  out/bootstrap/test/opeq.out \
  out/bootstrap/test/opeq.diff \
  out/bootstrap/test/opminus.out \
  out/bootstrap/test/opminus.diff \
  out/bootstrap/test/opplus.out \
  out/bootstrap/test/opplus.diff \
  out/bootstrap/test/opgt.out \
  out/bootstrap/test/opgt.diff \
  out/bootstrap/test/oplt.out \
  out/bootstrap/test/oplt.diff \
  out/bootstrap/test/opmul.out \
  out/bootstrap/test/opmul.diff \
  out/bootstrap/test/pair.out \
  out/bootstrap/test/pair.diff \
  out/bootstrap/test/mkstring.out \
  out/bootstrap/test/mkstring.diff \
  out/bootstrap/test/str2num.out \
  out/bootstrap/test/str2num.diff \
  out/bootstrap/test/opdiv.out \
  out/bootstrap/test/opdiv.diff \
  out/bootstrap/test/char.out \
  out/bootstrap/test/char.diff \
  out/bootstrap/test/pred.out \
  out/bootstrap/test/pred.diff \
  out/bootstrap/test/write.out \
  out/bootstrap/test/write.diff \
  out/bootstrap/test/equal.out \
  out/bootstrap/test/equal.diff \
  out/bootstrap/test/r5rs1.out \
  out/bootstrap/test/r5rs1.diff \
  out/bootstrap/test/r5rs2.out \
  out/bootstrap/test/r5rs2.diff \
  out/bootstrap/test/r5rs3.out \
  out/bootstrap/test/r5rs3.diff \
  out/bootstrap/test/char-case.out \
  out/bootstrap/test/char-case.diff \
  out/bootstrap/test/string.out \
  out/bootstrap/test/string.diff \
  out/bootstrap/test/string2.out \
  out/bootstrap/test/string2.diff \
  out/bootstrap/test/string3.out \
  out/bootstrap/test/string3.diff \
  out/bootstrap/test/read6.out \
  out/bootstrap/test/read6.diff \
  out/bootstrap/test/str2num2.out \
  out/bootstrap/test/str2num2.diff \
  out/bootstrap/test/num2str.out \
  out/bootstrap/test/num2str.diff \
  out/bootstrap/test/set1.out \
  out/bootstrap/test/set1.diff \
  out/bootstrap/test/read7.out \
  out/bootstrap/test/read7.diff \
  out/bootstrap/test/read8.out \
  out/bootstrap/test/read8.diff \
  out/bootstrap/test/strref.out \
  out/bootstrap/test/strref.diff \
  out/bootstrap/test/stringset.out \
  out/bootstrap/test/stringset.diff \
  out/bootstrap/test/order-eval.out \
  out/bootstrap/test/order-eval.diff \
  out/bootstrap/test/read9.out \
  out/bootstrap/test/read9.diff \
  out/bootstrap/test/read10.out \
  out/bootstrap/test/read10.diff \
  out/bootstrap/test/read11.out \
  out/bootstrap/test/read11.diff \
  \
  out/bootstrap/test/badapply.out \
  out/bootstrap/test/badapply.diff \
  out/bootstrap/test/badvrhi.out \
  out/bootstrap/test/badvrhi.diff \
  out/bootstrap/test/badvrlo.out \
  out/bootstrap/test/badvrlo.diff \
  out/bootstrap/test/badvshi.out \
  out/bootstrap/test/badvshi.diff \
  out/bootstrap/test/badvslo.out \
  out/bootstrap/test/badvslo.diff \
  out/bootstrap/test/badsrhi.out \
  out/bootstrap/test/badsrhi.diff \
  out/bootstrap/test/badsrlo.out \
  out/bootstrap/test/badsrlo.diff \
  out/bootstrap/test/badsshi.out \
  out/bootstrap/test/badsshi.diff \
  out/bootstrap/test/badsslo.out \
  out/bootstrap/test/badsslo.diff \
  out/bootstrap/test/gc1.out

DEPEND_RTL_OBJS=\
 out/bootstrap/c-rtlheap.o \
 out/bootstrap/gc.o \
 out/bootstrap/gc-wrapper.o \
 out/bootstrap/gc-invoke.o \
 out/bootstrap/gc-stack.o \
 out/bootstrap/gc-mark.o \
 out/bootstrap/gc-mark-range.o \
 out/bootstrap/gc-mark-array.o \
 out/bootstrap/gc-mark-class.o \
 out/bootstrap/gc-sweep.o \
 out/bootstrap/heap.o \
 out/bootstrap/heapfixed.o \
 out/bootstrap/heapvar.o \
 out/bootstrap/mjrtl.o \
 out/bootstrap/rtlheap.o \
 out/bootstrap/debug.o

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
  out/bootstrap/c-rtlheap.sasm-opt \
  out/bootstrap/c-rtlheap.asm \
  out/bootstrap/gc.sasm-opt \
  out/bootstrap/gc.asm \
  out/bootstrap/heap.sasm-opt \
  out/bootstrap/heap.asm \
  out/bootstrap/heapfixed.sasm-opt \
  out/bootstrap/heapfixed.asm \
  out/bootstrap/heapvar.sasm-opt \
  out/bootstrap/heapvar.asm \
  out/bootstrap/mjrtl.sasm-opt \
  out/bootstrap/mjrtl.asm \
  out/bootstrap/rtlheap.sasm-opt \
  out/bootstrap/rtlheap.asm \
  $(DEPEND_RTL_C) \
  $(DEPEND_RTL_OBJS)

DEPEND_SCHEME_RTL_OMIT_MAIN=\
  out/bootstrap/r5rs-library.o \
  out/bootstrap/r5rs-native.o \
  out/bootstrap/r5rs-wrap.o \
  out/bootstrap/rtlscheme.o

DEPEND_SCHEME_RTL=\
  $(DEPEND_SCHEME_RTL_OMIT_MAIN) \
  out/bootstrap/scheme-main.o

DEPEND_NEEDC=\
   needc-ts.scm \
   needc.scm

DEPEND_JAVA_TEST_OUTPUT_FILES=\
  out/bootstrap/test/java/Arrays.out \
  out/bootstrap/test/java/BinarySearch.out \
  out/bootstrap/test/java/BinaryTree.out \
  out/bootstrap/test/java/Bitwise.out \
  out/bootstrap/test/java/BubbleSort.out \
  out/bootstrap/test/java/CharString.out \
  out/bootstrap/test/java/Count.out \
  out/bootstrap/test/java/CtorTest.out \
  out/bootstrap/test/java/Factorial.out \
  out/bootstrap/test/java/LinearSearch.out \
  out/bootstrap/test/java/LinkedList.out \
  out/bootstrap/test/java/Messy.out \
  out/bootstrap/test/java/MyFactorial.out \
  out/bootstrap/test/java/NumberToString.out \
  out/bootstrap/test/java/ObjArray.out \
  out/bootstrap/test/java/OpEquals.out \
  out/bootstrap/test/java/OverrideTest.out \
  out/bootstrap/test/java/QuickSort.out \
  out/bootstrap/test/java/Rectangles.out \
  out/bootstrap/test/java/StaticMembers.out \
  out/bootstrap/test/java/StaticMethods.out \
  out/bootstrap/test/java/SubExp.out \
  out/bootstrap/test/java/TreeVisitor.out \
  out/bootstrap/test/java/TwoArgs.out

DEPEND_JAVA_TEST_EXE_FILES=\
  $(subst %.out,%.exe,$(DEPEND_JAVA_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_OUTPUT_FILES=\
  out/bootstrap/test/java/gc/GCTest1.out \
  out/bootstrap/test/java/gc/GCTest2.out \
  out/bootstrap/test/java/gc/GCTest3.out \
  out/bootstrap/test/java/gc/GCTest4.out \
  out/bootstrap/test/java/gc/GCStress.out \
  out/bootstrap/test/java/gc/GCStress2.out \
  out/bootstrap/test/java/gc/GCStress3.out

DEPEND_JAVA_GC_TEST_EXE_FILES=\
  $(subst %.out,%.exe,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_ASM_FILES=\
  $(subst %.out,%.asm,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_SASM_FILES=\
  $(subst %.out,%.sasm,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_SASM_OPT_FILES=\
  $(subst %.out,%.sasm-opt,$(DEPEND_JAVA_GC_TEST_OUTPUT_FILES))

DEPEND_JAVA_GC_TEST_FILES=\
  $(DEPEND_JAVA_GC_TEST_OUTPUT_FILES) \
  $(DEPEND_JAVA_GC_TEST_EXE_FILES) \
  $(DEPEND_JAVA_GC_TEST_ASM_FILES) \
  $(DEPEND_JAVA_GC_TEST_SASM_FILES) \
  $(DEPEND_JAVA_GC_TEST_SASM_OPT_FILES)

DEPEND_JAVA_TEST_MARKER=\
  out/bootstrap/test/java/tests.done

DEPEND_JAVA_GC_TEST_MARKER=\
  out/bootstrap/test/java/gc/tests.done

DEPEND_SCHEMEC_TEST_MARKER=\
  out/bootstrap/test/schemec-tests.done

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
  out/bootstrap-sasm-ts.cmd \
  out/sasm-bootstrap.out \
  out/bootstrap/sasm.exe \
  \
  out/bootstrap-sasm-opt-ts.cmd \
  out/sasm-opt-bootstrap.out \
  out/bootstrap/sasm-opt.exe \
  \
  out/bootstrap-scheme-compiler-ts.cmd \
  out/scheme-compiler-bootstrap.out \
  out/bootstrap/schemec.exe

all: $(DEPEND_ALL)
	echo done>all

.SECONDARY:

out/.exists:
	if [ -d out ]; then echo foo>/dev/null; else mkdir out; fi
	touch out/.exists

out/bootstrap/.exists : out/.exists
	if [ -d out/bootstrap ]; then echo foo>/dev/null; else mkdir out/bootstrap; fi
	touch out/bootstrap/.exists

out/bootstrap/test/.exists : out/bootstrap/.exists
	if [ -d out/bootstrap/test ]; then echo foo>/dev/null; else mkdir out/bootstrap/test; fi
	touch out/bootstrap/test/.exists

out/bootstrap/test/java/.exists : out/bootstrap/test/.exists
	if [ -d out/bootstrap/test/java ]; then echo foo>/dev/null; else mkdir out/bootstrap/test/java; fi
	touch out/bootstrap/test/java/.exists

out/bootstrap/test/java/gc/.exists : out/bootstrap/test/java/.exists
	if [ -d out/bootstrap/test/java/gc ]; then echo foo>/dev/null; else mkdir out/bootstrap/test/java/gc; fi
	touch out/bootstrap/test/java/gc/.exists

out/scheme-compiler-flat-ts.scm : $(OUT_DIR) needc-ts.scm $(deps_of_scheme_compiler)
	$(SCHEME) needc-ts.scm --output out/scheme-compiler-flat-ts.scm scheme-compiler-ts

out/scheme-gluec-flat-ts.scm : $(OUT_DIR) needc-ts.scm scheme-gluec.scm scheme-gluec-ts.scm
	$(SCHEME) needc-ts.scm --output out/scheme-gluec-flat-ts.scm scheme-gluec-ts

out/bootstrap/test/apply-expanded.scm : tests/apply.scm $(BOOTSTRAP_TEST_DIR) $(DEPEND_SCHEMEC)
	$(SCHEMEC) tests/apply.scm --output out/bootstrap/test/apply-expanded.scm --expand-only

out/bootstrap/test/syntax1-expanded.scm : $(SCHEMEC_FLAT_TS) tests/syntax1.scm $(BOOTSTRAP_TEST_DIR) $(DEPEND_SCHEMEC)
	$(SCHEMEC) tests/syntax1.scm --output out/bootstrap/test/syntax1-expanded.scm --expand-only

# Expand, flatten minijava compiler
out/bootstrap-expand-java-compiler-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_java_compiler)
	$(SCHEME) needc-ts.scm --script-mode --expand-only --output out/bootstrap-expand-java-compiler-ts.sh java-compiler-ts

out/java-compiler-expanded.out: out/bootstrap-expand-java-compiler-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-expand-java-compiler-ts.sh
	./out/bootstrap-expand-java-compiler-ts.sh
	touch out/java-compiler-expanded.out

out/java-compiler-flat-ts.scm : out/java-compiler-expanded.out
	$(SCHEME) needc-ts.scm --root out/bootstrap --flat-names --output out/java-compiler-flat-ts.scm java-compiler-ts

# Expand, flatten sasm-opt-ts tool
out/bootstrap-expand-sasm-opt-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm_opt)
	$(SCHEME) needc-ts.scm --script-mode --expand-only --output out/bootstrap-expand-sasm-opt-ts.sh sasm-opt-ts

out/sasm-opt-ts-expanded.out: out/bootstrap-expand-sasm-opt-ts.sh env.sh  $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-expand-sasm-opt-ts.sh
	./out/bootstrap-expand-sasm-opt-ts.sh
	touch out/sasm-opt-ts-expanded.out

out/sasm-opt-flat-ts.scm : out/sasm-opt-ts-expanded.out
	$(SCHEME) needc-ts.scm --root out/bootstrap --flat-names --output out/sasm-opt-flat-ts.scm sasm-opt-ts

# Expand, flatten sasm-ts tool
out/bootstrap-expand-sasm-ts.sh : sasm.scm $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm)
	$(SCHEME) needc-ts.scm --script-mode --expand-only --output out/bootstrap-expand-sasm-ts.sh sasm-ts

out/sasm-ts-expanded.out: out/bootstrap-expand-sasm-ts.sh env.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-expand-sasm-ts.sh
	./out/bootstrap-expand-sasm-ts.sh
	touch out/sasm-ts-expanded.out

out/sasm-flat-ts.scm: out/sasm-ts-expanded.out
	$(SCHEME) needc-ts.scm --root out/bootstrap --flat-names --output out/sasm-flat-ts.scm sasm-ts

# test minijava compiler
out/bootstrap/test/java/%.sasm : tests/%.java $(BOOTSTRAP_TEST_JAVA_DIR) $(DEPEND_JAVAC)
	$(JAVAC) --main=$(patsubst tests/%.java,%,$<) --out=$@ rtl/JavaRtl.java $<

out/bootstrap/test/java/%.sasm-opt: out/bootstrap/test/java/%.sasm $(DEPEND_SASMOPT) $(BOOTSTRAP_TEST_JAVA_DIR)
	$(SASMOPT) $< --out=$@

out/bootstrap/test/java/%.asm: out/bootstrap/test/java/%.sasm-opt $(DEPEND_SASMC) $(BOOTSTRAP_TEST_JAVA_DIR)
	$(SASMC) $< --out=$@

out/bootstrap/test/java/%.o: out/bootstrap/test/java/%.asm
	nasm -felf32 $< -o $@

out/bootstrap/test/java/%.exe: out/bootstrap/test/java/%.o rtl/mjrtl.c $(DEPEND_MJ_RTL)
	gcc -Irtl rtl/mjrtl.c $(DEPEND_MJ_RTL_OBJS) $< -o $@

out/bootstrap/test/java/%.out: out/bootstrap/test/java/%.exe
	$< >$@.tmp
	mv $@.tmp $@

$(DEPEND_JAVA_TEST_MARKER): $(BOOTSTRAP_TEST_JAVA_DIR) $(DEPEND_JAVA_TEST_OUTPUT_FILES) $(DEPEND_JAVA_TEST_EXE_FILES)
	touch out/bootstrap/test/java/tests.done

# java GC "stress" tests
out/bootstrap/test/java/gc/%.sasm : tests/gc/%.java $(BOOTSTRAP_TEST_JAVA_GC_DIR) $(DEPEND_JAVAC)
	$(JAVAC) --main=$(patsubst tests/gc/%.java,%,$<) --out=$@ rtl/JavaRtl.java $<

out/bootstrap/test/java/gc/%.sasm-opt: out/bootstrap/test/java/gc/%.sasm $(DEPEND_SASMOPT) $(BOOTSTRAP_TEST_JAVA_GC_DIR)
	$(SASMOPT) $< --out=$@

out/bootstrap/test/java/gc/%.asm: out/bootstrap/test/java/gc/%.sasm-opt $(DEPEND_SASMC) $(BOOTSTRAP_TEST_JAVA_GC_DIR)
	$(SASMC) $< --out=$@

out/bootstrap/test/java/gc/%.o: out/bootstrap/test/java/gc/%.asm
	nasm -felf32 $< -o $@

out/bootstrap/test/java/gc/%.exe: out/bootstrap/test/java/gc/%.o rtl/mjrtl.c $(DEPEND_MJ_RTL)
	gcc -Irtl rtl/mjrtl.c $(DEPEND_MJ_RTL_OBJS) $< -o $@

out/bootstrap/test/java/gc/%.out: out/bootstrap/test/java/gc/%.exe
	$< >$@.tmp
	mv $@.tmp $@

$(DEPEND_JAVA_GC_TEST_MARKER): $(BOOTSTRAP_TEST_JAVA_GC_DIR) $(DEPEND_JAVA_GC_TEST_FILES)
	touch out/bootstrap/test/java/gc/tests.done

$(DEPEND_SCHEMEC_TEST_MARKER): $(BOOTSTRAP_TEST_DIR) $(DEPEND_SCHEMEC_SIMPLE_TEST)
	touch out/bootstrap/test/schemec-tests.done

# Test sasmopt, sasmc - compile and run call.sasm
out/bootstrap/test/call.sasm-opt: tests/call.sasm $(DEPEND_SASMOPT) $(BOOTSTRAP_TEST_DIR)
	$(SASMOPT) tests/call.sasm --out=out/bootstrap/test/call.sasm-opt

out/bootstrap/test/call.asm: out/bootstrap/test/call.sasm-opt $(DEPEND_SASMC) $(BOOTSTRAP_TEST_DIR)
	$(SASMC) out/bootstrap/test/call.sasm-opt --out=out/bootstrap/test/call.asm

out/bootstrap/test/call.o: out/bootstrap/test/call.asm
	nasm -felf32 out/bootstrap/test/call.asm -o out/bootstrap/test/call.o

out/bootstrap/test/call.exe: out/bootstrap/test/call.o rtl/mjrtl.c
	gcc -Irtl rtl/mjrtl.c $(DEPEND_RTL_OBJS) $< -o $@

out/bootstrap/test/call.out: out/bootstrap/test/call.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/call.diff: out/bootstrap/test/call.out tests/baseline/call.sasm-interp.actual
	diff --strip-trailing-cr out/bootstrap/test/call.out tests/baseline/call.sasm-interp.actual
	touch out/bootstrap/test/call.diff

# Test scheme-compiler: apply
out/bootstrap/test/apply.sasm: tests/apply.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/apply.scm --output out/bootstrap/test/apply.sasm

out/bootstrap/test/apply.sasm-opt: out/bootstrap/test/apply.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/apply.sasm --out=out/bootstrap/test/apply.sasm-opt

out/bootstrap/test/apply.asm: out/bootstrap/test/apply.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/apply.sasm-opt --out=out/bootstrap/test/apply.asm

out/bootstrap/test/apply.o: out/bootstrap/test/apply.asm
	nasm -felf32 out/bootstrap/test/apply.asm -o out/bootstrap/test/apply.o

out/bootstrap/test/apply.exe: out/bootstrap/test/apply.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/apply.out: out/bootstrap/test/apply.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/apply.diff: out/bootstrap/test/apply.out tests/baseline/apply-s.actual
	diff --strip-trailing-cr out/bootstrap/test/apply.out tests/baseline/apply-s.actual
	touch out/bootstrap/test/apply.diff

# Test scheme-compiler: getenv
out/bootstrap/test/getenv.sasm: tests/getenv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/getenv.scm --output out/bootstrap/test/getenv.sasm

out/bootstrap/test/getenv.sasm-opt: out/bootstrap/test/getenv.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/getenv.sasm --out=out/bootstrap/test/getenv.sasm-opt

out/bootstrap/test/getenv.asm: out/bootstrap/test/getenv.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/getenv.sasm-opt --out=out/bootstrap/test/getenv.asm

out/bootstrap/test/getenv.o: out/bootstrap/test/getenv.asm
	nasm -felf32 out/bootstrap/test/getenv.asm -o out/bootstrap/test/getenv.o

out/bootstrap/test/getenv.exe: out/bootstrap/test/getenv.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/getenv.out: out/bootstrap/test/getenv.exe
	export SCHEME_TEST=SCHEME_TEST;$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/getenv.diff: out/bootstrap/test/getenv.out tests/baseline/getenv-s.actual
	diff --strip-trailing-cr out/bootstrap/test/getenv.out tests/baseline/getenv-s.actual
	touch out/bootstrap/test/getenv.diff

# Test scheme-compiler: cseconds
out/bootstrap/test/cseconds.sasm: tests/cseconds.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/cseconds.scm --output out/bootstrap/test/cseconds.sasm

out/bootstrap/test/cseconds.sasm-opt: out/bootstrap/test/cseconds.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/cseconds.sasm --out=out/bootstrap/test/cseconds.sasm-opt --cheap

out/bootstrap/test/cseconds.asm: out/bootstrap/test/cseconds.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/cseconds.sasm-opt --out=out/bootstrap/test/cseconds.asm

out/bootstrap/test/cseconds.o: out/bootstrap/test/cseconds.asm
	nasm -felf32 out/bootstrap/test/cseconds.asm -o out/bootstrap/test/cseconds.o

out/bootstrap/test/cseconds.exe: out/bootstrap/test/cseconds.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/cseconds.out: out/bootstrap/test/cseconds.exe
	$< >$@.tmp
	mv $@.tmp $@

# Test scheme-compiler: stat
out/bootstrap/test/stat.sasm: tests/stat.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/stat.scm --output out/bootstrap/test/stat.sasm

out/bootstrap/test/stat.sasm-opt: out/bootstrap/test/stat.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/stat.sasm --out=out/bootstrap/test/stat.sasm-opt --cheap

out/bootstrap/test/stat.asm: out/bootstrap/test/stat.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/stat.sasm-opt --out=out/bootstrap/test/stat.asm

out/bootstrap/test/stat.o: out/bootstrap/test/stat.asm
	nasm -felf32 out/bootstrap/test/stat.asm -o out/bootstrap/test/stat.o

out/bootstrap/test/stat.exe: out/bootstrap/test/stat.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/stat.out: out/bootstrap/test/stat.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/stat.diff: out/bootstrap/test/stat.out tests/baseline/stat-s.actual
	diff --strip-trailing-cr out/bootstrap/test/stat.out tests/baseline/stat-s.actual
	touch out/bootstrap/test/stat.diff

# Test scheme-compiler: delete
out/bootstrap/test/delete.sasm: tests/delete.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/delete.scm --output out/bootstrap/test/delete.sasm

out/bootstrap/test/delete.sasm-opt: out/bootstrap/test/delete.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/delete.sasm --out=out/bootstrap/test/delete.sasm-opt --cheap

out/bootstrap/test/delete.asm: out/bootstrap/test/delete.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/delete.sasm-opt --out=out/bootstrap/test/delete.asm

out/bootstrap/test/delete.o: out/bootstrap/test/delete.asm
	nasm -felf32 out/bootstrap/test/delete.asm -o out/bootstrap/test/delete.o

out/bootstrap/test/delete.exe: out/bootstrap/test/delete.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/delete.out: out/bootstrap/test/delete.exe
	touch deleteme.file
	$< >$@.tmp
	if [ -f deleteme.file ]; then exit 128; fi
	mv $@.tmp $@

out/bootstrap/test/delete.diff: out/bootstrap/test/delete.out tests/baseline/delete-s.actual
	diff --strip-trailing-cr out/bootstrap/test/delete.out tests/baseline/delete-s.actual
	touch out/bootstrap/test/delete.diff

# Test scheme-compiler: rename
out/bootstrap/test/rename.sasm: tests/rename.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/rename.scm --output out/bootstrap/test/rename.sasm

out/bootstrap/test/rename.sasm-opt: out/bootstrap/test/rename.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/rename.sasm --out=out/bootstrap/test/rename.sasm-opt --cheap

out/bootstrap/test/rename.asm: out/bootstrap/test/rename.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/rename.sasm-opt --out=out/bootstrap/test/rename.asm

out/bootstrap/test/rename.o: out/bootstrap/test/rename.asm
	nasm -felf32 out/bootstrap/test/rename.asm -o out/bootstrap/test/rename.o

out/bootstrap/test/rename.exe: out/bootstrap/test/rename.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/rename.out: out/bootstrap/test/rename.exe
	touch from.file
	$< >$@.tmp
	if [ -f from.file ]; then exit 128; fi
	if [ ! -f to.file ]; then exit 128; fi
	rm to.file
	mv $@.tmp $@

out/bootstrap/test/rename.diff: out/bootstrap/test/rename.out tests/baseline/rename-s.actual
	diff --strip-trailing-cr out/bootstrap/test/rename.out tests/baseline/rename-s.actual
	touch out/bootstrap/test/rename.diff

# Test scheme-compiler: argv
out/bootstrap/test/argv.sasm: tests/argv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/argv.scm --output out/bootstrap/test/argv.sasm

out/bootstrap/test/argv.sasm-opt: out/bootstrap/test/argv.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/argv.sasm --out=out/bootstrap/test/argv.sasm-opt --cheap

out/bootstrap/test/argv.asm: out/bootstrap/test/argv.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/argv.sasm-opt --out=out/bootstrap/test/argv.asm

out/bootstrap/test/argv.o: out/bootstrap/test/argv.asm
	nasm -felf32 out/bootstrap/test/argv.asm -o out/bootstrap/test/argv.o

out/bootstrap/test/argv.exe: out/bootstrap/test/argv.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/argv.out: out/bootstrap/test/argv.exe
	$< 1 2 3 4 five six 7 8 9 ten>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/argv.diff: out/bootstrap/test/argv.out tests/baseline/argv-s.actual
	diff --strip-trailing-cr out/bootstrap/test/argv.out tests/baseline/argv-s.actual
	touch out/bootstrap/test/argv.diff

# Test scheme-compiler: disptest
out/bootstrap/test/disptest.sasm: tests/disptest.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/disptest.scm --output out/bootstrap/test/disptest.sasm --debug

out/bootstrap/test/disptest.sasm-opt: out/bootstrap/test/disptest.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/disptest.sasm --out=out/bootstrap/test/disptest.sasm-opt --cheap

out/bootstrap/test/disptest.asm: out/bootstrap/test/disptest.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/disptest.sasm-opt --out=out/bootstrap/test/disptest.asm

out/bootstrap/test/disptest.o: out/bootstrap/test/disptest.asm
	nasm -felf32 out/bootstrap/test/disptest.asm -o out/bootstrap/test/disptest.o

out/bootstrap/test/disptest.exe: out/bootstrap/test/disptest.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/disptest.out: out/bootstrap/test/disptest.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/disptest.diff: out/bootstrap/test/disptest.out tests/baseline/disptest-s.actual
	diff --strip-trailing-cr out/bootstrap/test/disptest.out tests/baseline/disptest-s.actual
	touch out/bootstrap/test/disptest.diff

# Test scheme-compiler: disptest2
out/bootstrap/test/disptest2.sasm: tests/disptest2.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/disptest2.scm --output out/bootstrap/test/disptest2.sasm

out/bootstrap/test/disptest2.sasm-opt: out/bootstrap/test/disptest2.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/disptest2.sasm --out=out/bootstrap/test/disptest2.sasm-opt --cheap

out/bootstrap/test/disptest2.asm: out/bootstrap/test/disptest2.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/disptest2.sasm-opt --out=out/bootstrap/test/disptest2.asm

out/bootstrap/test/disptest2.o: out/bootstrap/test/disptest2.asm
	nasm -felf32 out/bootstrap/test/disptest2.asm -o out/bootstrap/test/disptest2.o

out/bootstrap/test/disptest2.exe: out/bootstrap/test/disptest2.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/disptest2.out: out/bootstrap/test/disptest2.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/disptest2.diff: out/bootstrap/test/disptest2.out tests/baseline/disptest2-s.actual
	diff --strip-trailing-cr out/bootstrap/test/disptest2.out tests/baseline/disptest2-s.actual
	touch out/bootstrap/test/disptest2.diff

# Test scheme-compiler: outputfile
out/bootstrap/test/outputfile.sasm: tests/outputfile.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/outputfile.scm --output out/bootstrap/test/outputfile.sasm

out/bootstrap/test/outputfile.sasm-opt: out/bootstrap/test/outputfile.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/outputfile.sasm --out=out/bootstrap/test/outputfile.sasm-opt --cheap

out/bootstrap/test/outputfile.asm: out/bootstrap/test/outputfile.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/outputfile.sasm-opt --out=out/bootstrap/test/outputfile.asm

out/bootstrap/test/outputfile.o: out/bootstrap/test/outputfile.asm
	nasm -felf32 out/bootstrap/test/outputfile.asm -o out/bootstrap/test/outputfile.o

out/bootstrap/test/outputfile.exe: out/bootstrap/test/outputfile.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/outputfile.out: out/bootstrap/test/outputfile.exe
	$< out/bootstrap/test/outputfile.dat>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/outputfile.diff: out/bootstrap/test/outputfile.out tests/baseline/outputfile-s.actual
	diff --strip-trailing-cr out/bootstrap/test/outputfile.out tests/baseline/outputfile-s.actual
	diff --strip-trailing-cr out/bootstrap/test/outputfile.dat tests/baseline/outputfile.dat
	touch out/bootstrap/test/outputfile.diff

# Test scheme-compiler: inputfile
out/bootstrap/test/inputfile.sasm: tests/inputfile.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/inputfile.scm --output out/bootstrap/test/inputfile.sasm

out/bootstrap/test/inputfile.sasm-opt: out/bootstrap/test/inputfile.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/inputfile.sasm --out=out/bootstrap/test/inputfile.sasm-opt --cheap

out/bootstrap/test/inputfile.asm: out/bootstrap/test/inputfile.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/inputfile.sasm-opt --out=out/bootstrap/test/inputfile.asm

out/bootstrap/test/inputfile.o: out/bootstrap/test/inputfile.asm
	nasm -felf32 out/bootstrap/test/inputfile.asm -o out/bootstrap/test/inputfile.o

out/bootstrap/test/inputfile.exe: out/bootstrap/test/inputfile.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/inputfile.out: out/bootstrap/test/inputfile.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/inputfile.diff: out/bootstrap/test/inputfile.out tests/baseline/inputfile-s.actual
	diff --strip-trailing-cr out/bootstrap/test/inputfile.out tests/baseline/inputfile-s.actual
	touch out/bootstrap/test/inputfile.diff

# Test scheme-compiler: read
out/bootstrap/test/read.sasm: tests/read.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read.scm --output out/bootstrap/test/read.sasm

out/bootstrap/test/read-expanded.scm: tests/read.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read.scm --output out/bootstrap/test/read-expanded.scm --expand-only

out/bootstrap/test/read.sasm-opt: out/bootstrap/test/read.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/read.sasm --out=out/bootstrap/test/read.sasm-opt --cheap

out/bootstrap/test/read.asm: out/bootstrap/test/read.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/read.sasm-opt --out=out/bootstrap/test/read.asm

out/bootstrap/test/read.o: out/bootstrap/test/read.asm
	nasm -felf32 out/bootstrap/test/read.asm -o out/bootstrap/test/read.o

out/bootstrap/test/read.exe: out/bootstrap/test/read.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/read.out: out/bootstrap/test/read.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/read.diff: out/bootstrap/test/read.out tests/baseline/read-s.actual
	diff --strip-trailing-cr out/bootstrap/test/read.out tests/baseline/read-s.actual
	touch out/bootstrap/test/read.diff

# Test scheme-compiler: read2
out/bootstrap/test/read2.sasm: tests/read2.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read2.scm --output out/bootstrap/test/read2.sasm

out/bootstrap/test/read2-expanded.scm: tests/read2.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read2.scm --output out/bootstrap/test/read2-expanded.scm --expand-only

out/bootstrap/test/read2.sasm-opt: out/bootstrap/test/read2.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/read2.sasm --out=out/bootstrap/test/read2.sasm-opt --cheap

out/bootstrap/test/read2.asm: out/bootstrap/test/read2.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/read2.sasm-opt --out=out/bootstrap/test/read2.asm

out/bootstrap/test/read2.o: out/bootstrap/test/read2.asm
	nasm -felf32 out/bootstrap/test/read2.asm -o out/bootstrap/test/read2.o

out/bootstrap/test/read2.exe: out/bootstrap/test/read2.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/read2.out: out/bootstrap/test/read2.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/read2.diff: out/bootstrap/test/read2.out tests/baseline/read2-s.actual
	diff --strip-trailing-cr out/bootstrap/test/read2.out tests/baseline/read2-s.actual
	touch out/bootstrap/test/read2.diff

# Test scheme-compiler: read3
out/bootstrap/test/read3.sasm: tests/read3.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read3.scm --output out/bootstrap/test/read3.sasm

out/bootstrap/test/read3-expanded.scm: tests/read3.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read3.scm --output out/bootstrap/test/read3-expanded.scm --expand-only

out/bootstrap/test/read3.sasm-opt: out/bootstrap/test/read3.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/read3.sasm --out=out/bootstrap/test/read3.sasm-opt --cheap

out/bootstrap/test/read3.asm: out/bootstrap/test/read3.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/read3.sasm-opt --out=out/bootstrap/test/read3.asm

out/bootstrap/test/read3.o: out/bootstrap/test/read3.asm
	nasm -felf32 out/bootstrap/test/read3.asm -o out/bootstrap/test/read3.o

out/bootstrap/test/read3.exe: out/bootstrap/test/read3.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/read3.out: out/bootstrap/test/read3.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/read3.diff: out/bootstrap/test/read3.out tests/baseline/read3-s.actual
	diff --strip-trailing-cr out/bootstrap/test/read3.out tests/baseline/read3-s.actual
	touch out/bootstrap/test/read3.diff


# Test scheme-compiler: peekchar
out/bootstrap/test/peekchar.sasm: tests/peekchar.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/peekchar.scm --output out/bootstrap/test/peekchar.sasm

out/bootstrap/test/peekchar-expanded.scm: tests/peekchar.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/peekchar.scm --output out/bootstrap/test/peekchar-expanded.scm --expand-only

out/bootstrap/test/peekchar.sasm-opt: out/bootstrap/test/peekchar.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/peekchar.sasm --out=out/bootstrap/test/peekchar.sasm-opt --cheap

out/bootstrap/test/peekchar.asm: out/bootstrap/test/peekchar.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/peekchar.sasm-opt --out=out/bootstrap/test/peekchar.asm

out/bootstrap/test/peekchar.o: out/bootstrap/test/peekchar.asm
	nasm -felf32 out/bootstrap/test/peekchar.asm -o out/bootstrap/test/peekchar.o

out/bootstrap/test/peekchar.exe: out/bootstrap/test/peekchar.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/peekchar.out: out/bootstrap/test/peekchar.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/peekchar.diff: out/bootstrap/test/peekchar.out tests/baseline/peekchar-s.actual
	diff --strip-trailing-cr out/bootstrap/test/peekchar.out tests/baseline/peekchar-s.actual
	touch out/bootstrap/test/peekchar.diff

# Test scheme-compiler: mkvec
out/bootstrap/test/mkvec.sasm: tests/mkvec.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/mkvec.scm --output out/bootstrap/test/mkvec.sasm

out/bootstrap/test/mkvec.sasm-opt: out/bootstrap/test/mkvec.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/mkvec.sasm --out=out/bootstrap/test/mkvec.sasm-opt --cheap

out/bootstrap/test/mkvec.asm: out/bootstrap/test/mkvec.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/mkvec.sasm-opt --out=out/bootstrap/test/mkvec.asm

out/bootstrap/test/mkvec.o: out/bootstrap/test/mkvec.asm
	nasm -felf32 out/bootstrap/test/mkvec.asm -o out/bootstrap/test/mkvec.o

out/bootstrap/test/mkvec.exe: out/bootstrap/test/mkvec.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/mkvec.out: out/bootstrap/test/mkvec.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/mkvec.diff: out/bootstrap/test/mkvec.out tests/baseline/mkvec-s.actual
	diff --strip-trailing-cr out/bootstrap/test/mkvec.out tests/baseline/mkvec-s.actual
	touch out/bootstrap/test/mkvec.diff

# Test scheme-compiler: eqv
out/bootstrap/test/eqv.sasm: tests/eqv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eqv.scm --output out/bootstrap/test/eqv.sasm

out/bootstrap/test/eqv-expanded.scm: tests/eqv.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eqv.scm --output out/bootstrap/test/eqv-expanded.scm --expand-only

out/bootstrap/test/eqv.sasm-opt: out/bootstrap/test/eqv.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/eqv.sasm --out=out/bootstrap/test/eqv.sasm-opt --cheap

out/bootstrap/test/eqv.asm: out/bootstrap/test/eqv.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/eqv.sasm-opt --out=out/bootstrap/test/eqv.asm

out/bootstrap/test/eqv.o: out/bootstrap/test/eqv.asm
	nasm -felf32 out/bootstrap/test/eqv.asm -o out/bootstrap/test/eqv.o

out/bootstrap/test/eqv.exe: out/bootstrap/test/eqv.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/eqv.out: out/bootstrap/test/eqv.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/eqv.diff: out/bootstrap/test/eqv.out tests/baseline/eqv-s.actual
	diff --strip-trailing-cr out/bootstrap/test/eqv.out tests/baseline/eqv-s.actual
	touch out/bootstrap/test/eqv.diff

# Test scheme-compiler: eq
out/bootstrap/test/eq.sasm: tests/eq.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eq.scm --output out/bootstrap/test/eq.sasm

out/bootstrap/test/eq-expanded.scm: tests/eq.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/eq.scm --output out/bootstrap/test/eq-expanded.scm --expand-only

out/bootstrap/test/eq.sasm-opt: out/bootstrap/test/eq.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/eq.sasm --out=out/bootstrap/test/eq.sasm-opt --cheap

out/bootstrap/test/eq.asm: out/bootstrap/test/eq.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/eq.sasm-opt --out=out/bootstrap/test/eq.asm

out/bootstrap/test/eq.o: out/bootstrap/test/eq.asm
	nasm -felf32 out/bootstrap/test/eq.asm -o out/bootstrap/test/eq.o

out/bootstrap/test/eq.exe: out/bootstrap/test/eq.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/eq.out: out/bootstrap/test/eq.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/eq.diff: out/bootstrap/test/eq.out tests/baseline/eq-s.actual
	diff --strip-trailing-cr out/bootstrap/test/eq.out tests/baseline/eq-s.actual
	touch out/bootstrap/test/eq.diff

# Test scheme-compiler: vararg
out/bootstrap/test/vararg.sasm: tests/vararg.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/vararg.scm --output out/bootstrap/test/vararg.sasm

out/bootstrap/test/vararg-expanded.scm: tests/vararg.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/vararg.scm --output out/bootstrap/test/vararg-expanded.scm --expand-only

out/bootstrap/test/vararg.sasm-opt: out/bootstrap/test/vararg.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/vararg.sasm --out=out/bootstrap/test/vararg.sasm-opt --cheap

out/bootstrap/test/vararg.asm: out/bootstrap/test/vararg.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/vararg.sasm-opt --out=out/bootstrap/test/vararg.asm

out/bootstrap/test/vararg.o: out/bootstrap/test/vararg.asm
	nasm -felf32 out/bootstrap/test/vararg.asm -o out/bootstrap/test/vararg.o

out/bootstrap/test/vararg.exe: out/bootstrap/test/vararg.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/vararg.out: out/bootstrap/test/vararg.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/vararg.diff: out/bootstrap/test/vararg.out tests/baseline/vararg-s.actual
	diff --strip-trailing-cr out/bootstrap/test/vararg.out tests/baseline/vararg-s.actual
	touch out/bootstrap/test/vararg.diff

# Test scheme-compiler: letrec
out/bootstrap/test/letrec.sasm: tests/letrec.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/letrec.scm --output out/bootstrap/test/letrec.sasm

out/bootstrap/test/letrec-expanded.scm: tests/letrec.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/letrec.scm --output out/bootstrap/test/letrec-expanded.scm --expand-only

out/bootstrap/test/letrec.sasm-opt: out/bootstrap/test/letrec.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/letrec.sasm --out=out/bootstrap/test/letrec.sasm-opt --cheap

out/bootstrap/test/letrec.asm: out/bootstrap/test/letrec.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/letrec.sasm-opt --out=out/bootstrap/test/letrec.asm

out/bootstrap/test/letrec.o: out/bootstrap/test/letrec.asm
	nasm -felf32 out/bootstrap/test/letrec.asm -o out/bootstrap/test/letrec.o

out/bootstrap/test/letrec.exe: out/bootstrap/test/letrec.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/letrec.out: out/bootstrap/test/letrec.exe
	$< >$@.tmp
	mv $@.tmp $@

# Test scheme-compiler: read4
out/bootstrap/test/read4.sasm: tests/read4.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read4.scm --output out/bootstrap/test/read4.sasm

out/bootstrap/test/read4-expanded.scm: tests/read4.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read4.scm --output out/bootstrap/test/read4-expanded.scm --expand-only

out/bootstrap/test/read4.sasm-opt: out/bootstrap/test/read4.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/read4.sasm --out=out/bootstrap/test/read4.sasm-opt --cheap

out/bootstrap/test/read4.asm: out/bootstrap/test/read4.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/read4.sasm-opt --out=out/bootstrap/test/read4.asm

out/bootstrap/test/read4.o: out/bootstrap/test/read4.asm
	nasm -felf32 out/bootstrap/test/read4.asm -o out/bootstrap/test/read4.o

out/bootstrap/test/read4.exe: out/bootstrap/test/read4.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/read4.out: out/bootstrap/test/read4.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/read4.diff: out/bootstrap/test/read4.out tests/baseline/read4-s.actual
	diff --strip-trailing-cr out/bootstrap/test/read4.out tests/baseline/read4-s.actual
	touch out/bootstrap/test/read4.diff

# Test scheme-compiler: read5
out/bootstrap/test/read5.sasm: tests/read5.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read5.scm --output out/bootstrap/test/read5.sasm

out/bootstrap/test/read5-expanded.scm: tests/read5.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/read5.scm --output out/bootstrap/test/read5-expanded.scm --expand-only

out/bootstrap/test/read5.sasm-opt: out/bootstrap/test/read5.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/read5.sasm --out=out/bootstrap/test/read5.sasm-opt --cheap

out/bootstrap/test/read5.asm: out/bootstrap/test/read5.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/read5.sasm-opt --out=out/bootstrap/test/read5.asm

out/bootstrap/test/read5.o: out/bootstrap/test/read5.asm
	nasm -felf32 out/bootstrap/test/read5.asm -o out/bootstrap/test/read5.o

out/bootstrap/test/read5.exe: out/bootstrap/test/read5.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/read5.out: out/bootstrap/test/read5.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/read5.diff: out/bootstrap/test/read5.out tests/baseline/read5-s.actual
	diff --strip-trailing-cr out/bootstrap/test/read5.out tests/baseline/read5-s.actual
	touch out/bootstrap/test/read5.diff

# Test scheme-compiler: sym1
out/bootstrap/test/sym1.sasm: tests/sym1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/sym1.scm --output out/bootstrap/test/sym1.sasm

out/bootstrap/test/sym1-expanded.scm: tests/sym1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/sym1.scm --output out/bootstrap/test/sym1-expanded.scm --expand-only

out/bootstrap/test/sym1.sasm-opt: out/bootstrap/test/sym1.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/sym1.sasm --out=out/bootstrap/test/sym1.sasm-opt --cheap

out/bootstrap/test/sym1.asm: out/bootstrap/test/sym1.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/sym1.sasm-opt --out=out/bootstrap/test/sym1.asm

out/bootstrap/test/sym1.o: out/bootstrap/test/sym1.asm
	nasm -felf32 out/bootstrap/test/sym1.asm -o out/bootstrap/test/sym1.o

out/bootstrap/test/sym1.exe: out/bootstrap/test/sym1.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/sym1.out: out/bootstrap/test/sym1.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/sym1.diff: out/bootstrap/test/sym1.out tests/baseline/sym1-s.actual
	diff --strip-trailing-cr out/bootstrap/test/sym1.out tests/baseline/sym1-s.actual
	touch out/bootstrap/test/sym1.diff

# Test scheme-compiler: badapply
out/bootstrap/test/badapply.sasm: tests/badapply.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/badapply.scm --output out/bootstrap/test/badapply.sasm

out/bootstrap/test/badapply-expanded.scm: tests/badapply.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/badapply.scm --output out/bootstrap/test/badapply-expanded.scm --expand-only

out/bootstrap/test/badapply.sasm-opt: out/bootstrap/test/badapply.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/badapply.sasm --out=out/bootstrap/test/badapply.sasm-opt --cheap

out/bootstrap/test/badapply.asm: out/bootstrap/test/badapply.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/badapply.sasm-opt --out=out/bootstrap/test/badapply.asm

out/bootstrap/test/badapply.o: out/bootstrap/test/badapply.asm
	nasm -felf32 out/bootstrap/test/badapply.asm -o out/bootstrap/test/badapply.o

out/bootstrap/test/badapply.exe: out/bootstrap/test/badapply.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/badapply.out: out/bootstrap/test/badapply.exe tests/badapply.cmd
	./tests/badapply.cmd>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badapply.diff: out/bootstrap/test/badapply.out tests/baseline/badapply-s.actual
	diff --strip-trailing-cr out/bootstrap/test/badapply.out tests/baseline/badapply-s.actual
	touch out/bootstrap/test/badapply.diff

out/bootstrap/test/badvrhi.out: out/bootstrap/test/badvrhi.exe tests/badvrhi.sh
	./tests/badvrhi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badvrlo.out: out/bootstrap/test/badvrlo.exe tests/badvrlo.sh
	./tests/badvrlo.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badvshi.out: out/bootstrap/test/badvshi.exe tests/badvshi.sh
	./tests/badvshi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badvslo.out: out/bootstrap/test/badvslo.exe tests/badvslo.sh
	./tests/badvslo.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badsrhi.out: out/bootstrap/test/badsrhi.exe tests/badsrhi.sh
	./tests/badsrhi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badsrlo.out: out/bootstrap/test/badsrlo.exe tests/badsrlo.sh
	./tests/badsrlo.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badsshi.out: out/bootstrap/test/badsshi.exe tests/badsshi.sh
	./tests/badsshi.sh>$@.tmp
	mv $@.tmp $@

out/bootstrap/test/badsslo.out: out/bootstrap/test/badsslo.exe tests/badsslo.sh
	./tests/badsslo.sh>$@.tmp
	mv $@.tmp $@


# Test scheme-compiler: gc1
out/bootstrap/test/tests-printer.sasm: tests/printer.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/printer.scm --outdir out/bootstrap/test --conspiracy --no-entry

out/bootstrap/test/tests-printer-helper.sasm: tests/printer-helper.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR) out/bootstrap/test/tests-printer.sasm
	$(SCHEMEC) tests/printer-helper.scm --outdir out/bootstrap/test --conspiracy --no-entry

out/bootstrap/test/gc1.sasm: tests/gc1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR) out/bootstrap/test/tests-printer-helper.sasm out/bootstrap/test/tests-printer.sasm
	$(SCHEMEC) tests/gc1.scm --outdir out/bootstrap/test --conspiracy
	cp out/bootstrap/test/tests-gc1.sasm out/bootstrap/test/gc1.sasm

out/bootstrap/test/gc1-expanded.scm: tests/gc1.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) tests/gc1.scm --output out/bootstrap/test/gc1-expanded.scm --expand-only

out/bootstrap/test/gc1.sasm-opt: out/bootstrap/test/gc1.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/gc1.sasm --out=out/bootstrap/test/gc1.sasm-opt --cheap

out/bootstrap/test/tests-printer.sasm-opt: out/bootstrap/test/tests-printer.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/tests-printer.sasm --out=out/bootstrap/test/tests-printer.sasm-opt --cheap

out/bootstrap/test/tests-printer-helper.sasm-opt: out/bootstrap/test/tests-printer-helper.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) out/bootstrap/test/tests-printer-helper.sasm --out=out/bootstrap/test/tests-printer-helper.sasm-opt --cheap

out/bootstrap/test/gc1.asm: out/bootstrap/test/gc1.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/gc1.sasm-opt --out=out/bootstrap/test/gc1.asm

out/bootstrap/test/tests-printer.asm: out/bootstrap/test/tests-printer.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/tests-printer.sasm-opt --out=out/bootstrap/test/tests-printer.asm

out/bootstrap/test/tests-printer-helper.asm: out/bootstrap/test/tests-printer-helper.sasm-opt $(DEPEND_SASMC)
	$(SASMC) out/bootstrap/test/tests-printer-helper.sasm-opt --out=out/bootstrap/test/tests-printer-helper.asm

out/bootstrap/test/gc1.o: out/bootstrap/test/gc1.asm
	nasm -felf32 out/bootstrap/test/gc1.asm -o out/bootstrap/test/gc1.o

out/bootstrap/test/tests-printer.o: out/bootstrap/test/tests-printer.asm
	nasm -felf32 out/bootstrap/test/tests-printer.asm -o out/bootstrap/test/tests-printer.o

out/bootstrap/test/tests-printer-helper.o: out/bootstrap/test/tests-printer-helper.asm
	nasm -felf32 out/bootstrap/test/tests-printer-helper.asm -o out/bootstrap/test/tests-printer-helper.o

out/bootstrap/test/gc1.exe: out/bootstrap/test/gc1.o out/bootstrap/test/tests-printer.o out/bootstrap/test/tests-printer-helper.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) $< out/bootstrap/test/tests-printer.o out/bootstrap/test/tests-printer-helper.o -o $@

out/bootstrap/test/gc1.out: out/bootstrap/test/gc1.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/r5rs3.out: out/bootstrap/test/r5rs3.exe
	$< out/bootstrap/test/r5rs3-cwof.dat>$@.tmp
	mv $@.tmp $@

# Test scheme-compiler: general case
out/bootstrap/test/%.sasm: tests/%.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) $< --output $@

out/bootstrap/test/%-expanded.scm: tests/%.scm $(DEPEND_SCHEMEC) $(BOOTSTRAP_TEST_DIR)
	$(SCHEMEC) $< --output $@ --expand-only

out/bootstrap/test/%.sasm-opt: out/bootstrap/test/%.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

out/bootstrap/test/%.asm: out/bootstrap/test/%.sasm-opt $(DEPEND_SASMC)
	$(SASMC) $< --out=$@

out/bootstrap/test/%.o: out/bootstrap/test/%.asm
	nasm -felf32 $< -o $@

out/bootstrap/test/%.exe: out/bootstrap/test/%.o $(DEPEND_RTL) $(DEPEND_SCHEME_RTL)
	gcc $(SCHEME_CFLAGS) -Irtl $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL) $< -o $@

out/bootstrap/test/%.out: out/bootstrap/test/%.exe
	$< >$@.tmp
	mv $@.tmp $@

out/bootstrap/test/%.diff: out/bootstrap/test/%.out tests/baseline/%-s.actual
	diff --strip-trailing-cr $< $(patsubst out/bootstrap/test/%.out,tests/baseline/%-s.actual,$<)
	touch $@



# Compile scheme RTL
out/bootstrap/r5rs-library.sasm: rtl/r5rs-library.scm $(DEPEND_SCHEMEC)
	$(SCHEMEC) rtl/r5rs-library.scm --outdir out/bootstrap --conspiracy --no-entry
	cp out/bootstrap/rtl-r5rs-library.sasm out/bootstrap/r5rs-library.sasm

out/bootstrap/r5rs-library.sasm-opt : out/bootstrap/r5rs-library.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

out/bootstrap/r5rs-wrap.sasm: rtl/r5rs-wrap.scm $(DEPEND_SCHEMEC)
	$(SCHEMEC) rtl/r5rs-wrap.scm --outdir out/bootstrap --conspiracy --no-entry
	cp out/bootstrap/rtl-r5rs-wrap.sasm out/bootstrap/r5rs-wrap.sasm

out/bootstrap/r5rs-wrap.sasm-opt : out/bootstrap/r5rs-wrap.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

out/bootstrap/r5rs-native.sasm : rtl/r5rs-native.scm $(DEPEND_GLUEC) $(BOOTSTRAP_DIR)
	$(GLUEC) rtl/r5rs-native.scm -o out/bootstrap/r5rs-native.sasm

out/bootstrap/r5rs-native.sasm-opt: out/bootstrap/r5rs-native.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@

# sasm-sasm-nasmx86 override, disable-optimizations

out/bootstrap/sasm-sasm-nasmx86.sasm-opt: out/bootstrap/sasm-sasm-nasmx86.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@ --cheap

# rtl rules
out/bootstrap/%.o: out/bootstrap/%.asm
	nasm -felf32 $< -o $@

out/bootstrap/debug.asm: rtl/debug.asm
	cp rtl/debug.asm out/bootstrap/debug.asm

out/bootstrap/%.asm: out/bootstrap/%.sasm-opt $(DEPEND_SASMC)
	$(SASMC) $< --out=$@

# bootstrap sasm tool
out/bootstrap-sasm-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm)
	$(SCHEME) needc-ts.scm --script-mode --output out/bootstrap-sasm-ts.sh sasm

out/sasm-bootstrap.out: out/bootstrap-sasm-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-sasm-ts.sh
	./out/bootstrap-sasm-ts.sh
	touch out/sasm-bootstrap.out

out/bootstrap/sasm.exe: $(DEPEND_RTL) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/sasm-bootstrap.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/bootstrap $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/bootstrap/sasm.o -o $@ -lsasm


# bootstrap sasm-opt tool
out/bootstrap-sasm-opt-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_sasm_opt)
	$(SCHEME) needc-ts.scm --script-mode --output out/bootstrap-sasm-opt-ts.sh sasm-opt

out/sasm-opt-bootstrap.out: out/bootstrap-sasm-opt-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-sasm-opt-ts.sh
	./out/bootstrap-sasm-opt-ts.sh
	touch out/sasm-opt-bootstrap.out

out/bootstrap/sasm-opt.exe: $(DEPEND_RTL) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/sasm-opt-bootstrap.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/bootstrap $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/bootstrap/sasm-opt.o -o $@ -lsasm-opt


# bootstrap schemec
out/bootstrap-scheme-compiler-ts.sh : $(OUT_DIR) $(DEPEND_NEEDC) $(deps_of_scheme_compiler)
	$(SCHEME) needc-ts.scm --script-mode --output out/bootstrap-scheme-compiler-ts.sh scheme-compiler

out/scheme-compiler-bootstrap.out: out/bootstrap-scheme-compiler-ts.sh $(DEPEND_SCHEMEC) $(BOOTSTRAP_DIR)
	chmod +x ./out/bootstrap-scheme-compiler-ts.sh
	./out/bootstrap-scheme-compiler-ts.sh
	touch out/scheme-compiler-bootstrap.out

out/bootstrap/schemec.exe: $(DEPEND_RTL) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/scheme-compiler-bootstrap.out
	gcc $(SCHEME_CFLAGS) -Irtl -Lout/bootstrap $(DEPEND_RTL_C) $(DEPEND_RTL_OBJS) $(DEPEND_SCHEME_RTL_OMIT_MAIN) out/bootstrap/scheme-compiler.o -o $@ -lscheme-compiler

# RTL rules which have to go under specific rules above
out/bootstrap/%.sasm-opt: rtl/%.sasm $(DEPEND_SASMOPT)
	$(SASMOPT) $< --out=$@

