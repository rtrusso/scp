ifndef TEST_OUTDIR_NAME
$(error TEST_OUTDIR_NAME not defined, run go.sh or env.sh)
endif

ifndef TEST_SCHEMEC
$(error TEST_SCHEMEC not defined, run go.sh or env.sh)
endif

ifndef TEST_SASMOPT
$(error TEST_SASMOPT not defined, run go.sh or env.sh)
endif

ifndef TEST_SASMC
$(error TEST_SASMC not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_SCHEMEC
$(error DEPEND_TEST_SCHEMEC not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_SASMOPT
$(error DEPEND_TEST_SASMOPT not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_SASMC
$(error DEPEND_TEST_SASMC not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_OUTPUT_DIR
$(error DEPEND_TEST_OUTPUT_DIR not defined, run go.sh or env.sh)
endif

ifndef TEST_SCHEME_CFLAGS
$(error TEST_SCHEME_CFLAGS not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_RTL
$(error DEPEND_TEST_RTL not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_RTL_C
$(error DEPEND_TEST_RTL_C not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_SCHEME_RTL
$(error DEPEND_TEST_SCHEME_RTL not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_SCHEME_RTL_OMIT_MAIN
$(error DEPEND_TEST_SCHEME_RTL_OMIT_MAIN not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_RTL
$(error DEPEND_TEST_RTL not defined, run go.sh or env.sh)
endif

ifndef DEPEND_TEST_RTL_OBJS
$(error DEPEND_TEST_RTL_OBJS not defined, run go.sh or env.sh)
endif


out/$(TEST_OUTDIR_NAME)/test/getenv.out: out/$(TEST_OUTDIR_NAME)/test/getenv.exe
	export SCHEME_TEST=SCHEME_TEST;$< >$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/delete.out: out/$(TEST_OUTDIR_NAME)/test/delete.exe
	touch deleteme.file
	$< >$@.tmp
	if [ -f deleteme.file ]; then exit 128; fi
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/rename.out: out/$(TEST_OUTDIR_NAME)/test/rename.exe
	touch from.file
	$< >$@.tmp
	if [ -f from.file ]; then exit 128; fi
	if [ ! -f to.file ]; then exit 128; fi
	rm to.file
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/argv.out: out/$(TEST_OUTDIR_NAME)/test/argv.exe
	$< 1 2 3 4 five six 7 8 9 ten>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/outputfile.out: out/$(TEST_OUTDIR_NAME)/test/outputfile.exe
	$< out/$(TEST_OUTDIR_NAME)/test/outputfile.dat>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/outputfile.diff: out/$(TEST_OUTDIR_NAME)/test/outputfile.out tests/baseline/outputfile-s.actual
	diff --strip-trailing-cr out/$(TEST_OUTDIR_NAME)/test/outputfile.out tests/baseline/outputfile-s.actual
	diff --strip-trailing-cr out/$(TEST_OUTDIR_NAME)/test/outputfile.dat tests/baseline/outputfile.dat
	touch out/$(TEST_OUTDIR_NAME)/test/outputfile.diff

out/$(TEST_OUTDIR_NAME)/test/r5rs3.out: out/$(TEST_OUTDIR_NAME)/test/r5rs3.exe
	$< out/$(TEST_OUTDIR_NAME)/test/r5rs3-cwof.dat>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badapply.out: out/$(TEST_OUTDIR_NAME)/test/badapply.exe tests/badapply.sh
	./tests/badapply.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badapply.diff: out/$(TEST_OUTDIR_NAME)/test/badapply.out tests/baseline/badapply-s-linux.actual
	diff --strip-trailing-cr out/$(TEST_OUTDIR_NAME)/test/badapply.out tests/baseline/badapply-s-linux.actual
	touch out/$(TEST_OUTDIR_NAME)/test/badapply.diff

out/$(TEST_OUTDIR_NAME)/test/badvrhi.out: out/$(TEST_OUTDIR_NAME)/test/badvrhi.exe tests/badvrhi.sh
	./tests/badvrhi.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badvrlo.out: out/$(TEST_OUTDIR_NAME)/test/badvrlo.exe tests/badvrlo.sh
	./tests/badvrlo.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badvshi.out: out/$(TEST_OUTDIR_NAME)/test/badvshi.exe tests/badvshi.sh
	./tests/badvshi.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badvshi.diff: out/$(TEST_OUTDIR_NAME)/test/badvshi.out tests/baseline/badvshi-s-linux.actual
	diff --strip-trailing-cr out/$(TEST_OUTDIR_NAME)/test/badvshi.out tests/baseline/badvshi-s-linux.actual
	touch out/$(TEST_OUTDIR_NAME)/test/badvshi.diff

out/$(TEST_OUTDIR_NAME)/test/badvslo.out: out/$(TEST_OUTDIR_NAME)/test/badvslo.exe tests/badvslo.sh
	./tests/badvslo.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badvslo.diff: out/$(TEST_OUTDIR_NAME)/test/badvslo.out tests/baseline/badvslo-s-linux.actual
	diff --strip-trailing-cr out/$(TEST_OUTDIR_NAME)/test/badvslo.out tests/baseline/badvslo-s-linux.actual
	touch out/$(TEST_OUTDIR_NAME)/test/badvslo.diff

out/$(TEST_OUTDIR_NAME)/test/badsrhi.out: out/$(TEST_OUTDIR_NAME)/test/badsrhi.exe tests/badsrhi.sh
	./tests/badsrhi.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badsrlo.out: out/$(TEST_OUTDIR_NAME)/test/badsrlo.exe tests/badsrlo.sh
	./tests/badsrlo.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badsshi.out: out/$(TEST_OUTDIR_NAME)/test/badsshi.exe tests/badsshi.sh
	./tests/badsshi.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badsshi.diff: out/$(TEST_OUTDIR_NAME)/test/badsshi.out tests/baseline/badsshi-s-linux.actual
	diff --strip-trailing-cr out/$(TEST_OUTDIR_NAME)/test/badsshi.out tests/baseline/badsshi-s-linux.actual
	touch out/$(TEST_OUTDIR_NAME)/test/badsshi.diff

out/$(TEST_OUTDIR_NAME)/test/badsslo.out: out/$(TEST_OUTDIR_NAME)/test/badsslo.exe tests/badsslo.sh
	./tests/badsslo.sh>$@.tmp
	mv $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/badsslo.diff: out/$(TEST_OUTDIR_NAME)/test/badsslo.out tests/baseline/badsslo-s-linux.actual
	diff --strip-trailing-cr out/$(TEST_OUTDIR_NAME)/test/badsslo.out tests/baseline/badsslo-s-linux.actual
	touch out/$(TEST_OUTDIR_NAME)/test/badsslo.diff

# Test scheme-compiler: gc1
out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm: tests/printer.scm $(DEPEND_TEST_SCHEMEC) $(DEPEND_TEST_OUTPUT_DIR)
	$(TEST_SCHEMEC) tests/printer.scm --outdir out/$(TEST_OUTDIR_NAME)/test --conspiracy --no-entry

out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm: tests/printer-helper.scm $(DEPEND_TEST_SCHEMEC) $(DEPEND_TEST_OUTPUT_DIR) out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm
	$(TEST_SCHEMEC) tests/printer-helper.scm --outdir out/$(TEST_OUTDIR_NAME)/test --conspiracy --no-entry

out/$(TEST_OUTDIR_NAME)/test/gc1.sasm: tests/gc1.scm $(DEPEND_TEST_SCHEMEC) $(DEPEND_TEST_OUTPUT_DIR) out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm
	$(TEST_SCHEMEC) tests/gc1.scm --outdir out/$(TEST_OUTDIR_NAME)/test --conspiracy
	cp out/$(TEST_OUTDIR_NAME)/test/tests-gc1.sasm $@

out/$(TEST_OUTDIR_NAME)/test/gc1-expanded.scm: tests/gc1.scm $(DEPEND_TEST_SCHEMEC) $(DEPEND_TEST_OUTPUT_DIR)
	$(TEST_SCHEMEC) tests/gc1.scm --output out/$(TEST_OUTDIR_NAME)/test/gc1-expanded.scm --expand-only

out/$(TEST_OUTDIR_NAME)/test/gc1.sasm-opt: out/$(TEST_OUTDIR_NAME)/test/gc1.sasm $(DEPEND_TEST_SASMOPT)
	$(TEST_SASMOPT) out/$(TEST_OUTDIR_NAME)/test/gc1.sasm --out=out/$(TEST_OUTDIR_NAME)/test/gc1.sasm-opt --cheap

out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm-opt: out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm $(DEPEND_TEST_SASMOPT)
	$(TEST_SASMOPT) out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm --out=out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm-opt --cheap

out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm-opt: out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm $(DEPEND_TEST_SASMOPT)
	$(TEST_SASMOPT) out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm --out=out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm-opt --cheap

out/$(TEST_OUTDIR_NAME)/test/gc1.asm: out/$(TEST_OUTDIR_NAME)/test/gc1.sasm-opt $(DEPEND_TEST_SASMC)
	$(TEST_SASMC) out/$(TEST_OUTDIR_NAME)/test/gc1.sasm-opt --out=out/$(TEST_OUTDIR_NAME)/test/gc1.asm

out/$(TEST_OUTDIR_NAME)/test/tests-printer.asm: out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm-opt $(DEPEND_TEST_SASMC)
	$(TEST_SASMC) out/$(TEST_OUTDIR_NAME)/test/tests-printer.sasm-opt --out=out/$(TEST_OUTDIR_NAME)/test/tests-printer.asm

out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.asm: out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm-opt $(DEPEND_TEST_SASMC)
	$(TEST_SASMC) out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.sasm-opt --out=out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.asm

out/$(TEST_OUTDIR_NAME)/test/gc1.o: out/$(TEST_OUTDIR_NAME)/test/gc1.asm
	nasm -fwin32 out/$(TEST_OUTDIR_NAME)/test/gc1.asm -o out/$(TEST_OUTDIR_NAME)/test/gc1.o

out/$(TEST_OUTDIR_NAME)/test/tests-printer.o: out/$(TEST_OUTDIR_NAME)/test/tests-printer.asm
	nasm -fwin32 out/$(TEST_OUTDIR_NAME)/test/tests-printer.asm -o out/$(TEST_OUTDIR_NAME)/test/tests-printer.o

out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.o: out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.asm
	nasm -fwin32 out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.asm -o out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.o

out/$(TEST_OUTDIR_NAME)/test/gc1.exe: out/$(TEST_OUTDIR_NAME)/test/gc1.o out/$(TEST_OUTDIR_NAME)/test/tests-printer.o out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.o $(DEPEND_TEST_RTL) $(DEPEND_TEST_SCHEME_RTL)
	gcc $(TEST_SCHEME_CFLAGS) -Irtl $(DEPEND_TEST_RTL_C) $(DEPEND_TEST_RTL_OBJS) $(DEPEND_TEST_SCHEME_RTL_OMIT_MAIN) $< out/$(TEST_OUTDIR_NAME)/test/tests-printer.o out/$(TEST_OUTDIR_NAME)/test/tests-printer-helper.o -o $@


# Test scheme-compiler: general case
out/$(TEST_OUTDIR_NAME)/test/%.sasm: tests/%.scm $(DEPEND_TEST_SCHEMEC) $(DEPEND_TEST_OUTPUT_DIR)
	$(TEST_SCHEMEC) $< --output $@

out/$(TEST_OUTDIR_NAME)/test/%-expanded.scm: tests/%.scm $(DEPEND_TEST_SCHEMEC) $(DEPEND_TEST_OUTPUT_DIR)
	$(TEST_SCHEMEC) $< --output $@ --expand-only

out/$(TEST_OUTDIR_NAME)/test/%.sasm-opt: out/$(TEST_OUTDIR_NAME)/test/%.sasm $(DEPEND_TEST_SASMOPT)
	$(TEST_SASMOPT) $< --out=$@ --cheap

out/$(TEST_OUTDIR_NAME)/test/%.asm: out/$(TEST_OUTDIR_NAME)/test/%.sasm-opt $(DEPEND_TEST_SASMC)
	$(TEST_SASMC) $< --out=$@

out/$(TEST_OUTDIR_NAME)/test/%.o: out/$(TEST_OUTDIR_NAME)/test/%.asm
	nasm -fwin32 $< -o $@

out/$(TEST_OUTDIR_NAME)/test/%.exe: out/$(TEST_OUTDIR_NAME)/test/%.o $(DEPEND_TEST_RTL) $(DEPEND_TEST_SCHEME_RTL)
	gcc $(TEST_SCHEME_CFLAGS) -Irtl $(DEPEND_TEST_RTL_C) $(DEPEND_TEST_RTL_OBJS) $(DEPEND_TEST_SCHEME_RTL) $< -o $@

out/$(TEST_OUTDIR_NAME)/test/%.out: out/$(TEST_OUTDIR_NAME)/test/%.exe
	$< >$@.tmp
	cp $@.tmp $@

out/$(TEST_OUTDIR_NAME)/test/%.diff: out/$(TEST_OUTDIR_NAME)/test/%.out tests/baseline/%-s.actual
	diff --strip-trailing-cr $< $(patsubst out/$(TEST_OUTDIR_NAME)/test/%.out,tests/baseline/%-s.actual,$<)
	touch $@



