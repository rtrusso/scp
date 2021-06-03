if [ ! -f ../bootstrap/tinyscheme-1.41/scheme.exe ]; then
    echo Unable to find ../bootstrap/tinyscheme-1.41/scheme.exe
    echo Run 'make' in ../bootstrap/tinyscheme-1.41 first, as a one-time step
    exit 1;
    fi

. ./env.sh
echo SCHEME=$SCHEME
if [ $? -ne 0 ]; then exit $?; fi

echo ==========================================================================
echo == Generating .dep files
echo ==========================================================================

export NEEDC="$SCHEME needc-ts.scm"
$NEEDC --makefile-mode --output sasm.dep sasm
if [ $? -ne 0 ]; then exit $?; fi
$NEEDC --makefile-mode --output sasm-opt.dep sasm-opt
if [ $? -ne 0 ]; then exit $?; fi
$NEEDC --makefile-mode --output scheme-compiler.dep scheme-compiler
if [ $? -ne 0 ]; then exit $?; fi
$NEEDC --makefile-mode --output java-compiler.dep java-compiler
if [ $? -ne 0 ]; then exit $?; fi

echo ==========================================================================
echo == Scheme and SASM Tool Bootstrap
echo ==========================================================================

make -f bootstrap.linux.makefile out/bootstrap/test/disptest.diff out/bootstrap/test/disptest2.diff out/bootstrap/test/disp1.diff out/bootstrap/test/disp2.diff out/bootstrap/test/n2s.diff out/bootstrap/test/n2s2.diff out/bootstrap/test/quot2.diff out/bootstrap/test/opdiv.diff out/bootstrap/test/java/tests.done out/bootstrap/test/java/gc/tests.done out/bootstrap/test/schemec-tests.done
#make -f bootstrap.linux.makefile out/bootstrap/test/java/tests.done out/bootstrap/test/java/gc/tests.done out/bootstrap/test/schemec-tests.done
#make -f bootstrap.linux.makefile out/bootstrap/test/n2s.diff
if [ $? -ne 0 ]; then exit $?; fi
if [ ! -f out/bootstrap/sasm.exe ]; then exit 1; fi
if [ ! -f out/bootstrap/sasm-opt.exe ]; then exit 1; fi
if [ ! -f out/bootstrap/schemec.exe ]; then exit 1; fi

echo ==========================================================================
echo == Scheme and SASM Self-Build
echo ==========================================================================

make -f selfbld.linux.makefile

