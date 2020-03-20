if [ ! -f ../bootstrap/tinyscheme-1.41/scheme.exe ]; then
    echo Unable to find ../bootstrap/tinyscheme-1.41/scheme.exe
    echo Run 'make' in ../bootstrap/tinyscheme-1.41 first, as a one-time step
    exit 1;
    fi

. ./env-x64.sh
echo SCHEME=$SCHEME
if [ $? -ne 0 ]; then exit $?; fi

echo ==========================================================================
echo == Generating .dep files
echo ==========================================================================

export NEEDC="$SCHEME needc-ts.scm"
$NEEDC --makefile-mode --output sasm-x64.dep sasm-x64
if [ $? -ne 0 ]; then exit $?; fi
$NEEDC --makefile-mode --output sasm-opt-x64.dep sasm-opt-x64
if [ $? -ne 0 ]; then exit $?; fi
$NEEDC --makefile-mode --output scheme-compiler.dep scheme-compiler
if [ $? -ne 0 ]; then exit $?; fi
$NEEDC --makefile-mode --output java-compiler.dep java-compiler
if [ $? -ne 0 ]; then exit $?; fi

echo ==========================================================================
echo == Scheme and SASM Tool Bootstrap
echo ==========================================================================

make -f bootstrap.linux-x64.makefile out/bootstrap-x64/test/call.exe
if [ $? -ne 0 ]; then exit $?; fi
if [ ! -f out/bootstrap-x64/sasm-x64.exe ]; then exit 1; fi
if [ ! -f out/bootstrap-x64/sasm-opt-x64.exe ]; then exit 1; fi
if [ ! -f out/bootstrap-x64/schemec.exe ]; then exit 1; fi

echo ==========================================================================
echo == Scheme and SASM Self-Build
echo ==========================================================================

make -f selfbld.linux-x64.makefile

