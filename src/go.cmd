@setlocal
@echo off

call env.cmd
if not exist ..\bootstrap\tinyscheme-1.41\scheme.exe goto :missing

echo ##########################################################################
echo ## Generating .dep files
echo ##########################################################################

rem generate dependencies
set NEEDC=%scheme% needc-ts.scm
%NEEDC% --makefile-mode --windows-mode --output sasm.dep sasm
%NEEDC% --makefile-mode --windows-mode --output sasm-opt.dep sasm-opt
%NEEDC% --makefile-mode --windows-mode --output scheme-compiler.dep scheme-compiler
%NEEDC% --makefile-mode --windows-mode --output java-compiler.dep java-compiler

echo ##########################################################################
echo ## Scheme and SASM Tool Bootstrap
echo ##########################################################################

REM call make -f bootstrap.win32.makefile out/bootstrap/test/java/tests.done out/bootstrap/test/java/gc/tests.done
REM call make -f bootstrap.win32.makefile out/bootstrap/test/java/CtorTest.diff
call make -f bootstrap.win32.makefile out/bootstrap/test/java/tests.done
if not exist out\bootstrap\sasm.exe exit /b 1
if not exist out\bootstrap\sasm-opt.exe exit /b 1
if not exist out\bootstrap\schemec.exe exit /b 1

echo ##########################################################################
echo ## Scheme and SASM Self-Build
echo ##########################################################################

call make -f selfbld.win32.makefile
exit /b %errorlevel%

:missing
echo Unable to find ..\bootstrap\tinyscheme-1.41\scheme.exe
echo Run 'make' in ..\bootstrap\tinyscheme-1.41 first, as a one-time step
exit /b 1
