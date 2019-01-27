@setlocal
@echo off
out\bootstrap\test\badsshi.exe 2>out\bootstrap\test\badsshi-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badsshi-stderr.out
echo.
echo exit=%status%
