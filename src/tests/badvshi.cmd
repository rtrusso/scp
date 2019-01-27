@setlocal
@echo off
out\bootstrap\test\badvshi.exe 2>out\bootstrap\test\badvshi-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badvshi-stderr.out
echo.
echo exit=%status%
