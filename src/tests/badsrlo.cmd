@setlocal
@echo off
out\bootstrap\test\badsrlo.exe 2>out\bootstrap\test\badsrlo-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badsrlo-stderr.out
echo.
echo exit=%status%
