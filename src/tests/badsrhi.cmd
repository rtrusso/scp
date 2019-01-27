@setlocal
@echo off
out\bootstrap\test\badsrhi.exe 2>out\bootstrap\test\badsrhi-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badsrhi-stderr.out
echo.
echo exit=%status%
