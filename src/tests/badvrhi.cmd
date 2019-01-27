@setlocal
@echo off
out\bootstrap\test\badvrhi.exe 2>out\bootstrap\test\badvrhi-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badvrhi-stderr.out
echo.
echo exit=%status%
