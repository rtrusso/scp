@setlocal
@echo off
out\bootstrap\test\badvrlo.exe 2>out\bootstrap\test\badvrlo-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badvrlo-stderr.out
echo.
echo exit=%status%
