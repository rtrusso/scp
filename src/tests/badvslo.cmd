@setlocal
@echo off
out\bootstrap\test\badvslo.exe 2>out\bootstrap\test\badvslo-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badvslo-stderr.out
echo.
echo exit=%status%
