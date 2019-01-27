@setlocal
@echo off
out\bootstrap\test\badsslo.exe 2>out\bootstrap\test\badsslo-stderr.out
set status=%errorlevel%
type out\bootstrap\test\badsslo-stderr.out
echo.
echo exit=%status%
