gcc -pg -DSTANDALONE=0 -DUSE_STRLWR=0 -DTS_BOOTSTRAP_MINGW32 -DUSE_DL=0 -DUSE_MATH=1 -DUSE_ASCII_NAMES=0 scheme.c test.c -lm -o test.exe
if not "%errorlevel%"=="0" exit /b 1
test.exe
set status=%errorlevel%
echo test.exe terminated with status %status% (%errorlevel%)
