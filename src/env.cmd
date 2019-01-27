rem configure paths to TinyScheme interpreter as bootstrap
set TINYSCHEMEINIT=%~dp0..\bootstrap\tinyscheme-1.41\init.scm
set SCHEME=%~dp0..\bootstrap\tinyscheme-1.41\scheme.exe -1

rem configure bootstrap tools on top of the interpreter
set SCHEMEC=%SCHEME% out\scheme-compiler-flat-ts.scm
set SASMOPT=%SCHEME% out\sasm-opt-flat-ts.scm
set SASMC=%SCHEME% out\sasm-flat-ts.scm

set OUTDIR=out\bootstrap
