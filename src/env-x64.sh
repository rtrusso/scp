# configure paths to TinyScheme interpreter as bootstrap
export TINYSCHEMEINIT=../bootstrap/tinyscheme-1.41/init.scm
export SCHEME='../bootstrap/tinyscheme-1.41/scheme.exe -1'

# configure bootstrap tools on top of the interpreter
export SCHEMEC="$SCHEME out/scheme-compiler-flat-ts.scm"
export SASMOPT="$SCHEME out/sasm-opt-x64-flat-ts.scm"
export SASMC="$SCHEME out/sasm-x64-flat-ts.scm"

export OUTDIR=out/bootstrap-x64
