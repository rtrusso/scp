out/bootstrap/test/badvshi.exe 2>out/bootstrap/test/badvshi-stderr.out
export status=$?
cat out/bootstrap/test/badvshi-stderr.out
echo
echo exit=$status
