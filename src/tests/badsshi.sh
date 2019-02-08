out/bootstrap/test/badsshi.exe 2>out/bootstrap/test/badsshi-stderr.out
export status=$?
cat out/bootstrap/test/badsshi-stderr.out
echo
echo exit=$status
