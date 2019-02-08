out/bootstrap/test/badsrhi.exe 2>out/bootstrap/test/badsrhi-stderr.out
export status=$?
cat out/bootstrap/test/badsrhi-stderr.out
echo
echo exit=$status
