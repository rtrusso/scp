out/bootstrap/test/badsrlo.exe 2>out/bootstrap/test/badsrlo-stderr.out
export status=$?
cat out/bootstrap/test/badsrlo-stderr.out
echo
echo exit=$status
