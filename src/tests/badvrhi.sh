out/bootstrap/test/badvrhi.exe 2>out/bootstrap/test/badvrhi-stderr.out
export status=$?
cat out/bootstrap/test/badvrhi-stderr.out
echo
echo exit=$status
