out/bootstrap/test/badvrlo.exe 2>out/bootstrap/test/badvrlo-stderr.out
export status=$?
cat out/bootstrap/test/badvrlo-stderr.out
echo
echo exit=$status
