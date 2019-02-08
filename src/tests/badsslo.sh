out/bootstrap/test/badsslo.exe 2>out/bootstrap/test/badsslo-stderr.out
export status=$?
cat out/bootstrap/test/badsslo-stderr.out
echo
echo exit=$status
