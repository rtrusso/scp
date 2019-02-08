out/bootstrap/test/badvslo.exe 2>out/bootstrap/test/badvslo-stderr.out
export status=$?
cat out/bootstrap/test/badvslo-stderr.out
echo
echo exit=$status
