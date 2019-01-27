CPP = cpp
CPP_ARGS = -C -P

scheme.java : scheme.javap
	$(CPP) $(CPP_ARGS) -DTESTING=0 -o scheme.java scheme.javap

scheme_test.java : scheme.javap
	$(CPP) $(CPP_ARGS) -DTESTING=1 -o scheme_test.java scheme.javap

scheme_test.exe : scheme_test.java
	gcj --main=scheme_test scheme_test.java -o scheme_test.exe

clean:
	rm -f scheme_test.java scheme_test.exe *~
