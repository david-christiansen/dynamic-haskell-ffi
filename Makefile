compile: Main.hs FFITest.hs
	ghc -o test --make Main

clean:
	rm *.o
	rm test

recompile: clean compile


