
compile: Main.hs FFI.hs Dlsym.hs Parser.hs
	ghc -o test --make Main

Dlsym.hs: gen
	./gen



gen: Gen.hs FFI.hs
	ghc -o gen -main-is Gen --make Gen

clean:
	rm *.o
	rm test

recompile: clean compile


