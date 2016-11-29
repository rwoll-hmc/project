gp: Main.hs AST.hs Parser.hs
	ghc $^ -o $@

build-test: Tests.hs gp
	ghc $< -o test

test: build-test
	./test

clean:
	rm *.hi *.o
