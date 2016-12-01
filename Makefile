gp: Main.hs AST.hs Parser.hs Interp.hs Renderer.hs SampleScript.hs
	ghc $^ -Wall -o $@

build-test: Tests.hs gp
	ghc $< -o test

test: build-test
	./test

clean:
	rm *.hi *.o
