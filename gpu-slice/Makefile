.PHONY: main

build:
	ghc --make -threaded Main.hs

build-opt:
	ghc --make -Odph -pgmlo opt -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 Main.hs

run:
	./Main -ddump-gc 2> out.txt

clean:
	rm *.o *.hi ./Main

interact:
	ghci -XDataKinds -XScopedTypeVariables -XTypeOperators -XNoMonomorphismRestriction Main.hs


