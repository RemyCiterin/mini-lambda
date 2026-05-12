all:
	cabal run
	gcc runtime/*.c -I runtime -o runtime/main -g -O2
	runtime/main

run:
	gcc runtime/*.c -I runtime -o runtime/main -g -O2
	runtime/main

doc:
	cabal v2-haddock --haddock-all
