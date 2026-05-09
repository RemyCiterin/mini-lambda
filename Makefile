all:
	cabal run
	gcc runtime/*.c -I runtime -o runtime/main -O2
	runtime/main
