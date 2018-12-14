default: Main.hs
	ghc -o Main Main.hs
clean: Main.hs
	ghc -o Main Main.hs
	rm *.hi
	rm *.o
remove:
	rm *.o
	rm *.hi
	rm Main
