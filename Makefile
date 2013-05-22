compile:
	ghc -O -isrc -odir bin -hidir bin -o bin/sedit src/Main.hs

clean:
	rm -rf bin/*