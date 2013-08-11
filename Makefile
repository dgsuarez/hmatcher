hmatcher: 
	ghc -O2 --make main.hs -o hmatcher

clean:
	rm hmatcher *.hi *.o
