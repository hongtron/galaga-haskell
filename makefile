COMPILER = ghc

Galaga : Galaga.hs
	$(COMPILER) Galaga.hs

clean:
	rm *.hi && rm *.o && rm Galaga
