tobase: tobase.hs
	ghc -O2 --make tobase

clean: tobase tobase.hi tobase.o
	rm tobase tobase.hi tobase.o
