all: tobase frombase

tobase: tobase.hs
	ghc -O2 --make tobase

frombase: frombase.hs
	ghc -O2 --make frombase

clean: tobase tobase.hi tobase.o frombase frombase.hi frombase.o
	rm tobase tobase.hi tobase.o frombase frombase.hi frombase.o
