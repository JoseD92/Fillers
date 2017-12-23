x86_64: filler.hs zerofiller.hs
	ghc filler.hs -cpp -Dx86_64
	ghc zerofiller.hs -cpp -Dx86_64

i386: filler.hs zerofiller.hs
	ghc filler.hs -cpp -Di386
	ghc zerofiller.hs -cpp -Di386