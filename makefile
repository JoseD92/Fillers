x86_64: filler.hs zerofiller.hs
	ghc filler.hs -cpp -rtsopts
	ghc zerofiller.hs -cpp -rtsopts

i386: filler.hs zerofiller.hs
	ghc filler.hs -cpp
	ghc zerofiller.hs -cpp
