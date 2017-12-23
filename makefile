x86_64: filler.hs zerofiller.hs
	ghc filler.hs -cpp
	ghc zerofiller.hs -cpp

i386: filler.hs zerofiller.hs
	ghc filler.hs -cpp
	ghc zerofiller.hs -cpp