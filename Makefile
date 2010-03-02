
all : bin/HelloWorld bin/TreeFractal bin/SnowFlake

# -- files that are ready to compile
src_Graphics_hs		=  $(shell find src -name "*.hs" -follow)

bin/HelloWorld : examples/HelloWorld/Main.hs $(src_Graphics_hs)
	ghc -isrc --make examples/HelloWorld/Main.hs -o bin/HelloWorld

bin/TreeFractal : examples/TreeFractal/Main.hs $(src_Graphics_hs)
	ghc -isrc --make examples/TreeFractal/Main.hs -o bin/TreeFractal

bin/SnowFlake : examples/SnowFlake/Main.hs $(src_Graphics_hs)
	ghc -isrc --make examples/SnowFlake/Main.hs -o bin/SnowFlake
	