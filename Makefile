
all : bin/HelloWorld bin/TreeFractal

# -- files that are ready to compile
src_Graphics_hs		=  $(shell find src -name "*.hs" -follow)

bin/HelloWorld : examples/HelloWorld/Main.hs $(src_Graphics_hs)
	ghc -isrc --make examples/HelloWorld/Main.hs -o bin/HelloWorld

bin/TreeFractal : examples/TreeFractal/Main.hs $(src_Graphics_hs)
	ghc -isrc --make examples/TreeFractal/Main.hs -o bin/TreeFractal
	