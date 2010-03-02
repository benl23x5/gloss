
all : bin/demo

# -- files that are ready to compile
src_Graphics_hs		=  $(shell find src -name "*.hs" -follow)

bin/demo : src/Demo/Main.hs $(src_Graphics_hs)
	ghc -isrc --make src/Demo/Main.hs -o bin/demo
	