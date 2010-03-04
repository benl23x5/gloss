
EXAMPLE_DIRS		= $(shell ls examples)
EXAMPLE_BINS		= $(patsubst %,bin/%,$(EXAMPLE_DIRS))

SRC_Graphics_hs		= $(shell find src -name "*.hs" -follow)


all : $(EXAMPLE_BINS)


bin/% : examples/%/Main.hs $(SRC_Graphics_hs)
	ghc -O2 -isrc -i$(<D) --make $< -o $@


# -- cleaning -------------------------
.PHONY : clean
clean :
	@find . 	-name "*.o" \
		-o 	-name "*.hi" \
		| xargs rm
		
	@rm -f bin/*


# -- docs ----------------------------
.PHONY : doc
doc :
	haddock -w -h -o doc --optghc=-isrc $(SRC_Graphics_hs)
