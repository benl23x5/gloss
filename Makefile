
all : bin/HelloWorld bin/TreeFractal bin/SnowFlake bin/TreeZen bin/ClockFractal 

# -- files that are ready to compile
src_Graphics_hs		=  $(shell find src -name "*.hs" -follow)

bin/HelloWorld : examples/HelloWorld/Main.hs $(src_Graphics_hs)
	ghc -O2 -isrc --make examples/HelloWorld/Main.hs -o bin/HelloWorld

bin/TreeFractal : examples/TreeFractal/Main.hs $(src_Graphics_hs)
	ghc -O2 -isrc --make examples/TreeFractal/Main.hs -o bin/TreeFractal

bin/SnowFlake : examples/SnowFlake/Main.hs $(src_Graphics_hs)
	ghc -O2 -isrc --make examples/SnowFlake/Main.hs -o bin/SnowFlake

bin/TreeZen : examples/TreeZen/Main.hs $(src_Graphics_hs)
	ghc -O2 -isrc --make examples/TreeZen/Main.hs -o bin/TreeZen

bin/ClockFractal : examples/ClockFractal/Main.hs $(src_Graphics_hs)
	ghc -O2 -isrc --make examples/ClockFractal/Main.hs -o bin/ClockFractal


.PHONY : clean
clean :
	@find . 	-name "*.o" \
		-o 	-name "*.hi" \
		| xargs rm
		
	@rm -f bin/*