MAKEFLAGS=--warn-undefined-variables

SDL2_CFLAGS	= `sdl2-config --cflags`
SDL2_LIBS	= `sdl2-config --libs`

sources_test_c	= $(shell find src -name "*.c")
objects_test_c	= $(patsubst %.c,%.o,${sources_test_c})

bin/test: ${objects_test_c}
	clang -Wall -std=c99 -g ${SDL2_CFLAGS} ${SDL2_LIBS} $^ -o $@

%.o : %.c
	clang -Wall -Werror -std=c99 -O2 -g ${SDL2_CFLAGS} -c $< -o $@

