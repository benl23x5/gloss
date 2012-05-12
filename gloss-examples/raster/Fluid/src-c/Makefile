

bin/fluid : src/Main.c src/Interface.c src/Model.c src/Solver.c src/Solver.h src/Model.h
	gcc -Wall -Werror -O3 -funroll-loops -o bin/fluid src/Main.c src/Interface.c src/Model.c src/Solver.c \
		-L/usr/X11R6/lib -lgl -lglut -lglu -g
