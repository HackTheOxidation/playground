SOURCE=pi_mutual.c
EXE=pi_mutual
CC=gcc
CFLAGS=-fopenmp

all: vars build

vars:
	export OMP_NUM_THREADS=4

build:
	$(CC) $(CFLAGS) $(SOURCE) -o $(EXE)

run: build
	./$(EXE)

clean:
	rm $(EXE)
