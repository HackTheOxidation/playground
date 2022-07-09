FILE = version
COMPILER = gcc
CONFIG = `pkg-config --libs --cflags gtk+-3.0`

default: build run

build:
	 $(COMPILER) -o $(FILE).o $(FILE).c $(CONFIG)

run:
	sudo ./$(FILE).o
