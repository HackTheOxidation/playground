FILE := labels_layout_gtk

default: build 

build:
	gcc `pkg-config --cflags --libs gtk+-3.0` $(FILE).c -o $(FILE)

clean:
	@rm $(FILE)
