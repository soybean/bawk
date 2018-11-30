CC = gcc
INCLUDES = 
CFLAGS = -g -Wall $(INCLUDES)
LDFLAGS = -g
LDLIBS =

mylist: mylist.o

mylist.o: mylist.c

.PHONY: clean
clean:
	rm -f *.o core mylist

.PHONY: all
all: clean mylist

.PHONY: valgrind
valgrind: mylist
	valgrind --leak-check=yes ./mylist
