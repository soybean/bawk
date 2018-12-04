CC = gcc
INCLUDES = 
CFLAGS = -g -Wall $(INCLUDES)
LDFLAGS = -g
LDLIBS =

mylist_nested: mylist_nested.o

mylist_nested.o: mylist_nested.c

.PHONY: clean
clean:
	rm -f *.o core mylist_nested

.PHONY: all
all: clean mylist_nested

.PHONY: valgrind
valgrind: mylist_nested
	valgrind --leak-check=yes ./mylist_nested
