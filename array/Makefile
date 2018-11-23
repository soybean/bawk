CC = gcc
INCLUDES = 
CFLAGS = -g -Wall $(INCLUDES)
LDFLAGS = -g
LDLIBS =

mylist-test: mylist-test.o libmylist.a

mylist-test.o: mylist-test.c mylist.h

mylist.o: mylist.c mylist.h

libmylist.a: mylist.o
	ar rc libmylist.a mylist.o
	ranlib libmylist.a

.PHONY: clean
clean:
	rm -f *.o core mylist-test

.PHONY: all
all: clean mylist-test

.PHONY: valgrind
valgrind: mylist-test
	valgrind --leak-check=yes ./mylist-test
