#!/bin/bash
make clean
make bawk.native
./bawk.native -c "tests/pass-helloworld.bawk" -f ./input.txt > helloworld.c
lli helloworld.c
