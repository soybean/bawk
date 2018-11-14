#!/bin/bash
make clean
make bawk.native
./bawk.native -c "tests/pass-helloworldloopend.bawk" -f ./input.txt > helloworld.c
lli helloworld.c
