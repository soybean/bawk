./bawk.native -c int_to.bawk > int_to.ll
llc -relocation-model=pic int_to.ll > int_to.s
cc -o int_to.exe int_to.s convert.o
./int_to.exe
