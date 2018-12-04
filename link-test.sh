set -e
echo compiling
./bawk.native -c int_to.bawk > int_to.ll
echo ll compiling
llc -relocation-model=pic int_to.ll > int_to.s
echo linking
cc -o int_to.exe int_to.s convert.o structure.o mylist.o rgx.o
echo executing
./int_to.exe input.txt


