set -e
./bawk.native -c $1 > bawk_out.ll
llc -relocation-model=pic bawk_out.ll > bawk_out.s
cc -o bawk_out.exe bawk_out.s convert.o structure.o mylist.o rgx.o
./bawk_out.exe $2


