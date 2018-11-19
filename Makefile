# "make test" removes all previously generated files, compiles everything, and runs the regression tests

.PHONY : test
test : all test-script.sh
	./test-script.sh

# "make all" removes all previously generated files and builds the executable

.PHONY : all
all : clean bawk.native convert.o

# "make bawk.native" compiles the compiler

.PRECIOUS : bawk.native
bawk.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		bawk.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean

convert: convert.c cc -o convert convert.c
