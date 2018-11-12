# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all test-script.sh
	./test-script.sh

# "make all" builds the executable

.PHONY : all
all : bawk.native

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
