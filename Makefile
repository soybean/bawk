.PHONY : all
all : san clean bawk.native convert.o structure.o mylist.o rgx.o

san:
	if [[ -f _build/sanitize.sh ]] ; then _build/sanitize.sh ; fi

.PRECIOUS : bawk.native
bawk.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		bawk.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff
	rm -rf *.o *.s *.ll *.out *.exe

convert : convert.c 
	cc -o convert convert.c

structure : structure.c 
	cc -o structure structure.c

rgx : rgx.c 
	cc -o rgx rgx.c

mylist : mylist.c 
	cc -o mylist mylist.c
