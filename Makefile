all:
	ocamlbuild -clean
	#ocamlbuild -use-ocamlfind -pkgs llvm, llvm.analysis -cflags -w,+a-4 bawk.native
