# Author(s): Alex

# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind

CC = gcc
CFLAGS = -g -Wall

.PHONY : all
all : neo.native libneoc.o

.PHONY : neo.native
neo.native :
	rm -f *.o
	ocamlbuild -use-ocamlfind \
		-pkgs llvm,llvm.analysis,str \
		-cflags -w,+a-4 \
		neo.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff neo scanner.ml parser.ml parser.mli
	rm -rf libneoc
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe *.err

libneoc.o :
	$(CC) $(CFLAGS) -c libneoc.c
