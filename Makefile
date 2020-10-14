# fvm makefile

SOURCES = fvm.c lib.c parse.c file.c

fvm: $(SOURCES) dict.inc prims.inc lib.h
	clang -m32 $(SOURCES) -lreadline -o fvm

prims.inc: meta.fs
	gforth meta.fs -e ciao

test: fvm
	./fvm
