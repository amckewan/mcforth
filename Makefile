# fvm makefile

SOURCES = fvm.c misc.c parse.c file.c

fvm: $(SOURCES) fvm.h dict.inc prims.inc
	clang -m32 $(SOURCES) -lreadline -o fvm

prims.inc: meta.fs
	gforth meta.fs -e ciao

test: fvm
	./fvm
