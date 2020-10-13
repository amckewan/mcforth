# fvm makefile

fvm: fvm.c dict.inc prims.inc lib.h lib.c
	clang -m32 fvm.c lib.c -lreadline -o fvm

prims.inc: meta.fs
	gforth meta.fs -e ciao

test: fvm
	./fvm
