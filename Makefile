# fvm makefile

fvm: fvm.c dict.inc prims.inc kernel.h kernel.c
	clang -m32 fvm.c kernel.c -o fvm

prims.inc: meta.fs
	gforth meta.fs -e ciao

test: fvm
	./fvm
