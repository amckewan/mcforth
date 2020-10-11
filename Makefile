# fvm makefile

fvm: fvm.c dict.inc prims.inc
	clang -m32 fvm.c -o fvm

prims.inc: meta.fs
	gforth meta.fs -e ciao

test: fvm
	./fvm
