# fvm makefile

fvm: fvm.c kernel.c
	clang -m32 fvm.c -o fvm

kernel.c: meta.f
	gforth meta.f
	