# fvm makefile

SOURCES = fvm.c misc.c parse.c file.c

fvm: $(SOURCES) fvm.h dict.inc prims.inc
	clang -m32 $(SOURCES) -lreadline -o fvm

prims.inc dict.inc: meta.fs
	gforth meta.fs -e ciao

test: fvm
	@./fvm

testv: fvm
	@./fvm -v

clean:
	@rm fvm prims.inc dict.*
