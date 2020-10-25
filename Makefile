# fvm makefile

SOURCES = fvm.c misc.c parse.c file.c

fvm: $(SOURCES) fvm.h dict.inc prims.inc
	clang -m32 $(SOURCES) -lreadline -o fvm

prims.inc dict.inc: meta.fs
	gforth meta.fs -e ciao

run: fvm
	@./fvm

extend: fvm extend.fs
	@./fvm extend.fs

test: fvm
	@echo "CR BYE" > bye
	@./fvm test.fs bye

testv: fvm
	@./fvm -v

clean:
	@rm fvm prims.inc dict.*
