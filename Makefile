# fvm makefile

all: test

CC = clang
CFLAGS = -m32 -Wall

SOURCES = fvm.c misc.c parse.c file.c
HEADERS = fvm.h dict.inc prims.inc
LIBS = -lreadline

fvm: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(SOURCES) $(LIBS) -o fvm

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
