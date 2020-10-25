# fo makefile

all: test

CC = clang
CFLAGS = -m32 -Wall

SOURCES = fo.c misc.c parse.c file.c
HEADERS = fo.h dict.inc prims.inc
LIBS = -lreadline

fo: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(SOURCES) $(LIBS) -o fo

prims.inc dict.inc: meta.fs
	gforth meta.fs -e ciao

run: fo
	@./fo

extend: fo extend.fs
	@./fo extend.fs

test: fo rth test.fs
	@echo "CR BYE" > bye
	@./fo rth test.fs bye

testv: fo
	@./fo -v

clean:
	@rm fo prims.inc dict.*
