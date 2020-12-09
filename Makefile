# mcforth makefile

all: test

CC = gcc
#CC = clang
CFLAGS = -m32
#CFLAGS += -Wall
CFLAGS += -Werror
CFLAGS += -O3

SOURCES = src/fo.c src/string.c src/parse.c src/file.c
HEADERS = src/fo.h
INCLUDES = -I.
LIBS = -ledit

forth: forth.inc $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $@

forth.inc: fo rth extend src/*.f
	./fo extend
	hexdump -C forth.img > forth.hex

fo: prims.inc kernel.inc $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $@

prims.inc kernel.inc: src/meta.f src/kernel.f
	./forth src/meta.f -e ciao
	hexdump -C kernel.img > kernel.hex

bootstrap:
	gforth -e "4 CONSTANT CELL" src/cross.f -e ciao
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo
	./fo extend
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth
	rm kernel.inc
	$(MAKE) test

asm: Makefile prims.inc kernel.inc $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) src/fo.c -S

run: forth
	@./forth

test: forth test/*
	@./forth test/suite.f -e "cr bye"

bench: forth
	make -C bench

checkans: forth
	./forth /usr/share/gforth/0.7.3/test/checkans.fs -e bye

clean:
	@rm -f fo* *.inc *.img *.hex *.info *.s
