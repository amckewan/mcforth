# mcforth makefile

all: test2

CELL = 4

CC = gcc
#CFLAGS = -Wall -Werror
CFLAGS += -Ofast
ifeq ($(CELL),4)
CFLAGS += -m32
endif

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
	gforth -e "$(CELL) CONSTANT CELL" src/cross.f -e ciao
	hexdump -C kernel.img > kernel.hex
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo
	./fo extend
	hexdump -C forth.img > forth.hex
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth
	rm kernel.inc
	$(MAKE) test

asm: prims.inc kernel.inc $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) src/fo.c -S

run: forth
	@./forth

test: forth test/*
	@./forth test/suite.f -e "cr bye"

test2:
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo

bench: forth
	make -C bench

checkans: forth
	./forth /usr/share/gforth/0.7.3/test/checkans.fs -e bye

clean:
	@rm -f fo forth *.inc *.img *.hex *.info *.s
