# mcforth makefile

all: tests

CELL = 4

CC = gcc
#CFLAGS = -Wall -Werror
CFLAGS += -Ofast
ifeq ($(CELL),4)
CFLAGS += -m32
endif

SOURCES = src/fo.c src/string.c src/parse.c src/file.c
HEADERS = src/fo.h
# use libedit to avoid GPL
#LIBS = -lreadline
LIBS = -ledit -ldl

forth: forth.inc $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(SOURCES) $(LIBS) -o $@

forth.inc: fo rth extend lib/*
	./fo exit-on-error extend
	hexdump -C forth.img > forth.hex

fo: prims.inc kernel.inc $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) $(SOURCES) $(LIBS) -o $@

prims.inc kernel.inc: src/meta.f src/kernel.f
	./forth src/meta.f -e ciao
	hexdump -C kernel.img > kernel.hex

bootstrap:
	gforth -e "$(CELL) CONSTANT CELL" src/cross.f -e ciao
	hexdump -C kernel.img > kernel.hex
	$(CC) -DKERNEL $(CFLAGS) $(SOURCES) $(LIBS) -o fo
	./fo extend
	hexdump -C forth.img > forth.hex
	$(CC) $(CFLAGS) $(SOURCES) $(LIBS) -o forth
	rm kernel.inc
	$(MAKE) tests

asm: prims.inc kernel.inc $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) src/fo.c -S

run: forth
	@./forth

tests: forth test/*
	@./forth exit-on-error test/all.f -e "cr bye"

bench: forth
	make -C bench

checkans: forth lib/standard.f
	./forth lib/standard.f test/checkans.f -e bye

clean:
	@rm -f fo forth *.inc *.img *.hex *.info *.s
