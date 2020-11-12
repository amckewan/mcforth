# mcforth makefile

all: test

CC = clang -m32
CC64 = clang -m64
CFLAGS = -Wall -Werror
CFLAGS += -Ofast

SOURCES = src/fo.c src/misc.c src/parse.c src/file.c
HEADERS = src/fo.h
INCLUDES = -I.
LIBS = -lreadline

forth: forth.inc $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $@

forth.inc: fo rth extend lib/*
	./fo extend
	hexdump -C forth.img > forth.hex

fo: kernel.inc $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $@

kernel.inc: meta.f kernel.f
	./forth meta.f kernel.f -e ciao
	hexdump -C kernel.img > kernel.hex

bootstrap:
	gforth -e "4 CONSTANT CELL" cross.f kernel.f -e ciao
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo
	./fo extend
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth
	touch kernel.f
	$(MAKE) test

asm: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) src/fo.c -S

forth64:
	gforth -e "8 CONSTANT CELL" cross.f kernel.f -e ciao
	$(CC64) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo64x
	./fo64x extend
	$(CC64) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth64x
	./forth64x meta.f kernel.f -e ciao
	$(CC64) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo64
	./fo64 extend
	hexdump -C kernel.img > kernel64.hex
	$(CC64) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth64
	@./forth64 test.f -e "cr bye"

asm64:
	$(CC64) -DKERNEL $(CFLAGS) $(INCLUDES) src/fo.c -S


run: forth
	@./forth

test: forth test.f test/*
	@./forth test.f -e "cr bye"

bench: forth
	make -C bench

checkans: forth
	./forth /usr/share/gforth/0.7.3/test/checkans.fs -e bye

clean:
	@rm -f fo* *.inc *.img *.hex *.s
