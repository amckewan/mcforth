# mcforth makefile

ARCH = 32
CELL = 4

all: bootstrap

CC = clang
CFLAGS = -m$(ARCH) -Wall -Werror -Ofast

SOURCES = src/fo.c src/misc.c src/parse.c src/file.c
HEADERS = src/fo.h
INCLUDES = -I.
LIBS = -lreadline

#forth: forth.inc $(SOURCES) $(HEADERS)
#	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $@

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
	gforth -e "$(CELL) CONSTANT CELL" cross.f kernel.f -e ciao
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo
	./fo extend
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth
	touch kernel.f
	$(MAKE) test

asm: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) src/fo.c -S


run: forth
	@./forth

test: forth test.f test/*
	@./forth test.f -e "cr bye"

testv: fo
	@./fo -v

bench: forth
	make -C bench

checkans: forth
	./forth /usr/share/gforth/0.7.3/test/checkans.fs -e bye

clean:
	@rm -f fo forth *.inc *.img *.hex *.s
