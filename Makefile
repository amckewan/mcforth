# mcforth makefile

all: test

CC = clang -m32
CC64 = clang -m64
CFLAGS = -Wall -Werror
CFLAGS += -Ofast

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
	hexdump -C kernel.img > kernel.hex
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo
	./fo extend
	hexdump -C forth.img > forth.hex
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth
	./forth test/suite.f -e "cr bye"
	rm kernel.inc
#	$(MAKE) test

asm: prims.inc kernel.inc $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) src/fo.c -S

forth64:
	gforth -e "8 CONSTANT CELL" src/cross.f -e ciao
	$(CC64) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo64
	./fo64 extend
	$(CC64) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth64
	./forth64 src/meta.f -e ciao
	$(CC64) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo64
	./fo64 extend
	hexdump -C kernel.img > kernel64.hex
	$(CC64) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth64
	@./forth64 test/suite.f -e "cr bye"

asm64:
	$(CC64) -DKERNEL $(CFLAGS) $(INCLUDES) src/fo.c -S

run: forth
	@./forth

test: forth test/*
	@./forth test/suite.f -e "cr bye"

ootest: forth
	./forth test/ootest.f -e "cr bye"

stringtest: forth
	./forth test/stringtest.f -e "cr bye"

bench: forth
	make -C bench

bench64: forth64
	make -C bench FORTH=../forth64

checkans: forth
	./forth /usr/share/gforth/0.7.3/test/checkans.fs -e bye

clean:
	@rm -f fo* *.inc *.img *.hex *.info *.s
