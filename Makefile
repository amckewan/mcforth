# mcforth makefile

all: abs

CC = clang -m32
CC64 = clang -m64
CFLAGS = -Wall -Werror
CFLAGS += -Ofast

SOURCES = src/fo.c src/misc.c src/parse.c src/file.c src/oo.c
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

kernel.inc: src/meta.f src/kernel.f
	./forth src/meta.f -e ciao
	hexdump -C kernel.img > kernel.hex

abs:
	gforth -e "4 CONSTANT CELL" -e "0 CONSTANT OFFSET" src/cross.f -e ciao
	mv kernel.bin kernel0.bin
	hexdump -C kernel0.bin > kernel0.hex
	gforth -e "4 CONSTANT CELL" -e "305419896 CONSTANT OFFSET" src/cross.f -e ciao
	mv kernel.bin kernel1.bin
	hexdump -C kernel1.bin > kernel1.hex


bootstrap:
	gforth -e "4 CONSTANT CELL" src/cross.f -e ciao
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo
	./fo extend
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth
	rm kernel.inc
	$(MAKE) test

asm: $(SOURCES) $(HEADERS)
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) src/fo.c -S
	$(CC) -DKERNEL $(CFLAGS) $(INCLUDES) src/oo.c -S

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
	@./forth64 test/test.f -e "cr bye"

asm64:
	$(CC64) -DKERNEL $(CFLAGS) $(INCLUDES) src/fo.c -S

run: forth
	@./forth

test: forth test/*
	@./forth test/test.f -e "cr bye"

bench: forth
	make -C bench

bench64: forth64
	make -C bench FORTH=../forth64

checkans: forth
	./forth /usr/share/gforth/0.7.3/test/checkans.fs -e bye

clean:
	@rm -f fo* *.inc *.img *.hex *.info *.s
