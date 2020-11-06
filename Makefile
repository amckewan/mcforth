# fo makefile

all: test

CC = clang
CFLAGS = -m32 -Wall -Werror -Ofast

SOURCES = src/fo.c src/misc.c src/parse.c src/file.c
HEADERS = src/fo.h prims.inc kernel.inc
INCLUDES = -I.
LIBS = -lreadline

fo: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo

asm: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) -S

forth: $(SOURCES) $(HEADERS) forth.inc
	$(CC) -DEXTEND $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth

prims.inc kernel.inc: meta.fs
	gforth meta.fs -e ciao
	hexdump -C kernel.img > kernel.hex

new:
	./forth kernel.fs -e "prune save cr bye"
	hexdump -C kernel2.img > kernel2.hex
	diff kernel.hex kernel2.hex

forth.inc: fo rth extend lib/*
	./fo extend
	hexdump -C forth.img > forth.hex

run: forth
	@./forth

test: forth test.fs test/*
	@./forth test.fs -e "cr bye"

testv: fo
	@./fo -v

bench: forth
	make -C bench

clean:
	@rm -f fo forth *.inc *.img *.hex *.s
