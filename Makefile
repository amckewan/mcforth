# fo makefile

all: test

CC = clang
CFLAGS = -m32 -Wall -Werror

SOURCES = fo.c misc.c parse.c file.c
HEADERS = fo.h dict.inc prims.inc
LIBS = -lreadline

fo: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(SOURCES) $(LIBS) -o fo

forth: $(SOURCES) $(HEADERS) forth.inc
	$(CC) -DEXTEND $(CFLAGS) $(SOURCES) $(LIBS) -o forth

prims.inc dict.inc: meta.fs
	gforth meta.fs -e ciao

forth.inc: fo rth extend
	./fo extend

compare:
	hexdump -C dict.img > dict.hex
	hexdump -C forth.img > forth.hex
	meld dict.hex forth.hex

run: fo
	@./fo

test: fo rth test.fs
	@echo "CR BYE" > bye
	@./fo rth test.fs bye

testv: fo
	@./fo -v

clean:
	@rm fo *.inc *.img *.dump bye
