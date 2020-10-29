# fo makefile

all: test

CC = clang
CFLAGS = -m32 -Wall -Werror

SOURCES = fo.c misc.c parse.c file.c
HEADERS = fo.h prims.inc kernel.inc
LIBS = -lreadline

fo: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(SOURCES) $(LIBS) -o fo

forth: $(SOURCES) $(HEADERS) extended.inc
	$(CC) -DEXTEND $(CFLAGS) $(SOURCES) $(LIBS) -o forth

prims.inc kernel.inc: meta.fs
	gforth meta.fs -e ciao
	hexdump -C kernel.img > kernel.hex

extended.inc: fo rth extend lib/see
	./fo extend
	hexdump -C extended.img > extended.hex

run: forth
	@./forth

test: forth test.fs
	@echo "CR BYE" > bye
	@./forth test.fs bye

testv: fo
	@./fo -v

clean:
	@rm fo forth *.inc *.img *.hex
