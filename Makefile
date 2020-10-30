# fo makefile

all: test

CC = clang
CFLAGS = -m32 -Wall -Werror

SOURCES = src/fo.c src/misc.c src/parse.c src/file.c
HEADERS = src/fo.h prims.inc kernel.inc
INCLUDES = -I.
LIBS = -lreadline

fo: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o fo

forth: $(SOURCES) $(HEADERS) extended.inc
	$(CC) -DEXTEND $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o forth

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
