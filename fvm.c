// fvm.c

#include "fvm.h"

#define DATASIZE 20000 // in cells
#define STACKSIZE 100

#define CELL sizeof(cell)

typedef struct {
    cell in;
    cell len;
    uchar *addr;
} Input;

typedef struct {
    cell link;
    uchar count;
    uchar name[1];
} Header;

byte dict[10000] = {
#include "dict.inc"
};

static cell M[DATASIZE];
byte * const m = (byte *)M;
int verbose;


#define NEXT ; goto next;
#define push *++sp = top, top =
#define pop top = *sp--
#define pop2 top = sp[-1], sp -= 2
#define pop3 top = sp[-2], sp -= 3
#define LOGICAL ? -1 : 0
#define aligned(x) (((cell)(x) + (CELL - 1)) & ~(CELL - 1))
#define c(x) HERE = x, HERE += CELL

// Memory Map
#define BOOT M[0]
#define HERE M[1]
#define SOURCE M[2]
#define BASE M[3]
#define STATE M[4]
#define CONTEXT 5 /* 3 cells */

cell cfa(cell nfa) // convert nfa to cfa (NAME>)
{
    return aligned(nfa + (m[nfa] & 31) + 1);
}

cell nfa(cell cfa) {
    // convert cfa to nfa (>NAME)
    do
        --cfa;
    while ((m[cfa] & 0x80) == 0);
    return cfa;
}

int match(const char *name, const char *str, int len) {
    for (int i = 0; i < len; i++) {
        if (toupper(name[i]) != toupper(str[i])) return 0;
    }
    return 1;
}

void typex(const char *str, int len) {
    fwrite(str, len, sizeof(char), stdout);
}

cell find(cell name, cell v) {
    //printf("find '"); typex((char*)m+name+1, m[name]); printf("'\n");
    // return nfa if found, else zero
    cell link = M[CONTEXT + v];
    cell len = m[name];
    while (link) {
        if ((m[link + CELL] & 63) == len
            && match((char*)m + name + 1, (char*)m + link + CELL + 1, len))
            return link + CELL;

        link = *(cell *)(m + link);
    }
    return 0;
}

static int digit(char c) {
    return (c <= '9') ? c - '0' : 10 + toupper(c) - 'A';
}

cell number(const char *str, cell *num) {
    int len = *str++;
    int n = 0;
    int sign = 1;
    if (len > 1 && *str == '-') {
        str++, len--;
        sign = -1;
    }
    for (int i = 0; i < len; i++) {
        int d = digit(str[i]);
        if (d < 0 || d >= BASE) return FALSE;
        n = n * BASE + d;
    }
    *num = n * sign;
    return TRUE;
}


/*
header *find(char *name)
{
        header *h = last;
        while (h && stricmp(h->name, name))
                h = h->link;
        return h;
}
*/

void type(cell addr, cell len) {
    while (len--) putchar(m[addr++]);
}

void *litq(void *ip) {
    uchar *p = (uchar *)ip;
    return (cell *)aligned(p + *p + 1);
}

void *dotq(void *ip) {
    char *p = (char *)ip;
    int n = *p++;
    while (n--) putchar(*p++);
    return (cell *)aligned(p);
}

void dotid(cell nfa) {
    type(nfa + 1, m[nfa] & 31);
    putchar(BL);
    if ((m[nfa] & 0x20)) printf("(smudged) ");
}

void words(cell v) {
    cell link = M[CONTEXT + v];
    while (link) {
        dotid(link + CELL);
        link = *(cell*)(m + link);
    }
}

void fvm() {
    cell stack[1000], *sp, top;
    cell rack[1000], *R;
    opcode *ip;
    cell w;

    printf("hi\n");
    BASE = 10;

abort:
    STATE = 0;
    M[CONTEXT] = 1;
    sp = stack;
    *sp = top = 0;
    R = rack;
    ip = (opcode *)phys(0x200);

next:
//printf("ip=%X op=%02X\n", ip-m, *(ip-m));
    w = *ip++;
exec:
    switch (w) {

#include "prims.inc"

    default:
        printf("Invalid opcode 0x%X\n", w);
        goto abort;
    }
}

int main(int argc, char *argv[]) {
    memcpy(m, dict, sizeof dict);
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-v")) {
            verbose = 1;
        } else {
//            include_file(argv[i]);
        }
    }
    if (verbose) printf("sizeof(source) = %u\n", sizeof(struct source));
    fvm();
    return 0;

    const char *filename = "dict.img";
    if (argc > 1) filename = argv[1];
    FILE *f = fopen(filename, "r");
    if (!f) {
        printf("Cannot open %s\n", filename);
        return 1;
    }
    fread(m, 1, 0x2000, f);
    fclose(f);

    fvm();
    return 0;
}
