// fvm.c

#include "fvm.h"

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

byte m[0x2000] = {
#include "dict.inc"
};

#define M(n) *(cell *)&m[n]
#define NEXT ; goto next;
#define push *++sp = top, top =
#define pop top = *sp--
#define pop2 top = sp[-1], sp -= 2
#define pop3 top = sp[-2], sp -= 3
#define LOGICAL ? -1 : 0
#define aligned(x) (((cell)(x) + (CELL - 1)) & ~(CELL - 1))
#define c(x) HERE = x, HERE += CELL

// Memory Map
#define BOOT M(0)
#define HERE M(4)
#define SOURCE M(8)
#define BASE M(12)
#define STATE M(16)
#define CONTEXT 20

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
    cell link = M(CONTEXT + v * CELL);
    cell len = m[name];
    while (link) {
        if ((m[link + CELL] & 63) == len
            && match((char*)m + name + 1, (char*)m + link + CELL + 1, len))
            return link + CELL;

        link = M(link);
    }
    return 0;
}

cell number(cell addr, cell *n) {
    char *p = (char *)(m + addr + 1);
    char *endptr;
    if (*p == '-')
        *n = strtol(p, &endptr, 10);
    else
        *n = strtoul(p, &endptr, 10);
    return (*endptr == 0);
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
    cell link = M(CONTEXT + v * CELL);
    while (link) {
        dotid(link + CELL);
        link = M(link);
    }
}

void fvm() {
    cell stack[100], *sp, top;
    cell rack[100], *rp;
    opcode *ip;
    cell w;

    printf("hi\n");
    BASE = 10;

abort:
    STATE = 0;
    M(CONTEXT) = 1;
    sp = stack;
    *sp = top = 0;
    rp = rack;
    ip = (opcode *)phys(0x200);

next:
//printf("ip=%X op=%02X\n", ip-m, *(ip-m));
    w = *ip++;
exec:
    switch (w) {

#include "prims.inc"

    case 0xFF: // call 32-bit address
        w = *(cell*)ip;
        //printf("call 0x%x\n", w);
        *++rp = (cell)ip + CELL;
        ip = (opcode*)(m + w);
        NEXT

    default:
        printf("Invalid opcode 0x%X\n", w);
        goto abort;
    }
}

int main(int argc, char *argv[]) {
    printf("sizeof(source) = %u\n", sizeof(struct source));
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
