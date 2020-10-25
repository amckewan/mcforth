// fo.c

#include "fo.h"

// sizes in cells
#define DATASIZE    20000
#define STACKSIZE   100

static cell M[DATASIZE];
byte * const m = (byte *)M;

// return stack grows down from top of memory
cell *const R0 = M + DATASIZE;

// data stack, grows down
static cell stack[STACKSIZE+20]; // a bit of underflow not tragic
cell * const S0 = stack + STACKSIZE;

int verbose;

#define CELL sizeof(cell)
#define aligned(x) (((cell)(x) + (CELL - 1)) & ~(CELL - 1))

// Memory Map
#define COLD M[0]
#define HERE M[1]
#define SOURCE M[2]
#define BASE M[3]
#define STATE M[4]
#define CONTEXT 5 /* 3 cells */
#define WARM M[8]

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

int digit(char c) {
    return (c <= '9') ? c - '0' : 10 + toupper(c) - 'A';
}

int number(const char *str, cell *num) {
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

cell to_number(cell *sp, cell top) {
    uint64_t u = (uint32_t)sp[2] | ((uint64_t)sp[1] << 32);
    char *str = abs(sp[0]);
    while (top) {
        int d = digit(*str);
        if (d < 0 || d >= BASE) break;
        u = u * BASE + d;
        ++str;
        --top;
    }
    sp[0] = rel(str);
    sp[1] = u >> 32;
    sp[2] = u;
    return top;
}

void type(cell addr, cell len) {
    while (len--) putchar(m[addr++]);
}

void *litq(void *I) {
    uchar *p = (uchar *)I;
    return (cell *)aligned(p + *p + 1);
}

void *dotq(void *I) {
    char *p = (char *)I;
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

void fo(int argc, char *argv[]) {
    cell *S, top;
    cell *R;
    byte *I;
    cell w;

    BASE = 10;

    if (COLD) {
        printf("Running from %u\n", COLD);
        I = abs(COLD);
        goto start;
    }

    printf("hi\n");

abort:
    I = abs(WARM);
start:
    STATE = 0;
    M[CONTEXT] = 1;
    S = S0;
    R = R0;

next:
//printf("I=%X op=%02X\n", I-m, *(I-m));
    w = *I++;
exec:
    switch (w) {

#include "prims.inc"

    default:
        printf("Invalid opcode 0x%X\n", w);
        goto abort;
    }
}

// Initial dictionary
byte dict[10000] = {
#include "dict.inc"
};

int main(int argc, char *argv[]) {
    memcpy(m, dict, sizeof dict);

    if (argc > 1 && !strcmp(argv[1], "-v")) {
        verbose = 1;
        for (int i = 2; i < argc; i++)
            argv[i-1] = argv[i];
        argc--;
    }

    if (verbose) {
        printf("sizeof(source) = %u\n", sizeof(struct source));
        printf("m = %p, argv = %p, diff = %td\n", m, argv, (byte*)argv-m);
        byte *temp = malloc(100);
        printf("m = %p, malloc = %p, diff = %td\n", m, temp, temp-m);
    }

    fo(argc, argv);
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

    fo(argc, argv);
    return 0;
}
