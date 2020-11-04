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


cell cfa(cell nfa) // convert nfa to cfa (NAME>)
{
    return nfa + (m[nfa] & 31) + 1;
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

void type(cell addr, cell len);

cell find(cell name, cell link) {
    //printf("find '"); typex((char*)m+name+1, m[name]); printf("'\n");
    // return xt if found, else zero
    cell len = m[name];
    while (link) {
        if ((m[link + CELL] & 63) == len
              && match((char*)m + name + 1, (char*)m + link + CELL + 1, len)) {
//            return link + CELL;
//            cell xt = link + CELL + 1 + len;
            cell xt = aligned(link + CELL + 1 + len);
            // xt = aligned(xt)
            // if ((m[link + CELL] & 0x80) == 0) { // headless?
            //     type(link + CELL + 1, len);
            //     printf(" oops ");
            //     return *(cell *)(m + xt);
            // }
            return xt;
        }

        link = *(cell *)(m + link);
    }
    return 0;
}

int digit(char c) {
    return (c <= '9') ? c - '0' : 10 + toupper(c) - 'A';
}

int number(const char *str, cell *num, int base) {
    int len = *str++;
    int n = 0;
    int sign = 1;
    if (len == 3 && *str == '\'' && str[2] == '\'') {
        *num = str[1];
        return TRUE;
    }
    if (len > 1) {
        if (*str == '#') base = 10, str++, len--;
        else if (*str == '$') base = 16, str++, len--;
        else if (*str == '%') base = 2, str++, len--;
    }
    if (len > 1 && *str == '-') {
        str++, len--;
        sign = -1;
    }
    if (len == 0) return FALSE;
    for (int i = 0; i < len; i++) {
        int d = digit(str[i]);
        if (d < 0 || d >= base) return FALSE;
        n = n * base + d;
    }
    *num = n * sign;
    return TRUE;
}

cell to_number(cell *sp, cell top, int base) {
    uint64_t u = (uint32_t)sp[2] | ((uint64_t)sp[1] << 32);
    char *str = abs(sp[0]);
    while (top) {
        int d = digit(*str);
        if (d < 0 || d >= base) break;
        u = u * base + d;
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
    return (cell *)(p + *p + 1);
}

void *dotq(void *I) {
    char *p = (char *)I;
    int n = *p++;
    while (n--) putchar(*p++);
    return (cell *)(p);
}

void dotid(cell nfa) {
    type(nfa + 1, m[nfa] & 31);
    putchar(BL);
    if ((m[nfa] & 0x80)) printf("%% ");
    if ((m[nfa] & 0x20)) printf("(smudged) ");
}

void words(cell link) {
    while (link) {
        dotid(link + CELL);
        link = *(cell*)(m + link);
    }
}

int run(int argc, char *argv[]) {

#include "prims.inc"

    default:
        printf("Invalid opcode 0x%02X\n", w);
        goto abort;
    }
}

// Initial dictionary
byte dict[] = {
#ifdef EXTEND
    #include "extended.inc"
#else
    #include "kernel.inc"
#endif
};

int main(int argc, char *argv[]) {
    memcpy(m, dict, sizeof dict);

    if (argc > 1 && !strncmp(argv[1], "-v", 2)) {
        verbose = strlen(argv[1]) - 1;
        for (int i = 2; i < argc; i++)
            argv[i-1] = argv[i];
        argc--;
    }

    if (verbose > 1) {
        printf("sizeof(source) = %u\n", sizeof(struct source));
        printf("m = %p, argv = %p, diff = %td\n", m, argv, (byte*)argv-m);
        byte *temp = malloc(100);
        printf("m = %p, malloc = %p, diff = %td\n", m, temp, temp-m);
    }

    return run(argc, argv);

    const char *filename = "dict.img";
    if (argc > 1) filename = argv[1];
    FILE *f = fopen(filename, "r");
    if (!f) {
        printf("Cannot open %s\n", filename);
        return 1;
    }
    fread(m, 1, 0x2000, f);
    fclose(f);

    return run(argc, argv);
}
