// fo.c

#include "fo.h"

// sizes in cells
#define DATASIZE    20000
#define STACKSIZE   100

static cell M[DATASIZE];

// return stack grows down from top of memory
cell *const R0 = M + DATASIZE;

// data stack, grows down
static cell stack[STACKSIZE+20]; // a bit of underflow not tragic
cell * const S0 = stack + STACKSIZE;

int verbose;

#define CELL sizeof(cell)
#define aligned(x) (((cell)(x) + (CELL - 1)) & ~(CELL - 1))


cell name_to_xt(cell nfa) {
    return aligned(nfa + (bat(nfa) & 31) + 1);
}

cell xt_to_name(cell cfa) {
    cell nfa = cfa - CELL;
    while (nfa > cfa - 32 && name_to_xt(nfa) != cfa) nfa -= CELL;
    return nfa;
}

int match(const char *name, const char *str, int len) {
    for (int i = 0; i < len; i++) {
        if (toupper(name[i]) != toupper(str[i])) return 0;
    }
    return 1;
}

#define relof(a)    ((cell)(a) - (cell)M)

cell find(cell strx, cell link) {
    const char *str = (const char *)strx;
    int len = *str++;
    //printf("find %s %X (%X)\n", str, link, relof(link));
    while (link) {
        //printf("find len=%d link=%tX\n", len, link);
        char *name = (char*)(link + CELL);
        if ((*name & 63) == len && match(name + 1, str, len)) {
            cell xt = aligned(link + CELL + 1 + len);
            if (*name & 0x80) // headless
                xt = *(cell *)xt;
            if (*name & 0x40) // immediate
                xt = -xt;
            //printf("found xt=%tX (%tX)\n", xt, relof(xt));
            return xt;
        }
        link = AT(link);
    }
    //printf("not found\n");
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
    while (len--) putchar(bat(addr++));
}

void *dotq(void *I) {
    char *p = (char *)I;
    int n = *p++;
    while (n--) putchar(*p++);
    return (cell *)(p);
}

void dotid(cell nfa) {
    type(nfa + 1, bat(nfa) & 31);
    putchar(BL);
    if ((bat(nfa) & 0x80)) printf("%% ");
    if ((bat(nfa) & 0x20)) printf("(smudged) ");
}

void words(cell link) {
    while (link) {
        dotid(link + CELL);
        link = at(link);
    }
}

int run(int argc, char *argv[]) {

#include "prims.inc"

    default:
        printf("Invalid opcode 0x%02tX\n", w);
        goto abort;
    }
}

// Initial dictionary
byte dict[] = {
#if defined KERNEL
    #include "kernel.inc"
#elif defined FORTH
    #include "forth.inc"
#endif
};

void load_image(const char *filename) {
    printf("load image %s\n", filename);
    FILE *f = fopen(filename, "r");
    if (!f) {
        printf("can't open image %s\n", filename);
        return;
    }
    fread(M, 1, sizeof M, f);
    fclose(f);
}

void load_and_relocate() {
    FILE *image_bin = fopen("kernel0.bin", "r");
    FILE *reloc_bin = fopen("reloc.bin", "r");
    if (!image_bin || !reloc_bin) {
        printf("can't open images\n");
        exit(1);
    }
    size_t size = fread(M, 1, sizeof M, image_bin);
    fclose(image_bin);
    uint8_t *reloc = malloc(size);
    fread(reloc, 1, size, reloc_bin);
    fclose(reloc_bin);

    // add M to every cell that needs relocating
    printf("M = %p\n", M);
    printf("relocating %tu bytes\n", size);
    uint8_t *image = (uint8_t *)M;
    cell offset = (cell)M;
    for (size_t i = 0; i < size; i++) {
        if (reloc[i]) {
            if (verbose > 1) printf("offset %4X\n", i);
            *(cell *)(image + i) += offset;
        }
    }
}

int main(int argc, char *argv[]) {
//    memcpy(m, dict, sizeof dict);

    int fargc = 1;
    char **fargv = calloc(argc, sizeof *argv);
    fargv[0] = argv[0];
    for (int i = 1; i < argc; i++) {
        char *arg = argv[i];
        if (*arg++ == '-') {
            switch (*arg) {
                case 'v':
                    verbose = strlen(arg);
                    continue;
                case 'i':
                    if (++i < argc) load_image(argv[i]);
                    continue;
                case 'r':
                    return relocate("kernel0.bin", "kernel1.bin", "kernel.img");
                    continue;
            }
        }
        fargv[fargc++] = argv[i];
    }

    load_and_relocate();

    if (verbose > 1) {
        // printf("sizeof(source) = %tu\n", sizeof(struct source));
        // printf("m = %p, argv = %p, diff = %td\n", m, argv, (byte*)argv-m);
        // byte *temp = malloc(100);
        // printf("m = %p, malloc = %p, diff = %td\n", m, temp, temp-m);
    }

    return run(fargc, fargv);
}
