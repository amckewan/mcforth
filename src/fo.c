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


cell name_to_xt(cell nfa) {
    return aligned(nfa + (m[nfa] & 31) + 1);
}

cell xt_to_name(cell cfa) {
    cell nfa = cfa - CELL;
    // including smudge bit makes mishits less likely
    while (nfa > cfa - 32 && aligned(nfa + (m[nfa] & 127) + 1) != cfa) nfa -= CELL;
    return nfa;
}

int match(const char *name, const char *str, int len) {
    for (int i = 0; i < len; i++) {
        if (toupper(name[i]) != toupper(str[i])) return 0;
    }
    return 1;
}

cell find(cell name, cell link) {
    //printf("find '"); type(name+1, m[name]); printf("' link %tX\n", link);
    // return xt if found, else zero
    int len = m[name];
    while (link) {
        //printf("find len=%d link=%tX\n", len, link);
        if ((m[link + CELL] & 63) == len
              && match((char*)m + name + 1, (char*)m + link + CELL + 1, len)) {
            cell xt = aligned(link + CELL + 1 + len);
            if (m[link + CELL] & 0x80) // headless
                xt = *(cell *)(m + xt);
            //if (m[link + CELL] & 0x40) // immediate
            //    xt = -xt;
            //printf("found xt=%tX\n", xt);
            return xt;
        }
        link = AT(link);
    }
    //printf("not found\n");
    return 0;
}

#define THREADS 8 // # of method threads in the class
cell find_method(cell class, cell selector) {
    cell link = class + selector & CELLS(THREADS - 1);
    while ((link = AT(link))) {
        if (AT(link + CELL) == selector) {
            return link + 2 * CELL;
        }
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
        link = AT(link);
    }
}

char *new_string(const char *str, int len) {
    char *cstr = malloc(len + 2);
    cstr[0] = len;
    memcpy(cstr+1, str, len);
    cstr[len+1] = 0;
    return cstr;
}

cell get_env(cell *S, cell len) {
    char *name = new_string(abs(*S), len);
    char *value = getenv(name + 1);
    free(name);
    *S = value ? rel(value) : 0;
    return value ? strlen(value) : 0;
}

void set_env(cell *S, cell len) {
    char *name = new_string(abs(*S), len);
    if (S[1] == 0) {
        unsetenv(name + 1);
    } else {
        char *value = new_string(abs(S[2]), S[1]);
        setenv(name + 1, value + 1, 1);
        free(value);
    }
    free(name);
}

void dump(int a, int n, int base) {
    int i, j;
    for (i = 0; i < n; i += 16, a += 16) {
        if (base == 10) printf("%4d ", a); else printf("%04X ", a);
        for (j = 0; j < 16; j++) {
            if (j % 4 == 0) putchar(' ');
            printf("%02X ", m[a + j]);
        }
        putchar(' ');
        for (j = 0; j < 16; j++)
            putchar(isprint(m[a + j]) ? m[a + j] : '.');
        putchar('\n');
    }
}

void show_error(const char *msg, const char *here, const struct source *source) {
    int col = source->in;
    if (col == source->len) col++;
    putchar('\n');
//    if (source->file != SOURCE_CONSOLE && source->file != SOURCE_EVALUATE)
    if (source->filename) {
        cell col, line = source_position(source, &col);
        printf("%s:%td:%d: ", abs(source->filename+1), line, col);
    }
    int n = *here & 31;
    while (n--) putchar(*++here);
    putchar(' ');
    if (msg) {
        n = *msg++;
        while (n--) putchar(*msg++);
    }
    // show the line and position
//    cr(); type(source->addr, source->len);
//    cr(); for (int i = 0; i < in; i++) emit(' '); putchar('^');
}

int run(int argc, char *argv[]) {

#include "prims.inc"

    default:
        printf("Invalid opcode 0x%02X\n", I[-1]);
        goto abort;
    }
}

// Initial dictionary
byte dict[] = {
#ifdef KERNEL
    #include "kernel.inc"
#else
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

int main(int argc, char *argv[]) {
    memcpy(m, dict, sizeof dict);

    // process args, handle and remove the ones I use
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
            }
        }
        fargv[fargc++] = argv[i];
    }

    if (verbose > 1) {
        printf("sizeof(source) = %tu\n", sizeof(struct source));
        printf("m = %p, argv = %p, diff = %td\n", m, argv, (byte*)argv-m);
        byte *temp = malloc(100);
        printf("m = %p, malloc = %p, diff = %td\n", m, temp, temp-m);
    }

    return run(fargc, fargv);
}
