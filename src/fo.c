// fo.c

#include "fo.h"
#include <dlfcn.h>

// sizes in cells
#define DATASIZE    (32*1024) // default without -m option
#define STACKSIZE   1024

byte *m;
static cell *M;

// return stack grows down from top of memory
// todo: move stack memory mangement to Forth?
cell *R0;

// data stack, grows down
static cell stack[STACKSIZE+20]; // a bit of underflow not tragic
cell * const S0 = stack + STACKSIZE;

int verbose;

#define CELL sizeof(cell)
#define aligned(x) (((cell)(x) + (CELL - 1)) & ~(CELL - 1))

// todo: move name creation to forth
// todo: make nfa always 8 bytes! (WIDTH=7)
// cell name_to_xt(cell nfa) { return nfa + CELL; }
// cell xt_to_name(cell cfa) { return cfa - CELL; }

cell name_to_xt(cell nfa) {
    return aligned(nfa + (m[nfa] & 31) + 1);
}

cell xt_to_name(cell cfa) {
    cell nfa = cfa - CELL;
    // including smudge bit makes mishits less likely
    while (nfa > cfa - 32 && aligned(nfa + (m[nfa] & 63) + 1) != cfa) nfa -= CELL;
    return nfa;
}

int match(const char *name, const char *str, int len) {
    for (int i = 0; i < len; i++) {
        if (toupper(name[i]) != toupper(str[i])) return 0;
    }
    return 1;
}

// Search one find, return nfa or 0
cell search_wordlist2(cell name, cell len, cell wid) {
    cell link = AT(wid);
    while (link) {
        if ((m[link + CELL] & 63) == len
              && match((char*)m + name, (char*)m + link + CELL + 1, len)) {
            cell xt = aligned(link + CELL + 1 + len);
            if (m[link + CELL] & 0x80) // headless
                xt = AT(xt);
            if (m[link + CELL] & 0x40) // immediate
                xt = -xt;
            return xt;
        }
        link = AT(link);
    }
    return 0;

}

// SEARCH-WORDLIST ( c-addr u wid -- 0 | xt 1 | xt -1 )
cell search_wordlist(cell name, cell len, cell wid) {
    cell link = AT(wid);
    while (link) {
        if ((m[link + CELL] & 63) == len
              && match((char*)m + name, (char*)m + link + CELL + 1, len)) {
            cell xt = aligned(link + CELL + 1 + len);
            if (m[link + CELL] & 0x80) // headless
                xt = AT(xt);
            if (m[link + CELL] & 0x40) // immediate
                xt = -xt;
            return xt;
        }
        link = AT(link);
    }
    return 0;
}

cell find(cell name, cell context) {
    cell len = m[name++];
    cell wid, prev = 0;
    while ((wid = AT(context))) {
        if (wid != prev) {
            cell xt = search_wordlist(name, len, wid);
            if (xt) return xt;
            prev = wid;
        }
        context += CELL;
    }
    return 0;
}


#define THREADS 8 // # of method threads in the class
cell find_method(cell class, cell selector) {
    cell link = class + (selector & CELLS(THREADS - 1));
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

int number(const char *str, cell *num, cell base) { // -> true if converted
    int len = *str++;
    cell n = 0;
    cell sign = 1;
    if (str[0] == '\'' && str[2] == '\'' && len == 3) { // 'c'
        *num = str[1];
        return TRUE;
    }
    if (len > 1) { // Std. prefixes (convenient)
        if (*str == '#') base = 10, str++, len--; else
        if (*str == '$') base = 16, str++, len--; else
        if (*str == '%') base =  2, str++, len--;
    }
    if (len > 1 && *str == '-') {
        sign = -1;
        str++, len--;
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

// todo: fix for 64-bits
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

void *dotq(void *cstr) {
    char *p = cstr;
    int n = *p++;
    while (n--) putchar(*p++);
    return p;
}

void dotid(cell nfa) {
    type(nfa + 1, m[nfa] & 31);
    putchar(BL);
    if ((m[nfa] & 0x80)) printf("%% ");
    if ((m[nfa] & 0x20)) printf("(smudged) ");
}

void words(cell link) {
    while ((link = AT(link))) {
        dotid(link + CELL);
    }
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

cell dl_open(cell addr, cell len, cell flags) {
    const char *filename = temp_string(abs(addr), len);
    void *handle = dlopen(filename, flags);
    //printf("dlopen %s returned %p error %s\n", filename, handle, dlerror());
    return (cell) handle;
}

cell dl_sym(cell addr, cell len, cell handle) {
    const char *symbol = temp_string(abs(addr), len);
    void *symaddr = dlsym((void*)handle, symbol);
    return (cell) symaddr;
}

cell dl_call(cell sym, cell nargs, cell *args) {
    typedef cell (*func0)(void);
    typedef cell (*func1)(cell);
    typedef cell (*func2)(cell,cell);
    typedef cell (*func3)(cell,cell,cell);
    typedef cell (*func4)(cell,cell,cell,cell);
    typedef cell (*func5)(cell,cell,cell,cell,cell);
    typedef cell (*func6)(cell,cell,cell,cell,cell,cell);
    switch (nargs) {
        case 1: return ((func1)sym)(args[0]);
        case 2: return ((func2)sym)(args[1], args[0]);
        case 3: return ((func3)sym)(args[2], args[1], args[0]);
        case 4: return ((func4)sym)(args[3], args[2], args[1], args[0]);
        case 5: return ((func5)sym)(args[4], args[3], args[2], args[1], args[0]);
        case 6: return ((func6)sym)(args[5], args[4], args[3], args[2], args[1], args[0]);
        default:return ((func0)sym)();
    }
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

void show_error(int code, const char *msg, const char *here, const struct source *source) {
    if (code == -1) return; // no msg on abort
    putchar('\n');
    if (isfile(source->file)) {
        int col = source->in;
        if (col == source->len) col++;
        printf("%s:%td:%d: ", (char*)abs(source->filename+1), source->line, col);
    }
    char n = *here++ & 31;
    while (n--) putchar(*here++);
    putchar(' ');
    if (code == -2 && msg) {
        n = *msg++;
        while (n--) putchar(*msg++);
    } else {
        printf("error %d", code);
    }
    // show the line and position
//    cr(); type(source->addr, source->len);
//    cr(); for (int i = 0; i < in; i++) emit(' '); putchar('^');
}

int run(int argc, char *argv[]) {
#include "../prims.inc"
}

// Initial dictionary
byte dict[] = {
#ifdef KERNEL
    #include "../kernel.inc"
#else
    #include "../forth.inc"
#endif
};

void load_image(const char *filename) {
    printf("load image %s\n", filename);
    FILE *f = fopen(filename, "r");
    if (!f) {
        printf("can't open image %s\n", filename);
        return;
    }
    int x = fread(M, 1, sizeof M, f);
    (void)x;
    fclose(f);
}

cell getsize(const char *arg) {
    //printf("arg = %s\n", arg);
    char *end;
    cell size = strtoul(arg, &end, 10);
    switch (*end) {
        case 'g':
        case 'G':
            size *= 1024;
        case 'm':
        case 'M':
            size *= 1024;
        case 'k':
        case 'K':
            size *= 1024;
            break;
    }
    cell minsize = sizeof dict + 1000;
    if (size < minsize) size = minsize;
    return aligned(size);
}

int main(int argc, char *argv[]) {
    cell datasize = CELLS(DATASIZE);

    // process args, handle and remove the ones I use
    int fargc = 1;
    char **fargv = calloc(argc, sizeof *argv);
    fargv[0] = argv[0];
    for (int i = 1; i < argc; i++) {
        char *arg = argv[i];
        if (*arg++ == '-') {
            switch (*arg) {
                case 'm':
                    if (!*++arg && ++i < argc)
                        arg = argv[i];
                    datasize = getsize(arg);
                    continue;
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

    //printf("data size = %tu\n", datasize);
    m = malloc(datasize);
    M = (cell*) m;
    R0 = (cell*) (m + datasize);

    memcpy(m, dict, sizeof dict);

    if (verbose > 1) {
        printf("sizeof(source) = %tu\n", sizeof(struct source));
        printf("m = %p, argv = %p, diff = %td\n", m, argv, (byte*)argv-m);
        byte *temp = malloc(100);
        printf("m = %p, malloc = %p, diff = %td\n", m, temp, temp-m);
    }

    return run(fargc, fargv);
}
