// fvm.c

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BL ' '
#define EOL '\n'

typedef int cell;
typedef unsigned int ucell;
typedef unsigned char uchar;

#define CELL sizeof(cell)

typedef struct {
    uchar *addr;
    cell len;
    cell in;
} Input;

typedef struct {
    cell link;
    uchar count;
    uchar name[1];
} Header;

uchar m[0x2000]; // This is the dictionary

#define M(n) *(cell *)&m[n]
#define NEXT goto next;
#define push *++sp = top, top =
#define pop top = *sp--
#define pop2 top = sp[-1], sp -= 2
#define LOGICAL ? -1 : 0
#define aligned(x) (((cell)(x) + (CELL - 1)) & ~(CELL - 1))
#define c(x) HERE = x, HERE += CELL

// Memory Map
#define HERE M(4)
#define SOURCE M(8)
#define CONTEXT 20

uchar *word(cell delim, Input *input, uchar *here) {
    uchar *p = input->addr + input->in;
    cell n = input->len - input->in;

    //printf("tib='%s' len=%d >in=%d\n", input->addr, input->len, input->in);

    uchar *q = here + 1;
    int i;

    if (delim == BL) {
        while (n > 0 && isspace(*p)) p++, n--;

        for (i = 0; i < n && !isspace(*p); i++) *q++ = *p++;
    } else {
        while (n > 0 && *p == delim) p++, n--;

        for (i = 0; i < n && *p != delim; i++) *q++ = *p++;
    }

    if (i < n) p++;
    input->in = p - input->addr;

    *q = 0;           // null-terminate string
    *here = (uchar)i; // store count byte
    //printf("word='%s' >in=%d\n", here+1, input->in);
    return here;
}

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

int accept(cell addr, cell len) {
    char *str = fgets((char *)m + addr, len, stdin);
    if (!str) exit (0);
    len = strlen(str);
    if (len && str[len-1] == EOL) str[--len] = 0;
    return len;
}

cell *litq(cell *ip) {
    uchar *p = (uchar *)ip;
    return (cell *)aligned(p + *p + 1);
}

cell *dotq(cell *ip) {
    char *p = (char *)ip;
    int n = *p++;
    while (n--) putchar(*p++);
    return (cell *)aligned(p);
}

void dotid(cell nfa) {
    type(nfa + 1, m[nfa] & 31);
    putchar(BL);
}

void words(cell v) {
    cell link = M(CONTEXT + v * CELL);
    while (link) {
        dotid(link + CELL);
        link = M(link);
    }
}

void dump(cell a, cell n) {
    cell i, j;
    for (i = 0; i < n; i += 16, a += 16) {
        printf("%04X  ", a);
        for (j = 0; j < 16; j++) printf("%02X ", m[a + j]);
        printf(" ");
        for (j = 0; j < 16; j++) printf("%c", isprint(m[a + j]) ? m[a + j] : '.');
        printf("\n");
    }
}

void fvm() {
    cell stack[100], *sp, top;
    cell rack[100], *rp;
    cell *ip, w;

    printf("hi\n");

abort:
    sp = stack;
    *sp = top = 0;
    rp = rack;
    ip = (cell *)&m[0x200];

next:
    w = *ip++;
exec:
    switch (w) {
#include "kernel.c"

        default: // call

            if (w < 0x200) {
                printf("Invalid opcode 0x%X\n", w);
                goto abort;
            }

            *++rp = (cell)ip;
            ip = (cell *)(m + w);
            NEXT
    }
}

int main(int argc, char *argv[]) {
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
