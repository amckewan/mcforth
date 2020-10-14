/* misc support functions */

#include "fvm.h"

#define MAX_SOURCE_DEPTH 8

//static struct source source_stack[MAX_SOURCE_DEPTH+1];
//struct source *source; // current input source

/*
 * Source stack handling
 */

#if 0
int source_depth() { return source - source_stack; }

void push_source(FILE *file, const char *filename, int len) {
    //printf("push depth = %d\n", source_depth());
    if (source_depth() >= MAX_SOURCE_DEPTH)
        fatal("include nesting too deep");
    ++source;
    source->file = file;
    source->filename = len ? new_string(filename, len) : 0;
    source->line = 0;
}

void pop_source() {
    //printf("pop depth = %d\n", source_depth());
    if (source_depth() < 1)
        fatal("trying to pop empty source stack!");
    if (source->file != SOURCE_CONSOLE && source->file != SOURCE_EVALUATE)
        fclose(source->file);
    free(source->filename);
    --source;
}
#endif

/*
 * Utilities
 */

char *new_string1(const char *str, int len) {
    char *cstr = malloc(len + 1);
    memcpy(cstr, str, len);
    cstr[len] = 0;
    return cstr;
}

cell new_string(cell str_va, int len) {
    char *cstr = malloc(len + 1);
    memcpy(cstr, phys(str_va), len);
    cstr[len] = 0;
    return virt(cstr);
}

void fatal(const char *msg) {
    fprintf(stderr, "error: %s\n", msg);
    exit(1);
}

void dump(int a, int n) {
    int i, j;
    for (i = 0; i < n; i += 16, a += 16) {
        printf("%04X ", a);
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

