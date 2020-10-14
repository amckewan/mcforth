/*
 * FVM support library
 *
 * Andrew McKewan 2020
 */

#include "lib.h"
#include <readline/readline.h>
#include <readline/history.h>

#define MAX_SOURCE_DEPTH 8

static struct source source_stack[MAX_SOURCE_DEPTH+1];
struct source *source; // current input source

/*
 * Source stack handling
 */

char *new_string(const char *str, int len) {
    char *cstr = malloc(len + 1);
    memcpy(cstr, str, len);
    cstr[len] = 0;
    return cstr;
}

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


/*
 * Parsing
 */

int parse(char c, const char **addr) {
    int len = 0;
    int in = source->in;
    *addr = source->addr + in;
    while (in < source->len && source->addr[in] != c) in++, len++;
    if (in < source->len) in++;
    source->in = in;
    return len;
}

int parse_name(const char **addr) {
    const char *src = source->addr;
    int in = source->in;
    int len = 0;
    while (in < source->len && isspace(src[in])) in++;
    *addr = source->addr + in;
    while (in < source->len && !isspace(src[in])) in++, len++;
    if (in < source->len) in++;
    source->in = in;
    return len;
}

char *word2(char c, char *here) {
    const char *src = source->addr;
    char *dest = here;
    int in = source->in;
    if (c == BL) {
        while (in < source->len && isspace(src[in])) {
            in++;
        }
        while (in < source->len && !isspace(src[in])) {
            *++dest = src[in++];
        }
    } else {
        while (in < source->len && src[in] == c) {
            in++;
        }
        while (in < source->len && src[in] != c) {
            *++dest = src[in++];
        }
    }
    if (in < source->len) in++;
    source->in = in;
    *here = dest - here;
    return here;
}

/*
 * Source and include
 */
int accept(char *str, int max) {
    char *line = readline(0);
    if (!line) return -1;

    int len = strlen(line);
    if (len && line[len-1] == '\n') len--;
    if (len > max) len = max;
    memcpy(str, line, len);
    add_history(line);
    free(line);
    return len;
}

static int refill_tib() {
    source->addr = source->buf;
    source->len = accept(source->buf, MAXLINE);
    source->in = 0;
    return source->len >= 0;
}

static int refill_file() {
    if (!fgets(source->buf, MAXLINE, source->file)) return FALSE;
    int len = strlen(source->buf);
    if (len && source->buf[len-1] == '\n') len--;
    source->line++;
    source->addr = source->buf;
    source->len = len;
    source->in = 0;
    return TRUE;
}

int refill() {
    if (source->file == SOURCE_EVALUATE)
        return FALSE;
    if (source->file == SOURCE_CONSOLE)
        return refill_tib();
    return refill_file();
}

/*
 * Tools
 */
void dump(const uint8_t *m, int a, int n) {
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

void fatal(const char *msg) {
    fprintf(stderr, "error: %s\n", msg);
    exit(1);
}
