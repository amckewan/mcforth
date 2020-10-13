/*
 * FVM support library
 *
 * Andrew McKewan 2020
 */

#include "lib.h"
#include <readline/readline.h>
#include <readline/history.h>

struct source *source; // current input source


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
    unsigned char *src = (unsigned char *)source->addr;
    int in = source->in;
    int len = 0;
    while (in < source->len && src[in] <= BL) in++;
    *addr = source->addr + in;
    while (in < source->len && src[in] > BL) in++, len++;
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
    source->in = (in < source->len) ? in + 1 : in;
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

