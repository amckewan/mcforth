/*
 * Files
 *
 * Andrew McKewan 2020
 */

#include "lib.h"
#include <readline/readline.h>
#include <readline/history.h>

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

static int refill_tib(struct source *source) {
    source->addr = source->buf;
    source->len = accept(source->buf, MAXLINE);
    source->in = 0;
    return source->len >= 0;
}

static int refill_file(struct source *source) {
    if (!fgets(source->buf, MAXLINE, source->file)) return FALSE;
    int len = strlen(source->buf);
    if (len && source->buf[len-1] == '\n') len--;
    source->line++;
    source->addr = source->buf;
    source->len = len;
    source->in = 0;
    return TRUE;
}

int refill(struct source *source) {
    if (source->file == SOURCE_EVALUATE)
        return FALSE;
    if (source->file == SOURCE_CONSOLE)
        return refill_tib(source);
    return refill_file(source);
}
