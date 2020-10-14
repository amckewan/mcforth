/*
 * Files
 *
 * Andrew McKewan 2020
 */

#include "fvm.h"
#include <readline/readline.h>
#include <readline/history.h>

cell accept(cell addr_va, cell max) {
    char *line = readline(0);
    if (!line) return -1;

    int len = strlen(line);
    if (len && line[len-1] == '\n') len--;
    if (len > max) len = max;
    memcpy(phys(addr_va), line, len);

    add_history(line);
    free(line);
    return len;
}

static int refill_tib(struct source *source) {
    int len = accept(source->buf, MAXLINE);
    if (len < 0) return FALSE;
    source->addr = source->buf;
    source->len = len;
    source->in = 0;
    return TRUE;
}

static int refill_file(struct source *source) {
    printf("refill file = %p line = %d\n", source->file, source->line);
    char *buf = phys(source->buf);
    if (!fgets(buf, MAXLINE, source->file)) return FALSE;
    int len = strlen(buf);
    if (len && buf[len-1] == '\n') len--;
    source->line++;
    source->addr = source->buf;
    source->len = len;
    source->in = 0;
    return TRUE;
}

cell refill(cell source_va) {
    struct source *source = phys(source_va);
    //printf("refill source = %p\n", source);
    //printf("file = %p\n", source->file);
    //printf("buf = 0x%X\n", source->buf);
    //printf("addr = 0x%X\n", source->addr);
    //exit(0);
    if (source->file == SOURCE_EVALUATE)
        return FALSE;
    if (source->file == SOURCE_CONSOLE)
        return refill_tib(source);
    return refill_file(source);
}
