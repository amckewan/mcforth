/*
 * Files
 *
 * Andrew McKewan 2020
 */

#include "fo.h"
#include <editline/history.h>

FILE *open_file(const char *str, int len, const char *mode) {
    char *filename = malloc(len + 1);
    memcpy(filename, str, len);
    filename[len] = 0;
    FILE *file = fopen(filename, mode);
    free(filename);
    return file;
}

cell accept(cell addr_va, cell max) {
    char *line = readline(0);
    if (!line) return -1;

    int len = strlen(line);
    if (len && line[len-1] == '\n') len--;
    if (len > max) len = max;
    memcpy(abs(addr_va), line, len);

    // undo newline echoed by readine
    // move cursor to the end of the previous line and erase to the end
    // \033[1A      up 1 line
    // \033[%dC     move to end
    // \033[K       erase to end of line (then add space)
    printf("\033[1A\033[%dC\033[K ", len); fflush(stdout);

    add_history(line);
    free(line);
    return len;
}

static int refill_tib(struct source *source) {
    int len = accept(source->addr, MAXLINE);
    if (len < 0) return FALSE;
    source->len = len;
    source->in = 0;
    return TRUE;
}

static int refill_file(struct source *source) {
    if (verbose) printf(">>> %s:%td\n", (char*)abs(source->filename), source->line+1);
    char *buf = abs(source->addr);
    if (!fgets(buf, MAXLINE, source->file)) {
        source->len = 0;
        source->in = 0;
        return FALSE;
    }
    int len = strlen(buf);
    if (len && buf[len-1] == '\n') len--;
    source->len = len;
    source->in = 0;
    source->line++;
    return TRUE;
}

cell refill(cell source_va) {
    struct source *source = abs(source_va);
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
