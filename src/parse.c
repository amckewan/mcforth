/* Parsing */

#include "fo.h"

cell parse(cell source_va, char c, cell *start_addr) {
    struct source *source = abs(source_va);
    const char *src = abs(source->addr);
    int len = 0;
    int in = source->in;

    *start_addr = source->addr + in;
    while (in < source->len && src[in] != c)
        in++, len++;
    if (in < source->len) in++;
    source->in = in;
    return len;
}

cell parse_name(cell source_va, cell *start_addr) {
    struct source *source = abs(source_va);
    const char *src = abs(source->addr);
    int len = 0;
    int in = source->in;

    while (in < source->len && isspace(src[in]))
        in++;
    *start_addr = source->addr + in;
    while (in < source->len && !isspace(src[in]))
        in++, len++;
    if (in < source->len) in++;
    source->in = in;
    return len;
}

cell word(cell source_va, char c, cell here_va) {
    struct source *source = abs(source_va);
    const char *src = abs(source->addr);
    int in = source->in;
    char *here = abs(here_va);
    char *dest = here;
    if (c == BL) {
        while (in < source->len && isspace(src[in]))
            in++;
        while (in < source->len && !isspace(src[in]))
            *++dest = src[in++];
    } else {
        while (in < source->len && src[in] == c)
            in++;
        while (in < source->len && src[in] != c)
            *++dest = src[in++];
    }
    dest[1] = 0;  // null-terminate string
    if (in < source->len) in++;
    source->in = in;
    *here = dest - here; // count
    return here_va;
}

cell source_position(const struct source *source, cell *column) {
    const char *src = abs(source->addr);
    const char *end = src + source->in;
    // if we've just parsed the last word on a line, >in could
    // now point to the beginning of the next line, so we
    // need to back up one.
    if (end > src && end[-1] == '\n') end--;

    int line = 1;
    int col = 1;
    while (src < end) {
        if (*src++ == '\n') {
            line++;
            col = 0;
        }
        col++;
    }
    if (column) *column = col;
    return line;
}
