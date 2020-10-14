/* FVM Parsing */

#include "fvm.h"

cell parse(cell source_va, char c, cell *start_addr) {
    struct source *source = phys(source_va);
    const char *src = phys(source->addr);
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
    struct source *source = phys(source_va);
    const char *src = phys(source->addr);
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

cell word2(cell source_va, char c, cell here_va) {
    struct source *source = phys(source_va);
    const char *src = phys(source->addr);
    int in = source->in;
    char *here = phys(here_va);
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

