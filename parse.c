/* FVM Parsing */

#include "lib.h"

int parse(struct source *source, char c, const char **addr) {
    int len = 0;
    int in = source->in;
    *addr = source->addr + in;
    while (in < source->len && source->addr[in] != c) in++, len++;
    if (in < source->len) in++;
    source->in = in;
    return len;
}

int parse_name(struct source *source, const char **addr) {
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

char *word2(struct source *source, char c, char *here) {
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
    dest[1] = 0;  // null-terminate string
    if (in < source->len) in++;
    source->in = in;
    *here = dest - here;
    return here;
}

