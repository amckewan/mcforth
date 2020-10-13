/*
 * FVM support library
 *
 * Andrew McKewan 2020
 */

#include "lib.h"

void dump(const uint8_t *m, int a, int n) {
    int i, j;
    for (i = 0; i < n; i += 16, a += 16) {
        printf("%04X ", a);
        for (j = 0; j < 16; j++) {
            if (j % 4 == 0) putchar(' ');
            printf("%02X ", m[a + j]);
        }
        printf(" ");
        for (j = 0; j < 16; j++) putchar(isprint(m[a + j]) ? m[a + j] : '.');
        putchar('\n');
    }
}
