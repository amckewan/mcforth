#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char m[1000];

int main(int argc, char **argv) {
    char *temp;

    printf("sizeof ptr = %tu\n", sizeof(void*));
    printf("sizeof int = %tu\n", sizeof(int));

    printf("m = %p\n", m);

    temp = malloc(500);
    printf("malloc = %p (0x%tx)\n", temp, temp-m);

    FILE *f = fopen("Makefile", "r");
    printf("file = %p (0x%tx)\n", f, (char*)f-m);
    fclose(f);

    temp = argv[0];
    printf("arg = %p (0x%tx)\n", temp, temp-m);

    printf("local = %p (0x%tx)\n", &temp, (char*)&temp-m);
    printf("argv = %p (0x%tx)\n", argv, (char*)argv-m);

    return 0;
}