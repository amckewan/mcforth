/* Support library */

#ifndef LIB_H_INCLUDED
#define LIB_H_INCLUDED

// lib.c
char *new_string(const char *str, int len);
void fatal(const char *msg);
void dump(const uint8_t *m, int a, int n);

int source_depth();
void push_source(FILE *file, const char *filename, int len);
void pop_source();

// parse.c
cell parse(cell source_va, char c, cell *start_addr);
cell parse_name(cell source_va, cell *start_addr);
cell word2(cell source_va, char c, cell here_va);

// file.c
cell accept(cell addr_va, cell max);
cell refill(cell source_va);

#endif
