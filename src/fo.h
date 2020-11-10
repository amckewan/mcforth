/* common definitions */

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STATIC_ASSERT(cond, msg) \
    typedef char static_assert_##msg[(cond)?1:-1]

STATIC_ASSERT(sizeof(void*) == sizeof(int32_t), cell_is_32_bits);

typedef int32_t cell;
typedef uint32_t ucell;
typedef unsigned char uchar;
typedef uint8_t byte;
typedef uint8_t opcode;

extern byte *const m; // forth memory
extern int verbose;

#define abs(va)    (void *)(m + (va))
#define rel(pa)    (cell)((byte*)(pa) - m)

#define TRUE -1
#define FALSE 0
#define BL ' '
#define EOL '\n'

#define MAXLINE 128

#define SOURCE_CONSOLE      ((FILE*) 0)
#define SOURCE_EVALUATE     ((FILE*)-1)

struct source {
    cell in;            // current parsing offset (>IN)
    cell len;           // length of input buffer
    cell addr;          // input buffer address
    FILE *file;         // SOURCE-ID: 0=console, -1=evaluate, else file id

    cell filename;      // file name, null terminated (malloc)
    cell line;          // line #
    long offset;        // file offset for save/restore
    cell unused;
};

// This is assumed for the memory layout in fo.c
STATIC_ASSERT(sizeof(struct source) == 32, source_size);

// lib.c
char *new_string(const char *str, int len);
//cell new_string(cell str_va, int len);
void fatal(const char *msg);
void dump(int a, int n, int base);

int source_depth();
void push_source(FILE *file, const char *filename, int len);
void pop_source();
void show_error(const char *msg, const char *here, const struct source *source);
int compare(const char *a1, int n1, const char *a2, int n2);
int search(cell *sp, cell top);

// parse.c
cell parse(cell source_va, char c, cell *start_addr);
cell parse_name(cell source_va, cell *start_addr);
cell word(cell source_va, char c, cell here_va);

// file.c
FILE *open_file(const char *str, int len, const char *mode);
FILE *open_on_path(char **filename, cell len);
cell accept(cell addr_va, cell max);
cell refill(cell source_va);
void type(cell addr, cell len);
