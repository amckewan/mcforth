/* common definitions */

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STATIC_ASSERT(cond, msg) \
    typedef char static_assert_##msg[(cond)?1:-1]

//STATIC_ASSERT(sizeof(void*) == sizeof(int32_t), cell_is_32_bits);

typedef intptr_t cell;
typedef uintptr_t ucell;
typedef uint8_t byte;

#define CELL sizeof(cell)
#define CELLS(n) ((n) * CELL)
#define at(a) *(cell*)(a)
#define AT(va) at(m + (va))
#define FETCH(va) AT(m + (va))

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

#define isfile(fid)     ((ucell)(fid) + 1 > 1u)

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
STATIC_ASSERT(sizeof(struct source) == 8*sizeof(cell), source_size);

// string.c
int compare(const char *a1, int n1, const char *a2, int n2);
int search(cell *sp, cell top);

// parse.c
cell parse(cell source_va, char c, cell *start_addr);
cell parse_name(cell source_va, cell *start_addr);
cell word(cell source_va, char c, cell here_va);

// file.c
FILE *open_file(const char *str, int len, const char *mode);
cell accept(cell addr_va, cell max);
cell refill(cell source_va);
