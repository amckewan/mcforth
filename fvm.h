/* common definitions */

#ifndef FVM_H_INCLUDED
#define FVM_H_INCLUDED

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

extern byte m[]; // forth memory

#define phys(va)    (void *)(m + (va))
#define virt(pa)    (cell)((pa) - m)

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

    cell buf;           // input buffer for file (malloc)
    cell filename;      // file name, null terminated (malloc)
    cell line;          // line #
    long offset;        // file offset for save/restore
};

// This is assumed for the memory layout in fvm.c
STATIC_ASSERT(sizeof(struct source) == 32, source_size);


#endif
