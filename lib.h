/* Support library */

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STATIC_ASSERT(cond, msg) \
    typedef char static_assert_##msg[(cond)?1:-1]

#define TRUE -1
#define FALSE 0
#define BL ' '

#define MAXLINE 128

#define SOURCE_CONSOLE      ((FILE*) 0)
#define SOURCE_EVALUATE     ((FILE*)-1)

struct source {
    const char *addr;   // input buffer address
    int len;            // length of input buffer
    int in;             // current parsing offset (>IN)
    FILE *file;         // SOURCE-ID: 0=console, -1=evaluate, else file id

    char *buf;          // input buffer for file or console (malloc)
    char *filename;     // file name during include (malloc)
    int line;           // line # during include
    long offset;        // file offset for save/restore
};

// This is assumed for the memory layout in fvm.c
STATIC_ASSERT(sizeof(struct source) == 32, source_size);

extern struct source *source; // current input source


void dump(const uint8_t *m, int a, int n);

int parse(char c, const char **addr);
int parse_name(const char **addr);
char *word2(char c, char *here);

int accept(char *str, int max);
int refill();
