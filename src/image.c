// Relocate and load images

#include "fo.h"

struct bin {
    const char *filename;
    uint8_t *data;
    cell size;
    cell origin;
};

static int oops(const char *msg) {
    printf("oops: %s\n", msg);
    return -1;
}

static int read_bin(struct bin *bin, const char *filename) {
    printf("reading %s\n", filename);
    FILE *f = fopen(filename, "r");
    if (!f) return oops("can't open it");
    fseek(f, 0, SEEK_END);
    bin->size = ftell(f);
    if (bin->size < 100) return oops("file too small");
    if (bin->size % CELL) printf("warning: size not aligned\n");
    fseek(f, 0, SEEK_SET);
    // allocate an extra zero cell at the end to simplify compare
    bin->data = malloc(bin->size + CELL);
    at(bin->data + bin->size) = 0;
    size_t read = fread(bin->data, 1, bin->size, f);
    fclose(f);
    if (read != bin->size) return oops("read error");
    // we know the first cell contains "here" so we can calculate origin
    cell here = *(cell*)bin->data;
    bin->origin = here - bin->size;
    bin->filename = strdup(filename);
    printf("size = 0x%X origin = 0x%X\n", bin->size, bin->origin);
    return 0;
}

static void compare_bin(struct bin *bin1, struct bin *bin2) {
    uint8_t *data1 = bin1->data;
    uint8_t *data2 = bin2->data;
    cell delta = bin2->origin - bin1->origin;
    printf("delta = %tX\n", delta);
    int size = bin1->size;
    int offset = 0;
    while (offset < size) {
        cell diff = at(data2 + offset) - at(data1 + offset);
        if (diff) {
            // find diff same as delta in next cell bytes
            int j = 1;
            while (diff != delta) {
                if (++j == CELL) break;
                offset++;
                diff = at(data2 + offset) - at(data1 + offset);
            }
            printf("offset %X diff %tX\n", offset, diff);
            offset += CELL;
        } else {
            offset++;
        }
    }
}

int relocate(const char *binfile1, const char *binfile2, const char *imgfile) {
    struct bin bin1, bin2;
    if (read_bin(&bin1, binfile1)) return -1;
    if (read_bin(&bin2, binfile2)) return -1;
    compare_bin(&bin1, &bin2);
    return 0;
}
