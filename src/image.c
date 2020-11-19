// Create and load dictionary images
// Copyright (c) 2020 Andrew McKewan

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

static int file_size(FILE *f) {
    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    fseek(f, 0, SEEK_SET);
    return size;
}

static int read_bin(struct bin *bin, const char *filename) {
    printf("reading %s\n", filename);
    FILE *f = fopen(filename, "r");
    if (!f) return oops("can't open it");

    bin->size = file_size(f);
    if (bin->size < 100) return oops("file too small");
    if (bin->size % CELL) printf("warning: size not aligned\n");

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
    printf("size = %X origin = %X\n", bin->size, bin->origin);
    return 0;
}

static void mark_bitmap(uint8_t *bitmap, int offset) {
    int i = offset >> 3;
    uint8_t mask = 1 << (offset & 7);
    bitmap[i] |= mask;
}

static uint8_t *compare_bin(struct bin *bin1, struct bin *bin2) {
    uint8_t *data1 = bin1->data;
    uint8_t *data2 = bin2->data;
    cell delta = bin2->origin - bin1->origin;
    printf("delta = %tX\n", delta);
    int size = bin1->size;
    uint8_t *bitmap = calloc(size/8+1, 1);
    int offset = 0;
    int diffs = 0;
    while (offset < size) {
        cell diff = at(data2 + offset) - at(data1 + offset);
        if (diff) {
            // find diff same as delta in next cell bytes
            int j = 0;
            while (diff != delta) {
                //printf("offset %X diff %tX\n", offset, diff);
                if (++j == CELL) {
                    printf("fail: ");
                    printf("offset %X diff %tX\n", offset, diff);
                    exit(-1);
                    return 0;
                }
                offset++;
                diff = at(data2 + offset) - at(data1 + offset);
            }
            printf("offset %X diff %tX\n", offset, diff);
            mark_bitmap(bitmap, offset);
            offset += CELL;
            diffs++;
        } else {
            offset++;
        }
    }
    printf("%d diffs / %d bytes\n", diffs, size);
    return bitmap;
}

static void relocate(uint8_t *image, int size, uint8_t *bitmap, cell delta) {
    uint8_t mask = *bitmap++;
    int bit = 0;
    for (int offset = 0; offset < size; offset++) {
        if (mask & 1) {
            at(image + offset) += delta;
        }
        if (++bit == 8) {
            mask = *bitmap++;
            bit = 0;
        } else {
            mask >>= 1;
        }
    }
}

static int write_image(const char *imgfile, struct bin *bin, uint8_t *bitmap) {
    FILE *f = fopen(imgfile, "w");
    fwrite(bin->data, 1, bin->size, f);
    fwrite(bitmap, 1, bin->size/8, f);
    fclose(f);
    return 0;
}

int create_image(const char *binfile1, const char *binfile2, const char *imgfile) {
    struct bin bin1, bin2;
    if (read_bin(&bin1, binfile1)) return -1;
    if (read_bin(&bin2, binfile2)) return -1;
    uint8_t *bitmap = compare_bin(&bin1, &bin2);
    if (bitmap) {
        cell delta = bin2.origin - bin1.origin;
        relocate(bin2.data, bin2.size, bitmap, -delta);
        write_image(imgfile, &bin2, bitmap);
    }
    return 0;
}

static int relocate_image(uint8_t *image, int bytes_read) {
    if (bytes_read < CELL) return 0;
    int size = at(image);
    if (bytes_read < size + size/8) return 0;
    uint8_t *bitmap = image + size;
    cell delta = (cell) image;
    relocate(image, size, bitmap, delta);
    return size;
}

int load_image(uint8_t *image, int max_size, const char *imgfile) {
    printf("loading %s\n", imgfile);
    FILE *f = fopen(imgfile, "r");
    if (!f) {
        printf("can't open %s\n", imgfile);
        return 0;
    }
    int size = fread(image, 1, max_size, f);
    fclose(f);
    printf("read %d bytes\n", size);
    return relocate_image(image, size);
}
