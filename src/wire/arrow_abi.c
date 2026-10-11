/* Arrow C Data Interface struct layouts.
   Compiled here only so tests can check sizeof and offsetof.
   This is not libarrow and it does not allocate arrays. */
#include <stddef.h>
#include <stdint.h>

struct ArrowSchema {
    const char *format;
    const char *name;
    const char *metadata;
    int64_t flags;
    int64_t n_children;
    struct ArrowSchema **children;
    struct ArrowSchema *dictionary;
    void (*release)(struct ArrowSchema *);
    void *private_data;
};

struct ArrowArray {
    int64_t length;
    int64_t null_count;
    int64_t offset;
    int64_t n_buffers;
    int64_t n_children;
    const void **buffers;
    struct ArrowArray **children;
    struct ArrowArray *dictionary;
    void (*release)(struct ArrowArray *);
    void *private_data;
};

void gld_abi_sizes(size_t *schema_size, size_t *array_size, size_t *schema_off,
                   size_t *array_off) {
    *schema_size = sizeof(struct ArrowSchema);
    *array_size = sizeof(struct ArrowArray);
    schema_off[0] = offsetof(struct ArrowSchema, format);
    schema_off[1] = offsetof(struct ArrowSchema, name);
    schema_off[2] = offsetof(struct ArrowSchema, metadata);
    schema_off[3] = offsetof(struct ArrowSchema, flags);
    schema_off[4] = offsetof(struct ArrowSchema, n_children);
    schema_off[5] = offsetof(struct ArrowSchema, children);
    schema_off[6] = offsetof(struct ArrowSchema, dictionary);
    schema_off[7] = offsetof(struct ArrowSchema, release);
    schema_off[8] = offsetof(struct ArrowSchema, private_data);
    array_off[0] = offsetof(struct ArrowArray, length);
    array_off[1] = offsetof(struct ArrowArray, null_count);
    array_off[2] = offsetof(struct ArrowArray, offset);
    array_off[3] = offsetof(struct ArrowArray, n_buffers);
    array_off[4] = offsetof(struct ArrowArray, n_children);
    array_off[5] = offsetof(struct ArrowArray, buffers);
    array_off[6] = offsetof(struct ArrowArray, children);
    array_off[7] = offsetof(struct ArrowArray, dictionary);
    array_off[8] = offsetof(struct ArrowArray, release);
    array_off[9] = offsetof(struct ArrowArray, private_data);
}
