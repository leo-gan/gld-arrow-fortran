# Techniques

## The structs are the wire format

`ArrowSchema` and `ArrowArray` are the on-the-wire description, except the
"wire" is the process address space. Field order matches the C Data Interface
header. The Fortran types use `bind(C)` and only interoperable components, so
the compiler does not insert a descriptor or default initializer.

`src/wire/arrow_abi.c` repeats the C structs and returns `sizeof` and nine
schema offsets plus ten array offsets. The test takes `c_loc` of each Fortran
component and requires the same distance from the base. On this ABI both
results are 72 and 80 bytes.

Children and the dictionary are null pointers. A primitive numeric array has
no child buffer vector. Import treats a non-zero child count, a non-null
child pointer, or a dictionary as `ARROW_ERR_TYPE`, and still calls release
so the producer is not leaked.

## Buffers

`n_buffers` is 2 for every type here, including bool and including length 0.

| Index | Contents |
| --- | --- |
| 0 | Validity bitmap, or null if `null_count` is 0 |
| 1 | Values. Null when `length` is 0 |

The value pointer is 64-byte aligned when the length is positive.
`aligned_alloc` is given a size rounded up to a multiple of 64, which is what
that function requires. `free` releases it later. The logical number of value
bytes is `length * width`, or `(length + 7) / 8` for bool. Padding past that
length is not part of the array.

`offset` is always 0 on export. Import rejects any other offset with
`ARROW_ERR_OFFSET` instead of sliding the pointer. A non-zero offset would
make the freed address different from the viewed address, and this library
does not keep a separate base pointer for that case.

## One numeric allocation

Export of a fixed-width array does two memory operations on the values:

1. `aligned_alloc` of the value buffer.
2. `memcpy` from the caller's contiguous array into that buffer.

There is no staging array in front of that copy. Format and the name
`values` are static C strings, so they are not allocated per call. The
metadata blob, the two-pointer buffer vector, and the two private blocks are
small allocations. They are not the numeric buffer.

Import of a fixed-width array that this library produced recognizes the magic
in `private_data`, takes the value pointer, and clears the own-data flag.
Release then frees the scaffolding and does not free the values. The pointer
returned to Fortran is that same allocation. The caller passes it to
`arrow_free`.

`arrow_free` keeps one numeric allocation. Export reuses it only when it is
at most twice the new request, so release cannot throw away a large block that
was borrowed for a smaller packed bool buffer. Import may reuse a larger block,
because the caller, not release, frees that pointer. Anything `arrow_free` does
not remember, including a validity mask from plain `malloc`, is passed to
`free`. Reuse is process-local and not safe to share across threads. It does
not change values: the buffer is overwritten before it is published. An
`atexit` handler frees that one cached block when the process exits.

A foreign `ArrowArray` whose private data is not ours is copied once with
`memcpy` into a new aligned buffer, then the foreign release callback is
invoked. The tests do not construct a foreign producer. The copy path is
there so import does not `free` memory it does not own.

Bool cannot adopt the packed buffer as a `logical(c_bool)` array. Export
still uses one packed allocation. Import fills one unpacked array, newly
allocated or reused, and lets release free the packed buffer.

## Validity bits

Bit `i` lives in byte `i / 8` at bit `i mod 8`, counting from the least
significant bit. A set bit means the value is valid. That is the same order
Arrow uses for bool values and for the validity bitmap.

Export builds the bitmap only when the mask contains at least one false.
Otherwise the bitmap pointer stays null and `null_count` is 0. Null value
slots are written as zero bytes (or a cleared bool bit) so the payload is
deterministic.

The mask passed to Fortran is not the bitmap. It is one `logical(c_bool)` per
element. That expansion is a separate allocation from the numeric buffer.

## Column-major rank 2

A rank-2 dummy is accepted only when `is_contiguous` is true. The address of
element `(1, 1)` is then the base of the column-major sequence. Export copies
that sequence as a flat primitive of length `size(a)`. It does not build a
fixed-size list child.

Metadata is the Arrow metadata blob: a little-endian `int32` pair count,
then for each pair a little-endian length and that many bytes, with no
trailing NUL on the key or the value. The keys are `gld.rank` and `gld.dims`.
For a 2 by 4 matrix the values are `2` and `2,4`. Import parses those strings
and checks that the product equals `ArrowArray%length`. The view passed back
is shaped `(2, 4)`.

Rank 1 writes the same keys (`1` and the length) so a rank-2 import of a
vector fails instead of guessing.

## Release callback

The callback is a `bind(C)` module procedure. Export stores `c_funloc` of it
on the struct. Import does not call the procedure by its Fortran name. It
converts the function pointer with `c_f_procpointer` and calls that, which is
what a consumer is supposed to do with any producer.

The callback is idempotent at the door: if `release` is already null it
returns. Otherwise it increments the optional counter, frees owned pointers,
frees the private block, and stores a null `release`. Schema and array have
separate private blocks and separate counters. The tests require each counter
to be 1 after import and 0 before it.

## Endian check

A 16-bit integer with the value 1 is transferred to two bytes. The first byte
must be 1. The result is cached for later calls. Multi-byte numeric export
then trusts `memcpy`. The check does not swap a big-endian buffer into
little-endian; that path is rejected.

## Type table

`src/codegen/arrow_kinds.f90` is the map from an internal type id to a format
character and a byte width. Bool's width is 0, meaning bits. The public
procedures are thin wrappers that pass the id, the rank, and `c_loc` of the
first element into one export routine and one import routine. Unsigned ids
share the signed Fortran kind and differ only in the format character
(`C` versus `c`, and so on).
