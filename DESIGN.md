# Design

gld-arrow-fortran moves contiguous numeric Fortran arrays across the Arrow C
Data Interface. That interface is two C structs in a shared address space,
`ArrowSchema` and `ArrowArray`, plus a release callback. It is not Arrow IPC.
Nothing in this library writes a file footer, a record batch message, or a
schema FlatBuffer.

The structs are declared again in Fortran with `bind(C)`. A small C file in
`src/wire` exists only so the tests can compare `sizeof` and `offsetof`. It
is not taken from the Arrow project and it is not linked to libarrow.

## Decisions

| Topic | Choice |
| --- | --- |
| Surface | C Data Interface only. No IPC stream, no IPC file, no Flight |
| Types | bool, signed and unsigned integers 8 through 64, float32, float64 |
| Shape | Rank 1 and rank 2. Length of a matrix is `rows * cols` |
| Order | Fortran column-major. Rank 2 shape is schema metadata, not a list |
| Endian | Little-endian hosts only. A big-endian host returns an error |
| Nulls | Optional mask. Bitmap omitted when `null_count` is 0 |
| Numeric memory | One 64-byte-aligned allocation. Import of a fixed-width array adopts it |
| Bool | Packed LSB-first on export. Import unpacks into `logical(c_bool)` bytes |
| Out of scope | Strings, lists, structs, unions, maps, dictionaries, decimals, non-zero offsets |

## Struct layout

On LP64 the schema is 72 bytes and the array is 80 bytes. Every field is 8
bytes wide, so there is no padding between them. The test compares each
Fortran component offset with the C `offsetof` of the same field.

Primitive arrays publish `n_buffers = 2`. Buffer 0 is the validity bitmap, or
a null pointer when there are no nulls. Buffer 1 is the values. `n_children`
is 0. The dictionary pointer is null. `offset` is 0. Flags include
`ARROW_FLAG_NULLABLE` (2) so a consumer may see a bitmap on a later array of
the same field.

Format strings are the single characters from the C Data Interface spec:
`b c C s S i I l L f g`.

## Rank 2 without a list type

Arrow arrays are logically one-dimensional. This library does not wrap a
matrix in a fixed-size list, because lists are outside the numeric-buffer
scope. A rank-2 export still publishes one primitive array whose `length` is
the element count. Schema metadata, in the Arrow key/value layout, carries:

| Key | Value |
| --- | --- |
| `gld.rank` | `1` or `2` |
| `gld.dims` | `n` or `rows,cols` |

Import of rank 2 reads those keys and views the adopted buffer with shape
`(rows, cols)`. Column `j` occupies elements `[(j-1)*rows + 1 .. j*rows]`.

## Ownership

Export allocates the value buffer with `aligned_alloc(64, ...)`, copies or
packs the caller's array into it, and installs a release callback. The
callback frees the value buffer only when the private block still says the
array owns it. Fixed-width import clears that flag and returns the same
pointer, so the numeric bytes are not copied again. Bool import must unpack
into a byte per logical, so it allocates that unpacked array and lets release
free the packed buffer.

Both callbacks run exactly once from import, including when import rejects
the array. After they run, `release` is a null function pointer. Calling
import on an already released struct returns `ARROW_ERR_STATE`.

The caller frees an adopted or unpacked pointer with `arrow_free`. A length-0
import leaves the pointer disassociated; there is nothing to free. An empty
rank-2 shape is not preserved on that disassociated pointer.

## Private block

The release callback needs the addresses it allocated. They live in a
`bind(C)` block pointed at by `private_data`, marked with a magic integer.
Schema and array do not share one block, so either can be released first.
A counter pointer, when the caller passed one, is incremented inside the
callback before the block is freed.

## What the tests cover

The C shim checks the struct size and every field offset. Each numeric type
is exported and imported at length 0, 1, and 8, plus a 2 by 2 matrix, from
records under `testdata/`. A 2 by 4 `float64` record is the larger matrix.
One `int32` record has two nulls and checks the bitmap bytes. A mask that is
entirely valid must publish a null bitmap. A strided section is rejected.
Importing an `int32` array as `int64` fails and still releases both structs
once. Release counters are 0 after export and 1 after import.

The records are little-endian payloads written independently of the Fortran
packer. They are test records, not a file format.

## Speed

Fixed-width export is one `memcpy` into a 64-byte-aligned buffer. Bool export
packs eight logical bytes with a multiply and a shift. Fixed-width import
adopts that buffer. `arrow_free` may keep the most recent numeric allocation
and hand it back to the next export; the bytes are overwritten before the
array is published. Format and the name `values` are static strings.

`scripts/run-microbench.sh` times ten export-plus-import rounds of 1,048,576
values, release build. The staging copy measured:

| Payload | ns/op | export | import |
| --- | --- | --- | --- |
| float64 | 6374554.6 | 2966442.5 | 3407653.9 |
| bool | 1209152.1 | 458264.7 | 750780.4 |

Fifteen repeats of the final release binary. The table is the repeat whose
float64 time is the median. Bool is that same process.

| Payload | ns/op | export | import |
| --- | --- | --- | --- |
| float64 | 403379.2 | 402827.9 | 471.2 |
| bool | 117042.8 | 77332.1 | 39649.8 |

Across those repeats float64 was 243280 to 1075212 ns/op and bool was 89767
to 150203 ns/op. The float64 spread is the 8 MiB copy. Float64 import stays
under about 2 µs: adopt the buffer and run the two release callbacks. Bool
import writes one byte per logical value and reuses that buffer once it is warm.
