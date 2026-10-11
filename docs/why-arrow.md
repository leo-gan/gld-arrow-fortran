# Why Arrow

Apache Arrow is a columnar memory model. A column of `float64` is a length,
a null bitmap, and a contiguous buffer of little-endian IEEE values. Many
languages can read that buffer without parsing a file.

The piece this library implements is the [C Data Interface](https://arrow.apache.org/docs/format/CDataInterface.html).
It is a stable description of one array in the current process:

| Struct | Role |
| --- | --- |
| `ArrowSchema` | Format string, name, metadata, flags, children |
| `ArrowArray` | Length, null count, offset, buffer pointers |
| Release callback | Frees the buffers the producer allocated |

The producer fills the structs. The consumer reads them. The consumer calls
`release` when it is done. After that call the struct must not be used, and
`release` itself is set to null so a second call is obvious.

That is a different job from Arrow IPC. IPC is a stream or a file: a schema
message, record batches, optional dictionaries, and a footer. IPC exists so
two processes, or a process and a disk, can exchange batches. The C Data
Interface exists so two libraries already linked into one process can point
at the same bytes. This repository does the second job only, and only for
plain numeric columns.

## Why a Fortran layer

Fortran already stores `real(c_double)` and `integer(c_int32_t)` in contiguous
columns. The missing piece is the pair of C structs a non-Fortran library
expects. Linking libarrow just to obtain those structs pulls in a large C++
library for a layout that is intentionally small enough to copy by hand.

gld-arrow-fortran declares the structs with `iso_c_binding` and fills them.
Export copies a Fortran array into one aligned buffer. Import reads the
structs and returns a Fortran pointer. The original array the caller passed
to export is not aliased by the returned pointer. Fixed-width import adopts
the buffer export allocated, so the bytes are not copied a second time.

## What is intentionally missing

Strings, lists, structs, unions, maps, and dictionaries need child arrays and
offset buffers. This library rejects `n_children /= 0` and a non-null
dictionary pointer. A rank-2 numeric array is not encoded as a fixed-size
list. Its shape is metadata on a single primitive array. See
[Techniques](techniques.md).

There is no compression, no dictionary encoding, and no device pointer. The
buffers are CPU memory.

## Little-endian numeric values

The Arrow columnar format stores multi-byte numbers little-endian. On a
little-endian host that is the same order as a Fortran `integer(c_int32_t)`
or `real(c_double)`, so export can copy the bytes as they sit. A big-endian
host is rejected with `ARROW_ERR_ENDIAN` rather than silently swapped. This
machine, and the CI runner, are little-endian. The big-endian branch is not
executed by the tests.

Bool is the exception to a raw copy. Fortran `logical(c_bool)` is one byte
per value. Arrow stores bools as bits, least-significant bit first, in the
value buffer. Export packs. Import unpacks.

## A stable boundary

The benefit of stopping at the C Data Interface is a boundary other Arrow
consumers already understand. They see a primitive format string, two
buffers, a length, and a release function. They do not need a Fortran
compiler. This library does not need their allocator except for `malloc`,
`aligned_alloc`, and `free` from the system C library.
