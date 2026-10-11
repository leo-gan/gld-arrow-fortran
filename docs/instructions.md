# Instructions

## Requirements

- gfortran 11 or newer. CI installs gfortran from apt on `ubuntu-latest`.
- [fpm](https://github.com/fortran-lang/fpm) 0.13 on `PATH`. The local
  checkout uses `~/.local/bin` when that directory exists.
- A little-endian CPU. Export and import return `ARROW_ERR_ENDIAN` otherwise.
- `logical(c_bool)` must be one byte. gfortran's `c_bool` is.

No libarrow package is required.

## Build and test

```bash
git clone https://github.com/leo-gan/gld-arrow-fortran.git
cd gld-arrow-fortran
scripts/run-tests.sh
```

That runs `fpm test` and the `export_array` example. The test prints
`records=` and then `ok`.

The microbench prints nanoseconds per export-plus-import:

```bash
scripts/run-microbench.sh
```

The payload is one million `float64` values, then one million bools, ten
repetitions, release flags.

Docs, from a virtualenv or a user install of the pinned requirements:

```bash
pip install -r requirements-docs.txt
mkdocs build --strict
```

## Call pattern

```fortran
use, intrinsic :: iso_c_binding, only: c_double, c_loc, c_int32_t
use gld_arrow
real(c_double), target :: values(4)
real(c_double), pointer :: back(:)
type(arrow_schema_t) :: schema
type(arrow_array_t) :: array
integer :: status
integer(c_int32_t), target :: schema_hits, array_hits

values = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
schema_hits = 0
array_hits = 0
call arrow_export_float64(values, schema, array, status, &
    release_schema=schema_hits, release_array=array_hits)
call arrow_import_float64(schema, array, back, status)
call arrow_free(c_loc(back(1)))
```

The array argument must have the `target` attribute. `c_loc` is how export
finds the first byte. It must also be contiguous. A strided section such as
`values(1:8:2)` returns `ARROW_ERR_LAYOUT` and does not arm a release
callback.

Import consumes the structs. It calls the schema release callback once and
the array release callback once, then nulls both function pointers. Do not
call release yourself after a successful or failed import. The optional
`release_schema` and `release_array` integers are incremented from inside
those callbacks, which is how the tests see that release ran once rather than
that the pointer was merely cleared.

`back` is associated when the length is positive. Free the first element with
`arrow_free`, then nullify the pointer. A length of 0 leaves `back`
disassociated. Do not call `arrow_free` in that case.

## Procedures

Each type has a generic with a rank-1 and a rank-2 specific:

| Procedure pair | Fortran element |
| --- | --- |
| `arrow_export_bool`, `arrow_import_bool` | `logical(c_bool)` |
| `arrow_export_int8`, `arrow_import_int8` | `integer(c_int8_t)` |
| `arrow_export_int16`, `arrow_import_int16` | `integer(c_int16_t)` |
| `arrow_export_int32`, `arrow_import_int32` | `integer(c_int32_t)` |
| `arrow_export_int64`, `arrow_import_int64` | `integer(c_int64_t)` |
| `arrow_export_uint8`, `arrow_import_uint8` | `integer(c_int8_t)` bit pattern |
| `arrow_export_uint16`, `arrow_import_uint16` | `integer(c_int16_t)` bit pattern |
| `arrow_export_uint32`, `arrow_import_uint32` | `integer(c_int32_t)` bit pattern |
| `arrow_export_uint64`, `arrow_import_uint64` | `integer(c_int64_t)` bit pattern |
| `arrow_export_float32`, `arrow_import_float32` | `real(c_float)` |
| `arrow_export_float64`, `arrow_import_float64` | `real(c_double)` |

Unsigned and signed procedures are separate names because the Fortran types
are the same width. A `uint8` value of 200 is stored as the two's-complement
byte `200`. Compared as a signed integer it is negative. Compared as bits it
is unchanged.

An optional `valid` argument has the same shape as the values. `.true.` means
the element is present. When the argument is absent, or every entry is true,
buffer 0 is a null pointer and `null_count` is 0. When any entry is false,
buffer 0 is a bitmap and null slots in the value buffer are set to zero bits.

Import accepts the same optional argument as a pointer. It is associated with
a byte per element, not with the packed bitmap. Free it with `arrow_free` too.

## Status codes

| Name | Value | When |
| --- | --- | --- |
| `ARROW_OK` | 0 | Success |
| `ARROW_ERR_ENDIAN` | 1 | Host is not little-endian |
| `ARROW_ERR_ALLOC` | 2 | `malloc` or `aligned_alloc` failed |
| `ARROW_ERR_TYPE` | 3 | Format, children, or dictionary is not a primitive this library handles |
| `ARROW_ERR_SHAPE` | 4 | Rank, dimensions, or null count do not match the buffers |
| `ARROW_ERR_STATE` | 5 | Import saw a struct whose release callback was already null |
| `ARROW_ERR_OFFSET` | 6 | `ArrowArray%offset` is not 0 |
| `ARROW_ERR_LAYOUT` | 7 | The Fortran array is not contiguous |
| `ARROW_ERR_NULL` | 8 | A required buffer pointer is null |

`ARROW_VERSION` is the string from `fpm.toml`, currently `0.1.0`.

## Rank 2

```fortran
real(c_double), target :: grid(2, 3)
real(c_double), pointer :: back(:, :)
call arrow_export_float64(grid, schema, array, status)
call arrow_import_float64(schema, array, back, status)
```

`array%length` is 6. `back` has shape `(2, 3)`. Memory order is column-major:
`grid(1,1)`, `grid(2,1)`, `grid(1,2)`, and so on. The schema metadata keys
`gld.rank` and `gld.dims` record that shape. A consumer that ignores metadata
sees a flat primitive of length 6, which is the correct numeric buffer.

## What not to pass

- Non-contiguous sections. They return `ARROW_ERR_LAYOUT`.
- Arrays without `target`. That is a compile error at `c_loc`, not a status code.
- An already released schema or array.
- Child arrays, dictionary arrays, or a non-zero offset. Import releases them
  and returns an error, so the producer memory is still freed.
