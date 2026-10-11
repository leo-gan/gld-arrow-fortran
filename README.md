# gld-arrow-fortran

Fortran arrays over the [Arrow C Data Interface](https://arrow.apache.org/docs/format/CDataInterface.html).
The library fills and reads `ArrowSchema` and `ArrowArray` for contiguous
little-endian numeric buffers. It does not link libarrow, and it does not
read or write Arrow IPC files or streams.

Version 0.1.0. MIT. Copyright (c) 2026 Leonid Ganeline.

Documentation: [Why Arrow](https://leo-gan.github.io/gld-arrow-fortran/why-arrow/),
[Instructions](https://leo-gan.github.io/gld-arrow-fortran/instructions/),
[Examples](https://leo-gan.github.io/gld-arrow-fortran/examples/),
[Techniques](https://leo-gan.github.io/gld-arrow-fortran/techniques/),
[Test data](https://leo-gan.github.io/gld-arrow-fortran/test-data/).

## Install

From a git checkout, with gfortran and [fpm](https://github.com/fortran-lang/fpm):

```bash
git clone https://github.com/leo-gan/gld-arrow-fortran.git
cd gld-arrow-fortran
scripts/run-tests.sh
```

`fpm build` produces the static library. Programs link it by using the
`gld_arrow` module.

## What it moves

| Surface | Behavior |
| --- | --- |
| Types | `bool`, `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `float32`, `float64` |
| Shape | Rank 1 and rank 2, contiguous, column-major |
| Bytes | Little-endian. One numeric buffer, reused after `arrow_free` |
| Nulls | Optional validity mask. The bitmap is omitted when every value is valid |
| Lifetime | Import calls each release callback once. The caller frees the returned array |

Unsigned values use a Fortran integer of the same width. The bit pattern is
what round-trips, so a `uint8` of 255 is the `integer(c_int8_t)` value `-1`.

## Layout

`src/gld_arrow.f90` is the public facade. `src/wire` is the C ABI struct
layout. `src/schema` is format metadata and the version string. `src/codegen`
is the type-to-format table. `src/runtime` exports, imports, and releases
buffers.

This repository is a standalone library. It is not part of any other project.

## License

MIT. Copyright (c) 2026 Leonid Ganeline.
