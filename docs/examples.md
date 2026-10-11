# Examples

The program in `examples/export_array.f90` is the one `scripts/run-tests.sh`
runs after the unit tests. It moves a length-4 vector and a 2 by 3 matrix.
Both are `float64`. Nothing is written to disk.

## Vector

```fortran
real(c_double), target :: values(4)
real(c_double), pointer :: back(:)
type(arrow_schema_t) :: schema
type(arrow_array_t) :: array
integer :: status
integer(c_int32_t), target :: schema_hits, array_hits

values = [1.0d0, 2.5d0, -4.0d0, 8.0d0]
schema_hits = 0
array_hits = 0
call arrow_export_float64(values, schema, array, status, &
    release_schema=schema_hits, release_array=array_hits)
```

After a successful export:

- `status` is `ARROW_OK`.
- `schema_hits` and `array_hits` are still 0. Export arms the callbacks and
  does not call them.
- `array%length` is 4, `array%null_count` is 0, `array%n_buffers` is 2.
- `array%buffers` points at two addresses. The first is null because there
  is no validity bitmap. The second is the aligned `float64` buffer.
- `schema%format` is the C string `g`. `schema%name` is `values`.
- `schema%flags` includes `ARROW_FLAG_NULLABLE`.

```fortran
call arrow_import_float64(schema, array, back, status)
```

Import checks the format character, the rank metadata, and the length. It
adopts the value buffer, then calls both release callbacks. `schema_hits`
and `array_hits` become 1. `back(i)` equals `values(i)` for each `i`. The
caller's `values` array is a separate allocation; freeing `back` does not
free it.

```fortran
call arrow_free(c_loc(back(1)))
nullify(back)
```

## Matrix

```fortran
real(c_double), target :: grid(2, 3)
real(c_double), pointer :: back2(:, :)

grid(1, :) = [1.0d0, 3.0d0, 5.0d0]
grid(2, :) = [2.0d0, 4.0d0, 6.0d0]
call arrow_export_float64(grid, schema, array, status, &
    release_schema=schema_hits, release_array=array_hits)
```

`array%length` is 6. The bytes, in order, are `1, 2, 3, 4, 5, 6` as
`float64`. That is column-major. Metadata `gld.dims` is the text `2,3`.

```fortran
call arrow_import_float64(schema, array, back2, status)
```

`back2` has shape `(2, 3)` and the same elements as `grid`. Release runs
once for the schema and once for the array. `arrow_free(c_loc(back2(1, 1)))`
frees the adopted buffer.

## A null mask

This snippet is the same pattern as the all-valid and the two-null tests.
`valid` uses `logical(c_bool)` and matches the value shape. `.true.` means
the slot is present.

```fortran
integer(c_int32_t), target :: src(4)
logical(c_bool), target :: valid(4)
integer(c_int32_t), pointer :: back(:)
logical(c_bool), pointer :: back_valid(:)

src = [10, 20, 30, 40]
valid = [.true., .false., .true., .true.]
call arrow_export_int32(src, schema, array, status, valid=valid)
```

`array%null_count` is 1. Buffer 0 is four bits in one byte, least-significant
bit first: bit 0 set, bit 1 clear, bit 2 set, bit 3 set. The value at index
2 is stored as integer 0. Import with `valid=back_valid` returns both the
values and a byte mask. Free each pointer.

If every mask entry is `.true.`, export drops the bitmap and sets
`null_count` to 0. Import with a mask argument then returns a mask of all
true values, allocated separately from the numeric buffer.

## Bool bits

```fortran
logical(c_bool), target :: bits(8)
logical(c_bool), pointer :: back(:)
bits = [.true., .false., .true., .true., .false., .false., .false., .true.]
call arrow_export_bool(bits, schema, array, status)
```

The value buffer is one byte, `Z'8D'`. Bit 0 is the first element. Import
returns eight `logical(c_bool)` values, not the packed byte. That unpacked
array is a new allocation. The packed byte is freed when import calls
release.

## Building the example

```bash
fpm run export_array
```

The program prints the four vector values and the matrix shape, or stops
with a non-zero status if a check fails.
