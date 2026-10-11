# Test data

The files in `testdata/` are test records. They are not Arrow IPC.
`testdata/cases.txt` lists them. The
Fortran test reads that list, builds an array from the payload bytes, exports
it, and checks that the Arrow value buffer matches the payload byte for byte.
Import must return the same values, and each release callback must have run
once.

A record is line-oriented text:

```text
type: int32
rank: 1
shape: 8
null_count: 0
payload_hex: e9 ff ff ff fa ff ff ff 0b 00 00 00 1c 00 00 00 2d 00 00 00 3e 00 00 00 4f 00 00 00 60 00 00 00
validity_hex:
```

`payload_hex` is the little-endian value buffer. Each token is one byte.
Those `int32` bytes are `-23, -6, 11, 28, 45, 62, 79, 96`.
`validity_hex` is empty when `null_count` is 0, and is the packed validity
bitmap otherwise. Rank 2 uses `shape: rows,cols`. The payload is column-major.

The bytes were packed with Python's `struct` module, not by the Fortran code
under test. Signed integers use two's complement. Floats are IEEE-754.
Unsigned integers use the unsigned struct codes, so a `uint8` above 127 is a
byte with the high bit set. The `int32` length-8 payload and the bool
length-8 byte `8d` match the value buffers PyArrow 25.0.1 builds for the same
numbers. PyArrow is not a build dependency and the tests do not import it.

## Length 0, 1, and 8

Every type has three rank-1 records, `*_len0.record`, `*_len1.record`, and
`*_len8.record`, and one 2 by 2 record. Length 0 has an empty payload. Export
must publish a null value pointer and import must return a disassociated
pointer. Length 1 and length 8 check the copy, including a `uint8` sequence
that sits above 127 and a `uint64` sequence at and above `2**63`.

The length-8 sequences, index `i` starting at 1, are:

| Type | Element `i` |
| --- | --- |
| bool | For length 8 only: 1 0 1 1 0 0 0 1. Otherwise 1 on odd `i` |
| int8 | `13*i - 20` |
| int16 | `1000*i - 3000` |
| int32 | `17*i - 40` |
| int64 | `10**10 * i - 3 * 10**10` |
| uint8 | `(30*i + 200) mod 256` |
| uint16 | `40000 + i` |
| uint32 | `3000000000 + i` |
| uint64 | `2**63 + 17*i` |
| float32 | `i + 0.25` |
| float64 | `i + 0.5` |

`int32` length 1 is the value `-23`, bytes `e9 ff ff ff`. The bool length-8
pattern packs to the single byte `8d`: bit 0 set, bit 1 clear, bit 2 set,
bit 3 set, bit 7 set.

`float64_tenth.record` and `float32_tenth.record` are the IEEE bytes of
decimal `0.1`, which is not a dyadic rational. Export must reproduce those
bytes, and import must compare equal. That checks a bit copy, not a decimal
round trip.

## Matrix

`float64_2x4.record` is shape `2,4`. Eight values use the `float64` sequence
above, stored column-major. `array%length` must be 8 and the imported pointer
must have shape `(2, 4)`. Each other type's `*_2x2.record` is the same idea
with four elements. The 2 by 2 bool pattern is odd-index bits, not the
length-8 pattern, and packs to `05`.

## Nulls

`int32_len8_nulls.record` uses the `int32` length-8 sequence but forces
elements 2 and 5 (1-based) to zero and marks them invalid. The validity byte
is `ed`:

| Bit | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Valid | 1 | 0 | 1 | 1 | 0 | 1 | 1 | 1 |

`null_count` is 2. The test checks those bitmap bytes, the zeroed slots, and
the logical mask returned by import.

A separate check, not a record, passes a mask that is all true. The exported
bitmap pointer must be null.

## Other checks in the same program

These are not files under `testdata/`, but they run in `tests/test_roundtrip.f90`:

- Schema size 72, array size 80, and every field offset against the C shim.
- `arrow_host_little_endian()` is true and `ARROW_VERSION` is `0.1.0`.
- A strided rank-1 section and a strided rank-2 section return
  `ARROW_ERR_LAYOUT` without arming release.
- Importing an exported `int32` as `int64` returns `ARROW_ERR_TYPE` and still
  increments both release counters once.

The test does not cover big-endian hosts, non-zero `offset`, child arrays,
or dictionary arrays beyond the rejection path for a wrong format. It does
not read an IPC file.
