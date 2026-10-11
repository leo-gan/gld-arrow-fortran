# gld-arrow-fortran

gld-arrow-fortran exports and imports contiguous numeric Fortran arrays
through the Arrow C Data Interface. The code is Fortran plus a tiny C file
that only defines the ABI structs for size checks. It does not link libarrow.

This is not Arrow IPC. There is no stream, no file, and no record-batch
message. Two processes cannot share these buffers. Two libraries in one
process can.

<div class="grid cards" markdown="1">

-   __Why Arrow__

    ---

    What the C Data Interface is for, and why this library stops at numeric
    buffers instead of implementing a file format.

    [:octicons-arrow-right-24: Read Why Arrow](why-arrow.md)

-   __Instructions__

    ---

    Build with gfortran and fpm, export an array, import it back, and free it.

    [:octicons-arrow-right-24: Open Instructions](instructions.md)

-   __Examples__

    ---

    A rank-1 vector and a rank-2 matrix, including the release counters.

    [:octicons-arrow-right-24: See Examples](examples.md)

-   __Techniques__

    ---

    Struct layout, validity bits, column-major matrices, and the one numeric
    allocation.

    [:octicons-arrow-right-24: Read Techniques](techniques.md)

-   __Test data__

    ---

    The little-endian records used to check lengths 0, 1, and 8, plus a
    float64 matrix.

    [:octicons-arrow-right-24: Open Test data](test-data.md)

</div>
