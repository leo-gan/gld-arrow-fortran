program test_roundtrip
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: error_unit
  use gld_arrow
  implicit none
  call check_abi()
  call check_endian_and_version()
  call check_layout_reject()
  call check_mismatch_releases()
  call check_all_valid_omits_bitmap()
  call check_two_live()
  call check_pool_loop()
  call run_records()
  write(*, "(A)") "ok"
contains

  subroutine die(msg)
    character(len=*), intent(in) :: msg
    write(error_unit, "(A)") "FAIL: " // trim(msg)
    error stop 1
  end subroutine die

  function code_to_i8(code) result(b)
    integer, intent(in) :: code
    integer(c_int8_t) :: b
    character(1) :: ch
    integer(c_int8_t) :: tmp(1)
    ch = achar(iand(code, 255))
    tmp = transfer(ch, tmp)
    b = tmp(1)
  end function code_to_i8

  function nibble(ch) result(v)
    character(len=1), intent(in) :: ch
    integer :: v
    integer :: k
    k = iachar(ch)
    if (k >= iachar("0") .and. k <= iachar("9")) then
      v = k - iachar("0")
    else if (k >= iachar("a") .and. k <= iachar("f")) then
      v = k - iachar("a") + 10
    else if (k >= iachar("A") .and. k <= iachar("F")) then
      v = k - iachar("A") + 10
    else
      v = -1
    end if
  end function nibble

  subroutine parse_hex(text, bytes)
    character(len=*), intent(in) :: text
    integer(c_int8_t), allocatable, intent(out) :: bytes(:)
    integer :: i, n, code, hi, lo
    integer, allocatable :: tmp(:)
    character(len=4096) :: buf
    buf = adjustl(text)
    n = 0
    allocate(tmp(2048))
    i = 1
    do while (i <= len_trim(buf))
      if (buf(i:i) == " " .or. buf(i:i) == char(9)) then
        i = i + 1
        cycle
      end if
      if (i >= len_trim(buf)) call die("truncated hex")
      hi = nibble(buf(i:i))
      lo = nibble(buf(i + 1:i + 1))
      if (hi < 0 .or. lo < 0) call die("bad hex digit")
      code = hi * 16 + lo
      n = n + 1
      if (n > size(tmp)) call die("hex too long")
      tmp(n) = code
      i = i + 2
    end do
    allocate(bytes(n))
    do i = 1, n
      bytes(i) = code_to_i8(tmp(i))
    end do
  end subroutine parse_hex

  subroutine paint(dst, payload)
    type(c_ptr), intent(in) :: dst
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), pointer :: bytes(:)
    integer :: n
    n = size(payload)
    if (n <= 0) return
    call c_f_pointer(dst, bytes, [n])
    bytes(1:n) = payload(1:n)
  end subroutine paint

  subroutine unpack_to_logical(payload, n, mask)
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int64_t), intent(in) :: n
    logical(c_bool), allocatable, intent(out) :: mask(:)
    integer :: i, bit, code, idx
    character(1) :: ch
    allocate(mask(int(n)))
    do i = 1, int(n)
      idx = 1 + (i - 1) / 8
      bit = mod(i - 1, 8)
      ch = transfer(payload(idx), ch)
      code = iachar(ch)
      mask(i) = iand(code, ishft(1, bit)) /= 0
    end do
  end subroutine unpack_to_logical

  function width_of(fmt) result(w)
    character(len=1), intent(in) :: fmt
    integer :: w
    select case (fmt)
    case ("b")
      w = 0
    case ("c", "C")
      w = 1
    case ("s", "S")
      w = 2
    case ("i", "I", "f")
      w = 4
    case ("l", "L", "g")
      w = 8
    case default
      w = -1
    end select
  end function width_of

  subroutine expect_bytes(array, slot, expect, label)
    type(arrow_array_t), intent(in) :: array
    integer, intent(in) :: slot
    integer(c_int8_t), intent(in) :: expect(:)
    character(len=*), intent(in) :: label
    type(c_ptr), pointer :: bufs(:)
    integer(c_int8_t), pointer :: got(:)
    integer :: i, n
    call c_f_pointer(array%buffers, bufs, [2])
    n = size(expect)
    if (n == 0) then
      if (c_associated(bufs(slot))) call die(trim(label) // " expected null buffer")
      return
    end if
    if (.not. c_associated(bufs(slot))) call die(trim(label) // " missing buffer")
    call c_f_pointer(bufs(slot), got, [n])
    do i = 1, n
      if (got(i) /= expect(i)) call die(trim(label) // " byte mismatch")
    end do
  end subroutine expect_bytes

  subroutine check_export(schema, array, payload, validity, null_count, n, fmt, label)
    type(arrow_schema_t), intent(in) :: schema
    type(arrow_array_t), intent(in) :: array
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    integer(c_int64_t), intent(in) :: n
    character(len=1), intent(in) :: fmt
    character(len=*), intent(in) :: label
    character(kind=c_char), pointer :: text(:)
    integer(c_intptr_t) :: addr
    type(c_ptr), pointer :: bufs(:)
    integer :: need, width
    integer(c_int8_t), allocatable :: empty(:)
    if (array%length /= n) call die(trim(label) // " length")
    if (array%null_count /= null_count) call die(trim(label) // " null_count")
    if (array%offset /= 0) call die(trim(label) // " offset")
    if (array%n_buffers /= 2) call die(trim(label) // " n_buffers")
    if (array%n_children /= 0 .or. schema%n_children /= 0) call die(trim(label) // " children")
    if (c_associated(schema%children) .or. c_associated(array%children)) then
      call die(trim(label) // " child ptr")
    end if
    if (c_associated(schema%dictionary) .or. c_associated(array%dictionary)) then
      call die(trim(label) // " dictionary")
    end if
    if (schema%flags /= int(ARROW_FLAG_NULLABLE, c_int64_t)) then
      call die(trim(label) // " flags")
    end if
    if (.not. c_associated(schema%format)) call die(trim(label) // " format missing")
    call c_f_pointer(schema%format, text, [2])
    if (text(1) /= fmt .or. text(2) /= c_null_char) call die(trim(label) // " format")
    if (.not. c_associated(schema%name)) call die(trim(label) // " name missing")
    call c_f_pointer(schema%name, text, [7])
    if (text(1) /= "v" .or. text(2) /= "a" .or. text(3) /= "l" .or. &
        text(4) /= "u" .or. text(5) /= "e" .or. text(6) /= "s" .or. &
        text(7) /= c_null_char) call die(trim(label) // " name")
    if (.not. c_associated(schema%metadata)) call die(trim(label) // " metadata")
    width = width_of(fmt)
    if (width < 0) call die(trim(label) // " width")
    if (fmt == "b") then
      need = 0
      if (n > 0) need = int((n + 7) / 8)
    else
      need = int(n) * width
    end if
    if (size(payload) /= need) call die(trim(label) // " payload size")
    if (null_count == 0) then
      if (size(validity) /= 0) call die(trim(label) // " unexpected validity")
    else
      if (size(validity) /= (int(n) + 7) / 8) call die(trim(label) // " validity size")
    end if
    call expect_bytes(array, 2, payload, trim(label) // " data")
    if (null_count == 0) then
      allocate(empty(0))
      call expect_bytes(array, 1, empty, trim(label) // " bitmap")
    else
      call expect_bytes(array, 1, validity, trim(label) // " bitmap")
    end if
    if (n > 0) then
      call c_f_pointer(array%buffers, bufs, [2])
      addr = transfer(bufs(2), addr)
      if (iand(addr, 63_c_intptr_t) /= 0) call die(trim(label) // " align")
    end if
  end subroutine check_export

  function find_file(name) result(path)
    character(len=*), intent(in) :: name
    character(len=512) :: path
    character(len=512) :: cand
    integer :: unit, ios, i
    character(len=32) :: prefix(3)
    path = ""
    prefix(1) = "testdata/"
    prefix(2) = "../testdata/"
    prefix(3) = "../../testdata/"
    do i = 1, 3
      cand = trim(prefix(i)) // trim(name)
      open(newunit=unit, file=trim(cand), status="old", action="read", iostat=ios)
      if (ios == 0) then
        close(unit)
        path = cand
        return
      end if
    end do
  end function find_file

  subroutine read_lines(path, lines, n)
    character(len=*), intent(in) :: path
    character(len=4096), intent(out) :: lines(:)
    integer, intent(out) :: n
    integer :: unit, ios
    character(len=4096) :: line
    n = 0
    open(newunit=unit, file=trim(path), status="old", action="read", iostat=ios)
    if (ios /= 0) call die("cannot open " // trim(path))
    do
      read(unit, "(A)", iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle
      if (line(len_trim(line):len_trim(line)) == char(13)) then
        line(len_trim(line):len_trim(line)) = " "
      end if
      n = n + 1
      if (n > size(lines)) call die("too many lines")
      lines(n) = line
    end do
    close(unit)
  end subroutine read_lines

  subroutine value_of(lines, n, key, val, found)
    character(len=4096), intent(in) :: lines(:)
    integer, intent(in) :: n
    character(len=*), intent(in) :: key
    character(len=4096), intent(out) :: val
    logical, intent(out) :: found
    integer :: i, cut
    character(len=64) :: got
    found = .false.
    val = ""
    do i = 1, n
      cut = index(lines(i), ":")
      if (cut <= 1) cycle
      got = adjustl(lines(i)(1:cut - 1))
      if (trim(got) == trim(key)) then
        val = adjustl(lines(i)(cut + 1:))
        found = .true.
        return
      end if
    end do
  end subroutine value_of

  subroutine run_one(filename)
    character(len=*), intent(in) :: filename
    character(len=512) :: path
    character(len=4096) :: lines(16), val
    integer :: nlines, rank, ios
    integer(c_int64_t) :: d1, d2, null_count
    integer(c_int8_t), allocatable :: payload(:), validity(:)
    character(len=32) :: type_name
    character(len=64) :: label
    logical :: found
    integer :: comma
    integer :: tmp
    path = find_file(filename)
    if (len_trim(path) == 0) call die("missing record " // trim(filename))
    call read_lines(path, lines, nlines)
    call value_of(lines, nlines, "type", val, found)
    if (.not. found) call die("type " // trim(filename))
    type_name = trim(val)
    call value_of(lines, nlines, "rank", val, found)
    if (.not. found) call die("rank " // trim(filename))
    read(val, *, iostat=ios) rank
    if (ios /= 0) call die("rank parse " // trim(filename))
    call value_of(lines, nlines, "shape", val, found)
    if (.not. found) call die("shape " // trim(filename))
    comma = index(val, ",")
    if (comma == 0) then
      read(val, *, iostat=ios) tmp
      if (ios /= 0) call die("shape parse " // trim(filename))
      d1 = int(tmp, c_int64_t)
      d2 = 0
    else
      read(val(1:comma - 1), *, iostat=ios) tmp
      if (ios /= 0) call die("shape d1 " // trim(filename))
      d1 = int(tmp, c_int64_t)
      read(val(comma + 1:), *, iostat=ios) tmp
      if (ios /= 0) call die("shape d2 " // trim(filename))
      d2 = int(tmp, c_int64_t)
    end if
    call value_of(lines, nlines, "null_count", val, found)
    if (.not. found) call die("null_count " // trim(filename))
    read(val, *, iostat=ios) tmp
    if (ios /= 0) call die("null parse " // trim(filename))
    null_count = int(tmp, c_int64_t)
    call value_of(lines, nlines, "payload_hex", val, found)
    if (.not. found) call die("payload " // trim(filename))
    call parse_hex(val, payload)
    call value_of(lines, nlines, "validity_hex", val, found)
    if (.not. found) call die("validity " // trim(filename))
    call parse_hex(val, validity)
    label = trim(filename)

    select case (trim(type_name))
    case ("bool")
      call drive_bool(rank, d1, d2, payload, validity, null_count, label)
    case ("int8")
      call drive_int8(rank, d1, d2, payload, validity, null_count, label)
    case ("int16")
      call drive_int16(rank, d1, d2, payload, validity, null_count, label)
    case ("int32")
      call drive_int32(rank, d1, d2, payload, validity, null_count, label)
    case ("int64")
      call drive_int64(rank, d1, d2, payload, validity, null_count, label)
    case ("uint8")
      call drive_uint8(rank, d1, d2, payload, validity, null_count, label)
    case ("uint16")
      call drive_uint16(rank, d1, d2, payload, validity, null_count, label)
    case ("uint32")
      call drive_uint32(rank, d1, d2, payload, validity, null_count, label)
    case ("uint64")
      call drive_uint64(rank, d1, d2, payload, validity, null_count, label)
    case ("float32")
      call drive_float32(rank, d1, d2, payload, validity, null_count, label)
    case ("float64")
      call drive_float64(rank, d1, d2, payload, validity, null_count, label)
    case default
      call die("unknown type " // trim(type_name))
    end select
  end subroutine run_one

  subroutine run_records()
    character(len=512) :: path
    character(len=4096) :: lines(80)
    integer :: n, i
    path = find_file("cases.txt")
    if (len_trim(path) == 0) call die("missing cases.txt")
    call read_lines(path, lines, n)
    if (n < 1) call die("no cases")
    do i = 1, n
      call run_one(trim(lines(i)))
    end do
    write(*, "(A,I0)") "records=", n
  end subroutine run_records

  subroutine check_off(base, field, expect, name)
    type(c_ptr), intent(in) :: base, field
    integer(c_size_t), intent(in) :: expect
    character(len=*), intent(in) :: name
    integer(c_intptr_t) :: b, f
    b = transfer(base, b)
    f = transfer(field, f)
    if (int(f - b, c_size_t) /= expect) then
      write(error_unit, "(A,I0,A,I0)") "FAIL offset " // trim(name) // " ", &
          f - b, " expected ", expect
      error stop 1
    end if
  end subroutine check_off

  subroutine check_abi()
    type(arrow_schema_t), target :: schema
    type(arrow_array_t), target :: array
    integer(c_size_t) :: schema_size, array_size, schema_off(9), array_off(10)
    call gld_abi_sizes(schema_size, array_size, schema_off, array_off)
    if (schema_size /= c_sizeof(schema)) call die("schema sizeof")
    if (array_size /= c_sizeof(array)) call die("array sizeof")
    if (schema_size /= 72_c_size_t) call die("schema size 72")
    if (array_size /= 80_c_size_t) call die("array size 80")
    call check_off(c_loc(schema), c_loc(schema%format), schema_off(1), "format")
    call check_off(c_loc(schema), c_loc(schema%name), schema_off(2), "name")
    call check_off(c_loc(schema), c_loc(schema%metadata), schema_off(3), "metadata")
    call check_off(c_loc(schema), c_loc(schema%flags), schema_off(4), "flags")
    call check_off(c_loc(schema), c_loc(schema%n_children), schema_off(5), "n_children")
    call check_off(c_loc(schema), c_loc(schema%children), schema_off(6), "children")
    call check_off(c_loc(schema), c_loc(schema%dictionary), schema_off(7), "dictionary")
    call check_off(c_loc(schema), c_loc(schema%release), schema_off(8), "schema release")
    call check_off(c_loc(schema), c_loc(schema%private_data), schema_off(9), "schema private")
    call check_off(c_loc(array), c_loc(array%length), array_off(1), "length")
    call check_off(c_loc(array), c_loc(array%null_count), array_off(2), "null_count")
    call check_off(c_loc(array), c_loc(array%offset), array_off(3), "offset")
    call check_off(c_loc(array), c_loc(array%n_buffers), array_off(4), "n_buffers")
    call check_off(c_loc(array), c_loc(array%n_children), array_off(5), "array n_children")
    call check_off(c_loc(array), c_loc(array%buffers), array_off(6), "buffers")
    call check_off(c_loc(array), c_loc(array%children), array_off(7), "array children")
    call check_off(c_loc(array), c_loc(array%dictionary), array_off(8), "array dictionary")
    call check_off(c_loc(array), c_loc(array%release), array_off(9), "array release")
    call check_off(c_loc(array), c_loc(array%private_data), array_off(10), "array private")
  end subroutine check_abi

  subroutine check_endian_and_version()
    if (.not. arrow_host_little_endian()) call die("host is not little-endian")
    if (ARROW_VERSION /= "0.1.0") call die("version")
  end subroutine check_endian_and_version

  subroutine check_layout_reject()
    integer(c_int32_t), target :: col(8)
    real(c_double), target :: grid(3, 4)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer :: stat
    col = 1
    grid = 1.0d0
    call arrow_export_int32(col(1:8:2), schema, array, stat)
    if (stat /= ARROW_ERR_LAYOUT) call die("rank1 layout")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die("layout armed release")
    end if
    call arrow_export_float64(grid(1:3, 1:4:2), schema, array, stat)
    if (stat /= ARROW_ERR_LAYOUT) call die("rank2 layout")
  end subroutine check_layout_reject

  subroutine check_mismatch_releases()
    integer(c_int32_t), target :: src(1)
    integer(c_int64_t), pointer :: back(:)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat
    src(1) = 7
    cs = 0
    ca = 0
    nullify(back)
    call arrow_export_int32(src, schema, array, stat, release_schema=cs, &
        release_array=ca)
    if (stat /= ARROW_OK) call die("mismatch export")
    call arrow_import_int64(schema, array, back, stat)
    if (stat /= ARROW_ERR_TYPE) call die("mismatch status")
    if (cs /= 1 .or. ca /= 1) call die("mismatch release")
    if (associated(back)) call die("mismatch pointer")
  end subroutine check_mismatch_releases

  subroutine check_all_valid_omits_bitmap()
    integer(c_int32_t), target :: src(4)
    logical(c_bool), target :: mask(4)
    integer(c_int32_t), pointer :: back(:)
    logical(c_bool), pointer :: vback(:)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    type(c_ptr), pointer :: bufs(:)
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i
    integer(c_int8_t), allocatable :: empty(:)
    src = [1_c_int32_t, 2_c_int32_t, 3_c_int32_t, 4_c_int32_t]
    mask = .true.
    cs = 0
    ca = 0
    call arrow_export_int32(src, schema, array, stat, valid=mask, &
        release_schema=cs, release_array=ca)
    if (stat /= ARROW_OK) call die("all-valid export")
    if (array%null_count /= 0) call die("all-valid null_count")
    allocate(empty(0))
    call expect_bytes(array, 1, empty, "all-valid bitmap")
    call c_f_pointer(array%buffers, bufs, [2])
    if (c_associated(bufs(1))) call die("all-valid bitmap ptr")
    call arrow_import_int32(schema, array, back, stat, valid=vback)
    if (stat /= ARROW_OK) call die("all-valid import")
    if (cs /= 1 .or. ca /= 1) call die("all-valid release")
    do i = 1, 4
      if (back(i) /= src(i)) call die("all-valid value")
      if (.not. vback(i)) call die("all-valid mask")
    end do
    call arrow_free(c_loc(back(1)))
    call arrow_free(c_loc(vback(1)))
  end subroutine check_all_valid_omits_bitmap

  subroutine check_two_live()
    integer(c_int32_t), target :: first(4), second(4)
    integer(c_int32_t), pointer :: back1(:), back2(:)
    type(arrow_schema_t) :: schema1, schema2
    type(arrow_array_t) :: array1, array2
    integer(c_int32_t), target :: c1, c2, d1, d2
    integer :: stat, i
    first = [1_c_int32_t, 2_c_int32_t, 3_c_int32_t, 4_c_int32_t]
    second = [9_c_int32_t, 8_c_int32_t, 7_c_int32_t, 6_c_int32_t]
    c1 = 0
    c2 = 0
    d1 = 0
    d2 = 0
    call arrow_export_int32(first, schema1, array1, stat, release_schema=c1, &
        release_array=c2)
    if (stat /= ARROW_OK) call die("two-live export1")
    call arrow_export_int32(second, schema2, array2, stat, release_schema=d1, &
        release_array=d2)
    if (stat /= ARROW_OK) call die("two-live export2")
    if (c1 /= 0 .or. c2 /= 0 .or. d1 /= 0 .or. d2 /= 0) call die("two-live early")
    call arrow_import_int32(schema1, array1, back1, stat)
    if (stat /= ARROW_OK) call die("two-live import1")
    call arrow_import_int32(schema2, array2, back2, stat)
    if (stat /= ARROW_OK) call die("two-live import2")
    if (c1 /= 1 .or. c2 /= 1 .or. d1 /= 1 .or. d2 /= 1) call die("two-live release")
    do i = 1, 4
      if (back1(i) /= first(i)) call die("two-live value1")
      if (back2(i) /= second(i)) call die("two-live value2")
    end do
    call arrow_free(c_loc(back1(1)))
    call arrow_free(c_loc(back2(1)))
  end subroutine check_two_live

  subroutine check_pool_loop()
    integer :: step, stat, i
    integer(c_int32_t), target :: ints(8)
    logical(c_bool), target :: bits(8)
    real(c_double), target :: vals(8)
    integer(c_int32_t), pointer :: iback(:)
    logical(c_bool), pointer :: bback(:)
    real(c_double), pointer :: vback(:)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    do i = 1, 8
      ints(i) = int(i * 3, c_int32_t)
      bits(i) = mod(i, 2) == 1
      vals(i) = real(i, c_double) + 0.1d0
    end do
    do step = 1, 200
      call arrow_export_int32(ints, schema, array, stat)
      if (stat /= ARROW_OK) call die("pool int export")
      call arrow_import_int32(schema, array, iback, stat)
      if (stat /= ARROW_OK) call die("pool int import")
      if (iback(8) /= ints(8)) call die("pool int value")
      call arrow_free(c_loc(iback(1)))
      call arrow_export_bool(bits, schema, array, stat)
      if (stat /= ARROW_OK) call die("pool bool export")
      call arrow_import_bool(schema, array, bback, stat)
      if (stat /= ARROW_OK) call die("pool bool import")
      if (bback(1) .neqv. bits(1) .or. bback(8) .neqv. bits(8)) call die("pool bool value")
      call arrow_free(c_loc(bback(1)))
      call arrow_export_float64(vals, schema, array, stat)
      if (stat /= ARROW_OK) call die("pool f64 export")
      call arrow_import_float64(schema, array, vback, stat)
      if (stat /= ARROW_OK) call die("pool f64 import")
      if (vback(3) /= vals(3)) call die("pool f64 value")
      call arrow_free(c_loc(vback(1)))
    end do
  end subroutine check_pool_loop


  subroutine drive_bool(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    logical(c_bool), allocatable, target :: src1(:)
    logical(c_bool), allocatable, target :: src2(:, :)
    logical(c_bool), pointer :: got1(:)
    logical(c_bool), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    call unpack_to_logical(payload, n, src1)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    call unpack_to_logical(payload, n, flat)
    allocate(src2(int(d1), int(d2)))
    k = 1
    do j = 1, int(d2)
      do i = 1, int(d1)
        if (n == 0) exit
        src2(i, j) = flat(k)
        k = k + 1
      end do
    end do
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_bool(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_bool(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_bool(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_bool(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "b", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_bool(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_bool(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_bool(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_bool(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) .neqv. src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) .neqv. src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_bool

  subroutine drive_int8(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int8_t), allocatable, target :: src1(:)
    integer(c_int8_t), allocatable, target :: src2(:, :)
    integer(c_int8_t), pointer :: got1(:)
    integer(c_int8_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_int8(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int8(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_int8(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int8(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "c", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_int8(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_int8(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_int8(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_int8(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_int8

  subroutine drive_int16(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int16_t), allocatable, target :: src1(:)
    integer(c_int16_t), allocatable, target :: src2(:, :)
    integer(c_int16_t), pointer :: got1(:)
    integer(c_int16_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_int16(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int16(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_int16(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int16(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "s", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_int16(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_int16(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_int16(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_int16(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_int16

  subroutine drive_int32(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int32_t), allocatable, target :: src1(:)
    integer(c_int32_t), allocatable, target :: src2(:, :)
    integer(c_int32_t), pointer :: got1(:)
    integer(c_int32_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_int32(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int32(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_int32(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int32(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "i", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_int32(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_int32(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_int32(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_int32(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_int32

  subroutine drive_int64(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int64_t), allocatable, target :: src1(:)
    integer(c_int64_t), allocatable, target :: src2(:, :)
    integer(c_int64_t), pointer :: got1(:)
    integer(c_int64_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_int64(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int64(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_int64(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_int64(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "l", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_int64(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_int64(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_int64(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_int64(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_int64

  subroutine drive_uint8(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int8_t), allocatable, target :: src1(:)
    integer(c_int8_t), allocatable, target :: src2(:, :)
    integer(c_int8_t), pointer :: got1(:)
    integer(c_int8_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_uint8(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint8(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_uint8(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint8(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "C", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_uint8(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_uint8(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_uint8(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_uint8(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_uint8

  subroutine drive_uint16(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int16_t), allocatable, target :: src1(:)
    integer(c_int16_t), allocatable, target :: src2(:, :)
    integer(c_int16_t), pointer :: got1(:)
    integer(c_int16_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_uint16(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint16(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_uint16(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint16(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "S", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_uint16(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_uint16(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_uint16(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_uint16(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_uint16

  subroutine drive_uint32(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int32_t), allocatable, target :: src1(:)
    integer(c_int32_t), allocatable, target :: src2(:, :)
    integer(c_int32_t), pointer :: got1(:)
    integer(c_int32_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_uint32(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint32(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_uint32(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint32(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "I", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_uint32(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_uint32(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_uint32(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_uint32(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_uint32

  subroutine drive_uint64(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    integer(c_int64_t), allocatable, target :: src1(:)
    integer(c_int64_t), allocatable, target :: src2(:, :)
    integer(c_int64_t), pointer :: got1(:)
    integer(c_int64_t), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_uint64(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint64(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_uint64(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_uint64(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "L", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_uint64(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_uint64(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_uint64(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_uint64(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_uint64

  subroutine drive_float32(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    real(c_float), allocatable, target :: src1(:)
    real(c_float), allocatable, target :: src2(:, :)
    real(c_float), pointer :: got1(:)
    real(c_float), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_float32(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_float32(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_float32(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_float32(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "f", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_float32(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_float32(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_float32(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_float32(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_float32

  subroutine drive_float64(rank, d1, d2, payload, validity, null_count, label)
    integer, intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1, d2
    integer(c_int8_t), intent(in) :: payload(:)
    integer(c_int8_t), intent(in) :: validity(:)
    integer(c_int64_t), intent(in) :: null_count
    character(len=*), intent(in) :: label
    real(c_double), allocatable, target :: src1(:)
    real(c_double), allocatable, target :: src2(:, :)
    real(c_double), pointer :: got1(:)
    real(c_double), pointer :: got2(:, :)
    logical(c_bool), allocatable, target :: mask1(:)
    logical(c_bool), allocatable, target :: mask2(:, :)
    logical(c_bool), allocatable :: flat(:)
    logical(c_bool), pointer :: v1(:)
    logical(c_bool), pointer :: v2(:, :)
    type(arrow_schema_t) :: schema
    type(arrow_array_t) :: array
    integer(c_int32_t), target :: cs, ca
    integer :: stat, i, j, k
    integer(c_int64_t) :: n
    logical :: use_mask

    nullify(got1, got2, v1, v2)
    n = d1
    if (rank == 2) n = d1 * d2
    use_mask = size(validity) > 0

  if (rank == 1) then
    allocate(src1(int(d1)))
    if (n > 0) call paint(c_loc(src1(1)), payload)
    if (use_mask) call unpack_to_logical(validity, n, mask1)
  else
    allocate(src2(int(d1), int(d2)))
    if (n > 0) call paint(c_loc(src2(1, 1)), payload)
    if (use_mask) then
      call unpack_to_logical(validity, n, flat)
      allocate(mask2(int(d1), int(d2)))
      k = 1
      do j = 1, int(d2)
        do i = 1, int(d1)
          mask2(i, j) = flat(k)
          k = k + 1
        end do
      end do
    end if
  end if

    cs = 0_c_int32_t
    ca = 0_c_int32_t
    if (rank == 1) then
      if (use_mask) then
        call arrow_export_float64(src1, schema, array, stat, valid=mask1, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_float64(src1, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    else
      if (use_mask) then
        call arrow_export_float64(src2, schema, array, stat, valid=mask2, &
            release_schema=cs, release_array=ca)
      else
        call arrow_export_float64(src2, schema, array, stat, release_schema=cs, &
            release_array=ca)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " export")
    if (cs /= 0 .or. ca /= 0) call die(trim(label) // " released early")
    call check_export(schema, array, payload, validity, null_count, n, &
        "g", label)
    if (rank == 1) then
      if (use_mask) then
        call arrow_import_float64(schema, array, got1, stat, valid=v1)
      else
        call arrow_import_float64(schema, array, got1, stat)
      end if
    else
      if (use_mask) then
        call arrow_import_float64(schema, array, got2, stat, valid=v2)
      else
        call arrow_import_float64(schema, array, got2, stat)
      end if
    end if
    if (stat /= ARROW_OK) call die(trim(label) // " import")
    if (cs /= 1 .or. ca /= 1) call die(trim(label) // " release count")
    if (c_associated(schema%release) .or. c_associated(array%release)) then
      call die(trim(label) // " release not cleared")
    end if
    if (rank == 1) then
      if (n == 0) then
        if (associated(got1)) call die(trim(label) // " empty pointer")
      else
        do i = 1, int(n)
          if (got1(i) /= src1(i)) call die(trim(label) // " value")
        end do
        call arrow_free(c_loc(got1(1)))
        nullify(got1)
      end if
      if (use_mask) then
        if (.not. associated(v1)) call die(trim(label) // " mask")
        do i = 1, int(n)
          if (v1(i) .neqv. mask1(i)) call die(trim(label) // " mask value")
        end do
        call arrow_free(c_loc(v1(1)))
      end if
    else
      if (n == 0) then
        if (associated(got2)) call die(trim(label) // " empty pointer")
      else
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (got2(i, j) /= src2(i, j)) call die(trim(label) // " value")
          end do
        end do
        call arrow_free(c_loc(got2(1, 1)))
        nullify(got2)
      end if
      if (use_mask) then
        if (.not. associated(v2)) call die(trim(label) // " mask")
        do j = 1, int(d2)
          do i = 1, int(d1)
            if (v2(i, j) .neqv. mask2(i, j)) call die(trim(label) // " mask value")
          end do
        end do
        call arrow_free(c_loc(v2(1, 1)))
      end if
    end if
  end subroutine drive_float64

end program test_roundtrip
