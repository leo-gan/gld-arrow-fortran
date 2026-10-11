module arrow_meta
  use, intrinsic :: iso_c_binding
  use arrow_abi, only: c_malloc, c_free, ptr_plus
  use arrow_kinds, only: ARROW_OK, ARROW_ERR_ALLOC, ARROW_ERR_TYPE
  implicit none
  private
  public :: meta_encode
  public :: meta_decode

contains

  subroutine meta_encode(rank, d1, d2, out, status)
    integer(c_int32_t), intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1
    integer(c_int64_t), intent(in) :: d2
    type(c_ptr), intent(out) :: out
    integer, intent(out) :: status
    character(len=32) :: rank_s, dim_s
    character(len=8), parameter :: k_rank = "gld.rank"
    character(len=8), parameter :: k_dims = "gld.dims"
    integer(c_int8_t), pointer :: buf(:)
    integer :: n, pos, lr, ld, n1, n2

    out = c_null_ptr
    call i64_to_char(int(rank, c_int64_t), rank_s, lr)
    if (rank == 1) then
      call i64_to_char(d1, dim_s, ld)
    else
      call i64_to_char(d1, dim_s, n1)
      dim_s(n1 + 1:n1 + 1) = ","
      call i64_to_char(d2, dim_s(n1 + 2:), n2)
      ld = n1 + 1 + n2
    end if
    n = 4 + (4 + 8 + 4 + lr) + (4 + 8 + 4 + ld)
    out = c_malloc(int(n, c_size_t))
    if (.not. c_associated(out)) then
      status = ARROW_ERR_ALLOC
      return
    end if
    call c_f_pointer(out, buf, [n])
    pos = 1
    call put_i32(buf, pos, 2_c_int32_t)
    call put_kv(buf, pos, k_rank, rank_s(1:lr))
    call put_kv(buf, pos, k_dims, dim_s(1:ld))
    if (pos /= n + 1) then
      call c_free(out)
      out = c_null_ptr
      status = ARROW_ERR_TYPE
      return
    end if
    status = ARROW_OK
  end subroutine meta_encode

  subroutine meta_decode(meta, rank, d1, d2, status)
    type(c_ptr), intent(in) :: meta
    integer(c_int32_t), intent(out) :: rank
    integer(c_int64_t), intent(out) :: d1
    integer(c_int64_t), intent(out) :: d2
    integer, intent(out) :: status
    integer(c_int32_t) :: npairs, klen, vlen, ipair
    integer :: consumed
    character(len=32) :: key, val
    logical :: saw_rank, saw_dims

    rank = 0
    d1 = 0
    d2 = 0
    status = ARROW_OK
    if (.not. c_associated(meta)) return

    consumed = 0
    npairs = load_i32(meta, consumed)
    if (npairs < 0 .or. npairs > 16) then
      status = ARROW_ERR_TYPE
      return
    end if
    saw_rank = .false.
    saw_dims = .false.
    do ipair = 1, npairs
      klen = load_i32(meta, consumed)
      vlen = 0
      if (klen < 0 .or. klen > 32) then
        status = ARROW_ERR_TYPE
        return
      end if
      call load_chars(meta, consumed, klen, key)
      vlen = load_i32(meta, consumed)
      if (vlen < 0 .or. vlen > 32) then
        status = ARROW_ERR_TYPE
        return
      end if
      call load_chars(meta, consumed, vlen, val)
      if (klen == 8 .and. key(1:8) == "gld.rank") then
        if (.not. parse_i32(val(1:vlen), rank)) then
          status = ARROW_ERR_TYPE
          return
        end if
        saw_rank = .true.
      else if (klen == 8 .and. key(1:8) == "gld.dims") then
        if (.not. parse_dims(val(1:vlen), d1, d2)) then
          status = ARROW_ERR_TYPE
          return
        end if
        saw_dims = .true.
      end if
    end do
    if (saw_rank .neqv. saw_dims) then
      status = ARROW_ERR_TYPE
      return
    end if
    if (.not. saw_rank) rank = 0
  end subroutine meta_decode

  subroutine i64_to_char(v, text, n)
    integer(c_int64_t), intent(in) :: v
    character(len=*), intent(out) :: text
    integer, intent(out) :: n
    character(len=20) :: rev
    integer(c_int64_t) :: x
    integer :: nd, i
    if (v <= 0_c_int64_t) then
      text(1:1) = "0"
      n = 1
      return
    end if
    x = v
    nd = 0
    do while (x > 0_c_int64_t)
      nd = nd + 1
      rev(nd:nd) = achar(iachar("0") + int(mod(x, 10_c_int64_t)))
      x = x / 10_c_int64_t
    end do
    do i = 1, nd
      text(i:i) = rev(nd - i + 1:nd - i + 1)
    end do
    n = nd
  end subroutine i64_to_char

  subroutine put_i32(buf, pos, v)
    integer(c_int8_t), intent(inout) :: buf(:)
    integer, intent(inout) :: pos
    integer(c_int32_t), intent(in) :: v
    integer(c_int32_t) :: x
    integer :: k
    x = v
    do k = 0, 3
      buf(pos + k) = int(iand(x, 255_c_int32_t), c_int8_t)
      x = ishft(x, -8)
    end do
    pos = pos + 4
  end subroutine put_i32

  subroutine put_kv(buf, pos, key, val)
    integer(c_int8_t), intent(inout) :: buf(:)
    integer, intent(inout) :: pos
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: val
    integer :: i
    call put_i32(buf, pos, int(len(key), c_int32_t))
    do i = 1, len(key)
      buf(pos) = int(iachar(key(i:i)), c_int8_t)
      pos = pos + 1
    end do
    call put_i32(buf, pos, int(len(val), c_int32_t))
    do i = 1, len(val)
      buf(pos) = int(iachar(val(i:i)), c_int8_t)
      pos = pos + 1
    end do
  end subroutine put_kv

  function load_i32(base, consumed) result(v)
    type(c_ptr), intent(in) :: base
    integer, intent(inout) :: consumed
    integer(c_int32_t) :: v
    integer :: k, code
    v = 0
    do k = 0, 3
      code = load_u8(base, consumed + k)
      v = ior(v, ishft(int(code, c_int32_t), 8 * k))
    end do
    consumed = consumed + 4
  end function load_i32

  subroutine load_chars(base, consumed, n, text)
    type(c_ptr), intent(in) :: base
    integer, intent(inout) :: consumed
    integer(c_int32_t), intent(in) :: n
    character(len=*), intent(out) :: text
    integer :: i
    text = " "
    do i = 1, int(n)
      text(i:i) = achar(load_u8(base, consumed))
      consumed = consumed + 1
    end do
  end subroutine load_chars

  function load_u8(base, offset) result(code)
    type(c_ptr), intent(in) :: base
    integer, intent(in) :: offset
    integer :: code
    integer(c_int8_t), pointer :: b
    character(1) :: ch
    call c_f_pointer(ptr_plus(base, int(offset, c_size_t)), b)
    ch = transfer(b, ch)
    code = iachar(ch)
  end function load_u8

  function parse_i32(text, out) result(ok)
    character(len=*), intent(in) :: text
    integer(c_int32_t), intent(out) :: out
    logical :: ok
    integer :: i, dig
    integer(c_int32_t) :: acc
    ok = len(text) > 0
    acc = 0
    do i = 1, len(text)
      dig = iachar(text(i:i)) - iachar("0")
      if (dig < 0 .or. dig > 9) then
        ok = .false.
        out = 0
        return
      end if
      acc = acc * 10 + int(dig, c_int32_t)
    end do
    out = acc
  end function parse_i32

  function parse_dims(text, d1, d2) result(ok)
    character(len=*), intent(in) :: text
    integer(c_int64_t), intent(out) :: d1
    integer(c_int64_t), intent(out) :: d2
    logical :: ok
    integer :: comma
    d1 = 0
    d2 = 0
    comma = index(text, ",")
    if (comma == 0) then
      ok = parse_i64(text, d1)
      return
    end if
    ok = parse_i64(text(1:comma - 1), d1)
    if (.not. ok) return
    ok = parse_i64(text(comma + 1:), d2)
  end function parse_dims

  function parse_i64(text, out) result(ok)
    character(len=*), intent(in) :: text
    integer(c_int64_t), intent(out) :: out
    logical :: ok
    integer :: i, dig
    integer(c_int64_t) :: acc
    ok = len_trim(text) > 0
    acc = 0
    do i = 1, len_trim(text)
      dig = iachar(text(i:i)) - iachar("0")
      if (dig < 0 .or. dig > 9) then
        ok = .false.
        out = 0
        return
      end if
      acc = acc * 10_c_int64_t + int(dig, c_int64_t)
    end do
    out = acc
  end function parse_i64

end module arrow_meta
