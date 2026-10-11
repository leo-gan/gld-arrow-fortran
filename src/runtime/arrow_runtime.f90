module arrow_runtime
  use, intrinsic :: iso_c_binding
  use arrow_abi
  use arrow_kinds
  use arrow_meta
  implicit none
  private

  public :: arrow_export_bool
  public :: arrow_import_bool
  public :: arrow_export_int8
  public :: arrow_import_int8
  public :: arrow_export_int16
  public :: arrow_import_int16
  public :: arrow_export_int32
  public :: arrow_import_int32
  public :: arrow_export_int64
  public :: arrow_import_int64
  public :: arrow_export_uint8
  public :: arrow_import_uint8
  public :: arrow_export_uint16
  public :: arrow_import_uint16
  public :: arrow_export_uint32
  public :: arrow_import_uint32
  public :: arrow_export_uint64
  public :: arrow_import_uint64
  public :: arrow_export_float32
  public :: arrow_import_float32
  public :: arrow_export_float64
  public :: arrow_import_float64
  public :: arrow_free
  public :: arrow_host_little_endian

  interface arrow_export_bool
    module procedure export_bool_1
    module procedure export_bool_2
  end interface arrow_export_bool
  interface arrow_import_bool
    module procedure import_bool_1
    module procedure import_bool_2
  end interface arrow_import_bool
  interface arrow_export_int8
    module procedure export_int8_1
    module procedure export_int8_2
  end interface arrow_export_int8
  interface arrow_import_int8
    module procedure import_int8_1
    module procedure import_int8_2
  end interface arrow_import_int8
  interface arrow_export_int16
    module procedure export_int16_1
    module procedure export_int16_2
  end interface arrow_export_int16
  interface arrow_import_int16
    module procedure import_int16_1
    module procedure import_int16_2
  end interface arrow_import_int16
  interface arrow_export_int32
    module procedure export_int32_1
    module procedure export_int32_2
  end interface arrow_export_int32
  interface arrow_import_int32
    module procedure import_int32_1
    module procedure import_int32_2
  end interface arrow_import_int32
  interface arrow_export_int64
    module procedure export_int64_1
    module procedure export_int64_2
  end interface arrow_export_int64
  interface arrow_import_int64
    module procedure import_int64_1
    module procedure import_int64_2
  end interface arrow_import_int64
  interface arrow_export_uint8
    module procedure export_uint8_1
    module procedure export_uint8_2
  end interface arrow_export_uint8
  interface arrow_import_uint8
    module procedure import_uint8_1
    module procedure import_uint8_2
  end interface arrow_import_uint8
  interface arrow_export_uint16
    module procedure export_uint16_1
    module procedure export_uint16_2
  end interface arrow_export_uint16
  interface arrow_import_uint16
    module procedure import_uint16_1
    module procedure import_uint16_2
  end interface arrow_import_uint16
  interface arrow_export_uint32
    module procedure export_uint32_1
    module procedure export_uint32_2
  end interface arrow_export_uint32
  interface arrow_import_uint32
    module procedure import_uint32_1
    module procedure import_uint32_2
  end interface arrow_import_uint32
  interface arrow_export_uint64
    module procedure export_uint64_1
    module procedure export_uint64_2
  end interface arrow_export_uint64
  interface arrow_import_uint64
    module procedure import_uint64_1
    module procedure import_uint64_2
  end interface arrow_import_uint64
  interface arrow_export_float32
    module procedure export_float32_1
    module procedure export_float32_2
  end interface arrow_export_float32
  interface arrow_import_float32
    module procedure import_float32_1
    module procedure import_float32_2
  end interface arrow_import_float32
  interface arrow_export_float64
    module procedure export_float64_1
    module procedure export_float64_2
  end interface arrow_export_float64
  interface arrow_import_float64
    module procedure import_float64_1
    module procedure import_float64_2
  end interface arrow_import_float64

  integer(c_int32_t), parameter :: GLD_MAGIC = 1196319745_c_int32_t

  type, bind(C) :: arrow_owned
    integer(c_int32_t) :: magic
    integer(c_int32_t) :: owns_data
    integer(c_int32_t) :: owns_format
    integer(c_int32_t) :: owns_name
    integer(c_int32_t) :: owns_meta
    integer(c_int32_t) :: owns_bitmap
    integer(c_int32_t) :: owns_buffers
    integer(c_int32_t) :: pad
    type(c_ptr) :: format_ptr
    type(c_ptr) :: name_ptr
    type(c_ptr) :: metadata_ptr
    type(c_ptr) :: buffers_ptr
    type(c_ptr) :: data_ptr
    type(c_ptr) :: bitmap_ptr
    type(c_ptr) :: counter
  end type arrow_owned

  abstract interface
    subroutine schema_release_iface(schema) bind(C)
      import :: arrow_schema_t
      type(arrow_schema_t), intent(inout) :: schema
    end subroutine schema_release_iface
    subroutine array_release_iface(array) bind(C)
      import :: arrow_array_t
      type(arrow_array_t), intent(inout) :: array
    end subroutine array_release_iface
  end interface

  type(c_ptr), save :: pool_user = c_null_ptr
  integer(c_int64_t), save :: pool_cap = 0
  type(c_ptr), save :: note_ptr = c_null_ptr
  integer(c_int64_t), save :: note_cap = 0
  logical, save :: pool_exit_armed = .false.

contains

  function arrow_host_little_endian() result(yes)
    logical :: yes
    integer(c_int16_t) :: probe
    character(1) :: bytes(2)
    logical, save :: ready = .false.
    logical, save :: cached = .false.
    if (.not. ready) then
      probe = 1_c_int16_t
      bytes = transfer(probe, bytes)
      cached = iachar(bytes(1)) == 1
      ready = .true.
    end if
    yes = cached
  end function arrow_host_little_endian

  subroutine arrow_free(ptr)
    type(c_ptr), intent(in) :: ptr
    call recycle_user(ptr)
  end subroutine arrow_free

  subroutine arm_pool_exit()
    interface
      function c_atexit(fun) bind(C, name="atexit")
        import :: c_funptr, c_int
        type(c_funptr), value :: fun
        integer(c_int) :: c_atexit
      end function c_atexit
    end interface
    integer(c_int) :: rc
    if (pool_exit_armed) return
    rc = c_atexit(c_funloc(gld_pool_cleanup))
    if (rc == 0) pool_exit_armed = .true.
  end subroutine arm_pool_exit

  subroutine gld_pool_cleanup() bind(C)
    if (c_associated(pool_user)) then
      call c_free(pool_user)
      pool_user = c_null_ptr
      pool_cap = 0_c_int64_t
    end if
  end subroutine gld_pool_cleanup

  function take_aligned(nbytes, loose) result(user)
    integer(c_int64_t), intent(in) :: nbytes
    logical, intent(in) :: loose
    type(c_ptr) :: user
    integer(c_size_t) :: need
    integer(c_int64_t) :: kept, need64
    logical :: close_fit
    need = align64(nbytes)
    if (need == 0_c_size_t) then
      user = c_null_ptr
      return
    end if
    need64 = int(need, c_int64_t)
    ! Export must not spend a large block on a smaller one: release would
    ! free it. Import may reuse any larger block because the caller frees it.
    if (c_associated(pool_user) .and. pool_cap >= need64) then
      close_fit = pool_cap - need64 <= need64
      if (loose .or. close_fit) then
        user = pool_user
        kept = pool_cap
        pool_user = c_null_ptr
        pool_cap = 0_c_int64_t
        note_ptr = user
        note_cap = kept
        return
      end if
    end if
    if (c_associated(pool_user) .and. pool_cap < need64) then
      call c_free(pool_user)
      pool_user = c_null_ptr
      pool_cap = 0_c_int64_t
    end if
    user = c_aligned_alloc(64_c_size_t, need)
    if (.not. c_associated(user)) return
    note_ptr = user
    note_cap = int(need, c_int64_t)
  end function take_aligned

  subroutine recycle_user(user)
    type(c_ptr), intent(in) :: user
    integer(c_int64_t) :: cap
    if (.not. c_associated(user)) return
    if (.not. c_associated(user, note_ptr)) then
      call c_free(user)
      return
    end if
    cap = note_cap
    note_ptr = c_null_ptr
    note_cap = 0_c_int64_t
    if (.not. c_associated(pool_user)) then
      pool_user = user
      pool_cap = cap
      call arm_pool_exit()
      return
    end if
    if (cap > pool_cap) then
      call c_free(pool_user)
      pool_user = user
      pool_cap = cap
    else
      call c_free(user)
    end if
  end subroutine recycle_user

  subroutine blank_pair(schema, array)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    schema%format = c_null_ptr
    schema%name = c_null_ptr
    schema%metadata = c_null_ptr
    schema%flags = 0_c_int64_t
    schema%n_children = 0_c_int64_t
    schema%children = c_null_ptr
    schema%dictionary = c_null_ptr
    schema%release = c_null_funptr
    schema%private_data = c_null_ptr
    array%length = 0_c_int64_t
    array%null_count = 0_c_int64_t
    array%offset = 0_c_int64_t
    array%n_buffers = 0_c_int64_t
    array%n_children = 0_c_int64_t
    array%buffers = c_null_ptr
    array%children = c_null_ptr
    array%dictionary = c_null_ptr
    array%release = c_null_funptr
    array%private_data = c_null_ptr
  end subroutine blank_pair

  function owned_bytes() result(n)
    integer(c_size_t) :: n
    type(arrow_owned) :: sample
    n = c_sizeof(sample)
  end function owned_bytes

  function ptr_bytes() result(n)
    integer(c_size_t) :: n
    type(c_ptr) :: sample
    n = c_sizeof(sample)
  end function ptr_bytes

  function align64(n) result(m)
    integer(c_int64_t), intent(in) :: n
    integer(c_size_t) :: m
    integer(c_int64_t) :: up
    if (n <= 0) then
      m = 0_c_size_t
      return
    end if
    up = ((n + 63_c_int64_t) / 64_c_int64_t) * 64_c_int64_t
    m = int(up, c_size_t)
  end function align64

  function byte_code(b) result(code)
    integer(c_int8_t), intent(in) :: b
    integer :: code
    character(1) :: ch
    ch = transfer(b, ch)
    code = iachar(ch)
  end function byte_code

  function code_to_i8(code) result(b)
    integer, intent(in) :: code
    integer(c_int8_t) :: b
    character(1) :: ch
    integer(c_int8_t) :: tmp(1)
    ch = achar(iand(code, 255))
    tmp = transfer(ch, tmp)
    b = tmp(1)
  end function code_to_i8

  function static_format_ptr(type_id) result(p)
    integer, intent(in) :: type_id
    type(c_ptr) :: p
    character(kind=c_char), target, save :: chars(2, 11)
    logical, save :: ready = .false.
    if (.not. ready) then
      chars(1, AK_BOOL) = "b"
      chars(1, AK_INT8) = "c"
      chars(1, AK_INT16) = "s"
      chars(1, AK_INT32) = "i"
      chars(1, AK_INT64) = "l"
      chars(1, AK_UINT8) = "C"
      chars(1, AK_UINT16) = "S"
      chars(1, AK_UINT32) = "I"
      chars(1, AK_UINT64) = "L"
      chars(1, AK_FLOAT32) = "f"
      chars(1, AK_FLOAT64) = "g"
      chars(2, :) = c_null_char
      ready = .true.
    end if
    p = c_loc(chars(1, type_id))
  end function static_format_ptr

  function static_name_ptr() result(p)
    type(c_ptr) :: p
    character(kind=c_char), target, save :: chars(7)
    logical, save :: ready = .false.
    if (.not. ready) then
      chars(1) = "v"
      chars(2) = "a"
      chars(3) = "l"
      chars(4) = "u"
      chars(5) = "e"
      chars(6) = "s"
      chars(7) = c_null_char
      ready = .true.
    end if
    p = c_loc(chars(1))
  end function static_name_ptr

  subroutine pack_bool(src, n, dst)
    type(c_ptr), intent(in) :: src
    integer(c_int64_t), intent(in) :: n
    type(c_ptr), intent(in) :: dst
    integer(c_int8_t), pointer :: raw(:)
    integer(c_int8_t), pointer :: bytes(:)
    integer(c_int8_t) :: chunk(8)
    integer(c_int64_t) :: nbytes, word, magic, mask64
    integer :: groups, g, b0, tail, bit, code
    nbytes = (n + 7_c_int64_t) / 8_c_int64_t
    if (nbytes <= 0) return
    call c_f_pointer(src, raw, [n])
    call c_f_pointer(dst, bytes, [nbytes])
    ! Low bit of each logical byte, then bit i moved to position 56+i.
    magic = int(z"0102040810204080", c_int64_t)
    mask64 = int(z"0101010101010101", c_int64_t)
    groups = int(n / 8_c_int64_t)
    do g = 1, groups
      b0 = (g - 1) * 8 + 1
      chunk = raw(b0:b0 + 7)
      word = transfer(chunk, word)
      word = iand(word, mask64) * magic
      bytes(g) = code_to_i8(int(ishft(word, -56)))
    end do
    tail = int(n - int(groups, c_int64_t) * 8_c_int64_t)
    if (tail > 0) then
      code = 0
      b0 = groups * 8
      do bit = 0, tail - 1
        if (raw(b0 + 1 + bit) /= 0_c_int8_t) code = ior(code, ishft(1, bit))
      end do
      bytes(groups + 1) = code_to_i8(code)
    end if
  end subroutine pack_bool

  subroutine unpack_bool(src, n, dst)
    type(c_ptr), intent(in) :: src
    integer(c_int64_t), intent(in) :: n
    type(c_ptr), intent(in) :: dst
    integer(c_int8_t), pointer :: bytes(:)
    integer(c_int8_t), pointer :: outb(:)
    integer(c_int64_t), pointer :: words(:)
    integer(c_int64_t), save :: expanded(0:255)
    logical, save :: ready = .false.
    integer(c_int64_t) :: nbytes, full
    integer(c_int8_t) :: piece(8)
    integer :: code, i, bit, groups, t
    if (.not. ready) then
      do code = 0, 255
        do t = 0, 7
          if (btest(code, t)) then
            piece(t + 1) = 1_c_int8_t
          else
            piece(t + 1) = 0_c_int8_t
          end if
        end do
        expanded(code) = transfer(piece, expanded(code))
      end do
      ready = .true.
    end if
    if (n <= 0) return
    nbytes = (n + 7_c_int64_t) / 8_c_int64_t
    call c_f_pointer(src, bytes, [nbytes])
    call c_f_pointer(dst, outb, [n])
    groups = int(n / 8_c_int64_t)
    if (groups > 0) call c_f_pointer(dst, words, [groups])
    do i = 1, groups
      code = iand(int(bytes(i)), 255)
      words(i) = expanded(code)
    end do
    full = int(groups, c_int64_t) * 8_c_int64_t
    if (full < n) then
      code = byte_code(bytes(groups + 1))
      do i = 0, int(n - full) - 1
        bit = i
        if (iand(code, ishft(1, bit)) /= 0) then
          outb(int(full) + 1 + i) = 1_c_int8_t
        else
          outb(int(full) + 1 + i) = 0_c_int8_t
        end if
      end do
    end if
  end subroutine unpack_bool

  subroutine build_bitmap(valid, n, bitmap, null_count, status)
    type(c_ptr), intent(in) :: valid
    integer(c_int64_t), intent(in) :: n
    type(c_ptr), intent(out) :: bitmap
    integer(c_int64_t), intent(out) :: null_count
    integer, intent(out) :: status
    logical(c_bool), pointer :: mask(:)
    integer(c_int64_t) :: i, nbytes
    bitmap = c_null_ptr
    null_count = 0_c_int64_t
    nbytes = (n + 7_c_int64_t) / 8_c_int64_t
    bitmap = c_malloc(int(nbytes, c_size_t))
    if (.not. c_associated(bitmap)) then
      status = ARROW_ERR_ALLOC
      return
    end if
    call pack_bool(valid, n, bitmap)
    call c_f_pointer(valid, mask, [n])
    do i = 1, n
      if (.not. mask(i)) null_count = null_count + 1_c_int64_t
    end do
    status = ARROW_OK
  end subroutine build_bitmap

  subroutine zero_null_slots(type_id, data, valid, n, width)
    integer, intent(in) :: type_id
    type(c_ptr), intent(in) :: data
    type(c_ptr), intent(in) :: valid
    integer(c_int64_t), intent(in) :: n
    integer, intent(in) :: width
    logical(c_bool), pointer :: mask(:)
    integer(c_int8_t), pointer :: bytes(:)
    integer(c_int64_t) :: i, nbytes, off
    integer :: bit, code, idx
    if (n <= 0 .or. .not. c_associated(data)) return
    call c_f_pointer(valid, mask, [n])
    if (type_id == AK_BOOL) then
      nbytes = (n + 7_c_int64_t) / 8_c_int64_t
      call c_f_pointer(data, bytes, [nbytes])
      do i = 1, n
        if (.not. mask(i)) then
          bit = int(mod(i - 1_c_int64_t, 8_c_int64_t))
          idx = int(1 + (i - 1_c_int64_t) / 8_c_int64_t)
          code = byte_code(bytes(idx))
          code = iand(code, not(ishft(1, bit)))
          bytes(idx) = code_to_i8(code)
        end if
      end do
    else
      nbytes = n * int(width, c_int64_t)
      call c_f_pointer(data, bytes, [nbytes])
      do i = 1, n
        if (.not. mask(i)) then
          off = (i - 1_c_int64_t) * int(width, c_int64_t)
          bytes(off + 1:off + width) = 0_c_int8_t
        end if
      end do
    end if
  end subroutine zero_null_slots

  function cstr_eq(ptr, text) result(ok)
    type(c_ptr), intent(in) :: ptr
    character(len=*), intent(in) :: text
    logical :: ok
    character(kind=c_char), pointer :: buf(:)
    integer :: i, n
    ok = .false.
    if (.not. c_associated(ptr)) return
    n = len(text)
    call c_f_pointer(ptr, buf, [n + 1])
    do i = 1, n
      if (buf(i) /= text(i:i)) return
    end do
    if (buf(n + 1) /= c_null_char) return
    ok = .true.
  end function cstr_eq

  subroutine new_owned(priv, mem, counter, status)
    type(arrow_owned), pointer, intent(out) :: priv
    type(c_ptr), intent(out) :: mem
    type(c_ptr), intent(in) :: counter
    integer, intent(out) :: status
    mem = c_malloc(owned_bytes())
    if (.not. c_associated(mem)) then
      status = ARROW_ERR_ALLOC
      nullify(priv)
      return
    end if
    call c_f_pointer(mem, priv)
    priv%magic = GLD_MAGIC
    priv%owns_data = 0
    priv%owns_format = 0
    priv%owns_name = 0
    priv%owns_meta = 0
    priv%owns_bitmap = 0
    priv%owns_buffers = 0
    priv%pad = 0
    priv%format_ptr = c_null_ptr
    priv%name_ptr = c_null_ptr
    priv%metadata_ptr = c_null_ptr
    priv%buffers_ptr = c_null_ptr
    priv%data_ptr = c_null_ptr
    priv%bitmap_ptr = c_null_ptr
    priv%counter = counter
    status = ARROW_OK
  end subroutine new_owned

  subroutine free_owned(priv)
    type(arrow_owned), intent(inout) :: priv
    integer(c_int32_t), pointer :: counter
    if (priv%magic /= GLD_MAGIC) return
    if (c_associated(priv%counter)) then
      call c_f_pointer(priv%counter, counter)
      counter = counter + 1_c_int32_t
    end if
    if (priv%owns_data /= 0 .and. c_associated(priv%data_ptr)) call recycle_user(priv%data_ptr)
    if (priv%owns_bitmap /= 0 .and. c_associated(priv%bitmap_ptr)) call c_free(priv%bitmap_ptr)
    if (priv%owns_buffers /= 0 .and. c_associated(priv%buffers_ptr)) call c_free(priv%buffers_ptr)
    if (priv%owns_format /= 0 .and. c_associated(priv%format_ptr)) call c_free(priv%format_ptr)
    if (priv%owns_name /= 0 .and. c_associated(priv%name_ptr)) call c_free(priv%name_ptr)
    if (priv%owns_meta /= 0 .and. c_associated(priv%metadata_ptr)) call c_free(priv%metadata_ptr)
    priv%owns_data = 0
    priv%owns_bitmap = 0
    priv%owns_buffers = 0
    priv%owns_format = 0
    priv%owns_name = 0
    priv%owns_meta = 0
    priv%data_ptr = c_null_ptr
    priv%bitmap_ptr = c_null_ptr
    priv%buffers_ptr = c_null_ptr
    priv%format_ptr = c_null_ptr
    priv%name_ptr = c_null_ptr
    priv%metadata_ptr = c_null_ptr
    priv%counter = c_null_ptr
    priv%magic = 0
  end subroutine free_owned

  subroutine gld_schema_release(schema) bind(C)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_owned), pointer :: priv
    if (.not. c_associated(schema%release)) return
    if (c_associated(schema%private_data)) then
      call c_f_pointer(schema%private_data, priv)
      call free_owned(priv)
      call c_free(schema%private_data)
    end if
    schema%format = c_null_ptr
    schema%name = c_null_ptr
    schema%metadata = c_null_ptr
    schema%flags = 0_c_int64_t
    schema%n_children = 0_c_int64_t
    schema%children = c_null_ptr
    schema%dictionary = c_null_ptr
    schema%private_data = c_null_ptr
    schema%release = c_null_funptr
  end subroutine gld_schema_release

  subroutine gld_array_release(array) bind(C)
    type(arrow_array_t), intent(inout) :: array
    type(arrow_owned), pointer :: priv
    if (.not. c_associated(array%release)) return
    if (c_associated(array%private_data)) then
      call c_f_pointer(array%private_data, priv)
      call free_owned(priv)
      call c_free(array%private_data)
    end if
    array%length = 0_c_int64_t
    array%null_count = 0_c_int64_t
    array%offset = 0_c_int64_t
    array%n_buffers = 0_c_int64_t
    array%n_children = 0_c_int64_t
    array%buffers = c_null_ptr
    array%children = c_null_ptr
    array%dictionary = c_null_ptr
    array%private_data = c_null_ptr
    array%release = c_null_funptr
  end subroutine gld_array_release

  subroutine invoke_schema_release(schema)
    type(arrow_schema_t), intent(inout) :: schema
    procedure(schema_release_iface), pointer :: fp
    if (.not. c_associated(schema%release)) return
    call c_f_procpointer(schema%release, fp)
    call fp(schema)
  end subroutine invoke_schema_release

  subroutine invoke_array_release(array)
    type(arrow_array_t), intent(inout) :: array
    procedure(array_release_iface), pointer :: fp
    if (.not. c_associated(array%release)) return
    call c_f_procpointer(array%release, fp)
    call fp(array)
  end subroutine invoke_array_release

  subroutine arrow_export_raw(type_id, rank, d1, d2, src, valid, rel_s, rel_a, &
      schema, array, status)
    integer, intent(in) :: type_id
    integer(c_int32_t), intent(in) :: rank
    integer(c_int64_t), intent(in) :: d1
    integer(c_int64_t), intent(in) :: d2
    type(c_ptr), intent(in) :: src
    type(c_ptr), intent(in) :: valid
    type(c_ptr), intent(in) :: rel_s
    type(c_ptr), intent(in) :: rel_a
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    type(c_ptr) :: data, bitmap, bufmem, fmt, name, meta
    type(c_ptr) :: schema_mem, array_mem
    type(arrow_owned), pointer :: schema_priv, array_priv
    type(c_ptr), pointer :: bufs(:)
    integer(c_int64_t) :: n, nbytes, null_count
    integer :: width
    logical(c_bool) :: bool_probe
    character(len=1) :: fmt_text

    call blank_pair(schema, array)
    status = ARROW_OK
    data = c_null_ptr
    bitmap = c_null_ptr
    bufmem = c_null_ptr
    fmt = c_null_ptr
    name = c_null_ptr
    meta = c_null_ptr
    schema_mem = c_null_ptr
    array_mem = c_null_ptr
    nullify(schema_priv)
    nullify(array_priv)

    if (.not. arrow_host_little_endian()) then
      status = ARROW_ERR_ENDIAN
      return
    end if
    if (type_id == AK_BOOL) then
      if (c_sizeof(bool_probe) /= 1_c_size_t) then
        status = ARROW_ERR_TYPE
        return
      end if
    end if
    width = arrow_width_of(type_id)
    fmt_text = arrow_format_of(type_id)
    if (width < 0 .or. fmt_text == "?") then
      status = ARROW_ERR_TYPE
      return
    end if
    if (rank == 1) then
      if (d1 < 0) then
        status = ARROW_ERR_SHAPE
        return
      end if
      n = d1
    else if (rank == 2) then
      if (d1 < 0 .or. d2 < 0) then
        status = ARROW_ERR_SHAPE
        return
      end if
      if (d2 > 0_c_int64_t) then
        if (d1 > huge(d1) / d2) then
          status = ARROW_ERR_SHAPE
          return
        end if
      end if
      n = d1 * d2
    else
      status = ARROW_ERR_SHAPE
      return
    end if
    if (n > 0 .and. .not. c_associated(src)) then
      status = ARROW_ERR_NULL
      return
    end if
    if (type_id == AK_BOOL) then
      nbytes = (n + 7_c_int64_t) / 8_c_int64_t
    else
      if (n > huge(n) / int(width, c_int64_t)) then
        status = ARROW_ERR_SHAPE
        return
      end if
      nbytes = n * int(width, c_int64_t)
    end if

    if (nbytes > 0) then
      data = take_aligned(nbytes, .false.)
      if (.not. c_associated(data)) then
        status = ARROW_ERR_ALLOC
        goto 900
      end if
      if (type_id == AK_BOOL) then
        call pack_bool(src, n, data)
      else
        call c_memcpy(data, src, int(nbytes, c_size_t))
      end if
    end if

    null_count = 0_c_int64_t
    if (c_associated(valid) .and. n > 0) then
      call build_bitmap(valid, n, bitmap, null_count, status)
      if (status /= ARROW_OK) goto 900
      if (null_count == 0) then
        if (c_associated(bitmap)) call c_free(bitmap)
        bitmap = c_null_ptr
      else
        call zero_null_slots(type_id, data, valid, n, width)
      end if
    end if

    bufmem = c_malloc(2_c_size_t * ptr_bytes())
    if (.not. c_associated(bufmem)) then
      status = ARROW_ERR_ALLOC
      goto 900
    end if
    call c_f_pointer(bufmem, bufs, [2])
    bufs(1) = bitmap
    bufs(2) = data

    fmt = static_format_ptr(type_id)
    name = static_name_ptr()
    if (rank == 1) then
      call meta_encode(rank, d1, 0_c_int64_t, meta, status)
    else
      call meta_encode(rank, d1, d2, meta, status)
    end if
    if (status /= ARROW_OK) goto 900

    call new_owned(schema_priv, schema_mem, rel_s, status)
    if (status /= ARROW_OK) goto 900
    schema_priv%owns_format = 0
    schema_priv%owns_name = 0
    schema_priv%owns_meta = 1
    schema_priv%format_ptr = fmt
    schema_priv%name_ptr = name
    schema_priv%metadata_ptr = meta

    call new_owned(array_priv, array_mem, rel_a, status)
    if (status /= ARROW_OK) goto 900
    array_priv%owns_buffers = 1
    array_priv%buffers_ptr = bufmem
    if (c_associated(data)) then
      array_priv%owns_data = 1
      array_priv%data_ptr = data
    end if
    if (c_associated(bitmap)) then
      array_priv%owns_bitmap = 1
      array_priv%bitmap_ptr = bitmap
    end if

    schema%format = fmt
    schema%name = name
    schema%metadata = meta
    schema%flags = int(ARROW_FLAG_NULLABLE, c_int64_t)
    schema%n_children = 0_c_int64_t
    schema%children = c_null_ptr
    schema%dictionary = c_null_ptr
    schema%private_data = schema_mem
    schema%release = c_funloc(gld_schema_release)

    array%length = n
    array%null_count = null_count
    array%offset = 0_c_int64_t
    array%n_buffers = 2_c_int64_t
    array%n_children = 0_c_int64_t
    array%buffers = bufmem
    array%children = c_null_ptr
    array%dictionary = c_null_ptr
    array%private_data = array_mem
    array%release = c_funloc(gld_array_release)
    return

900 continue
    if (c_associated(data)) call recycle_user(data)
    if (c_associated(bitmap)) call c_free(bitmap)
    if (c_associated(bufmem)) call c_free(bufmem)
    if (c_associated(meta)) call c_free(meta)
    if (c_associated(schema_mem)) call c_free(schema_mem)
    if (c_associated(array_mem)) call c_free(array_mem)
    call blank_pair(schema, array)
  end subroutine arrow_export_raw

  subroutine arrow_import_raw(type_id, want_rank, schema, array, raw, vraw, &
      rank_out, d1, d2, want_valid, status)
    integer, intent(in) :: type_id
    integer(c_int32_t), intent(in) :: want_rank
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    type(c_ptr), intent(out) :: raw
    type(c_ptr), intent(out) :: vraw
    integer(c_int32_t), intent(out) :: rank_out
    integer(c_int64_t), intent(out) :: d1
    integer(c_int64_t), intent(out) :: d2
    logical, intent(in) :: want_valid
    integer, intent(out) :: status
    type(c_ptr), pointer :: bufs(:)
    type(arrow_owned), pointer :: priv
    type(c_ptr) :: data, bitmap
    integer(c_int64_t) :: n, nbytes, nulls
    integer(c_int32_t) :: meta_rank
    integer :: width
    character(len=1) :: expect
    logical :: armed, stolen

    raw = c_null_ptr
    vraw = c_null_ptr
    rank_out = 0
    d1 = 0
    d2 = 0
    stolen = .false.
    nullify(priv)
    status = ARROW_OK
    armed = c_associated(schema%release) .and. c_associated(array%release)
    if (.not. arrow_host_little_endian()) then
      status = ARROW_ERR_ENDIAN
      if (armed) goto 800
      return
    end if
    if (.not. armed) then
      status = ARROW_ERR_STATE
      return
    end if
    expect = arrow_format_of(type_id)
    width = arrow_width_of(type_id)
    if (width < 0 .or. .not. cstr_eq(schema%format, expect)) then
      status = ARROW_ERR_TYPE
      goto 800
    end if
    if (schema%n_children /= 0 .or. array%n_children /= 0) then
      status = ARROW_ERR_TYPE
      goto 800
    end if
    if (c_associated(schema%children) .or. c_associated(array%children)) then
      status = ARROW_ERR_TYPE
      goto 800
    end if
    if (c_associated(schema%dictionary) .or. c_associated(array%dictionary)) then
      status = ARROW_ERR_TYPE
      goto 800
    end if
    if (array%offset /= 0) then
      status = ARROW_ERR_OFFSET
      goto 800
    end if
    if (array%n_buffers /= 2) then
      status = ARROW_ERR_TYPE
      goto 800
    end if
    if (.not. c_associated(array%buffers)) then
      status = ARROW_ERR_NULL
      goto 800
    end if
    n = array%length
    if (n < 0) then
      status = ARROW_ERR_SHAPE
      goto 800
    end if
    call meta_decode(schema%metadata, meta_rank, d1, d2, status)
    if (status /= ARROW_OK) goto 800
    if (meta_rank == 0) then
      if (want_rank /= 1) then
        status = ARROW_ERR_SHAPE
        goto 800
      end if
      d1 = n
      d2 = 0
    else if (meta_rank /= want_rank) then
      status = ARROW_ERR_SHAPE
      goto 800
    else if (want_rank == 1) then
      if (d1 /= n) then
        status = ARROW_ERR_SHAPE
        goto 800
      end if
    else
      if (d1 < 0 .or. d2 < 0 .or. d1 * d2 /= n) then
        status = ARROW_ERR_SHAPE
        goto 800
      end if
    end if
    rank_out = want_rank
    call c_f_pointer(array%buffers, bufs, [2])
    bitmap = bufs(1)
    data = bufs(2)
    if (n == 0) then
      data = c_null_ptr
    else if (.not. c_associated(data)) then
      status = ARROW_ERR_NULL
      goto 800
    end if
    nulls = array%null_count
    if (nulls < -1) then
      status = ARROW_ERR_SHAPE
      goto 800
    end if
    if (nulls > 0 .and. .not. c_associated(bitmap)) then
      status = ARROW_ERR_NULL
      goto 800
    end if
    if (type_id == AK_BOOL) then
      nbytes = n
    else
      nbytes = n * int(width, c_int64_t)
    end if
    if (type_id /= AK_BOOL .and. nbytes > 0 .and. c_associated(array%private_data)) then
      call c_f_pointer(array%private_data, priv)
      if (priv%magic == GLD_MAGIC .and. priv%owns_data /= 0 .and. &
          c_associated(priv%data_ptr)) then
        raw = priv%data_ptr
        priv%owns_data = 0
        priv%data_ptr = c_null_ptr
        stolen = .true.
      end if
    end if
    if (.not. stolen .and. nbytes > 0) then
      raw = take_aligned(nbytes, .true.)
      if (.not. c_associated(raw)) then
        status = ARROW_ERR_ALLOC
        goto 800
      end if
      if (type_id == AK_BOOL) then
        call unpack_bool(data, n, raw)
      else
        call c_memcpy(raw, data, int(nbytes, c_size_t))
      end if
    end if
    if (want_valid .and. n > 0) then
      vraw = c_malloc(int(n, c_size_t))
      if (.not. c_associated(vraw)) then
        status = ARROW_ERR_ALLOC
        goto 800
      end if
      if (c_associated(bitmap)) then
        call unpack_bool(bitmap, n, vraw)
      else
        call fill_valid(vraw, n)
      end if
    end if
    call invoke_array_release(array)
    call invoke_schema_release(schema)
    return

800 continue
    if (c_associated(raw)) call recycle_user(raw)
    if (c_associated(vraw)) call c_free(vraw)
    raw = c_null_ptr
    vraw = c_null_ptr
    if (armed) then
      call invoke_array_release(array)
      call invoke_schema_release(schema)
    end if
  end subroutine arrow_import_raw

  subroutine fill_valid(dst, n)
    type(c_ptr), intent(in) :: dst
    integer(c_int64_t), intent(in) :: n
    logical(c_bool), pointer :: mask(:)
    integer(c_int64_t) :: i
    call c_f_pointer(dst, mask, [n])
    do i = 1, n
      mask(i) = .true.
    end do
  end subroutine fill_valid


  subroutine export_bool_1(a, schema, array, status, valid, &
      release_schema, release_array)
    logical(c_bool), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_BOOL, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_bool_1

  subroutine export_bool_2(a, schema, array, status, valid, &
      release_schema, release_array)
    logical(c_bool), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_BOOL, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_bool_2

  subroutine import_bool_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    logical(c_bool), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_BOOL, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_bool_1

  subroutine import_bool_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    logical(c_bool), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_BOOL, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_bool_2


  subroutine export_int8_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int8_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT8, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int8_1

  subroutine export_int8_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int8_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT8, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int8_2

  subroutine import_int8_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int8_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT8, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_int8_1

  subroutine import_int8_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int8_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT8, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_int8_2


  subroutine export_int16_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int16_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT16, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int16_1

  subroutine export_int16_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int16_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT16, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int16_2

  subroutine import_int16_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int16_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT16, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_int16_1

  subroutine import_int16_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int16_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT16, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_int16_2


  subroutine export_int32_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int32_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT32, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int32_1

  subroutine export_int32_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int32_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT32, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int32_2

  subroutine import_int32_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int32_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT32, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_int32_1

  subroutine import_int32_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int32_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT32, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_int32_2


  subroutine export_int64_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int64_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT64, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int64_1

  subroutine export_int64_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int64_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_INT64, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_int64_2

  subroutine import_int64_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int64_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT64, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_int64_1

  subroutine import_int64_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int64_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_INT64, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_int64_2


  subroutine export_uint8_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int8_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT8, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint8_1

  subroutine export_uint8_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int8_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT8, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint8_2

  subroutine import_uint8_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int8_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT8, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_uint8_1

  subroutine import_uint8_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int8_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT8, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_uint8_2


  subroutine export_uint16_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int16_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT16, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint16_1

  subroutine export_uint16_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int16_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT16, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint16_2

  subroutine import_uint16_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int16_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT16, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_uint16_1

  subroutine import_uint16_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int16_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT16, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_uint16_2


  subroutine export_uint32_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int32_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT32, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint32_1

  subroutine export_uint32_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int32_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT32, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint32_2

  subroutine import_uint32_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int32_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT32, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_uint32_1

  subroutine import_uint32_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int32_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT32, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_uint32_2


  subroutine export_uint64_1(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int64_t), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT64, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint64_1

  subroutine export_uint64_2(a, schema, array, status, valid, &
      release_schema, release_array)
    integer(c_int64_t), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_UINT64, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_uint64_2

  subroutine import_uint64_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int64_t), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT64, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_uint64_1

  subroutine import_uint64_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    integer(c_int64_t), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_UINT64, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_uint64_2


  subroutine export_float32_1(a, schema, array, status, valid, &
      release_schema, release_array)
    real(c_float), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_FLOAT32, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_float32_1

  subroutine export_float32_2(a, schema, array, status, valid, &
      release_schema, release_array)
    real(c_float), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_FLOAT32, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_float32_2

  subroutine import_float32_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    real(c_float), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_FLOAT32, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_float32_1

  subroutine import_float32_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    real(c_float), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_FLOAT32, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_float32_2


  subroutine export_float64_1(a, schema, array, status, valid, &
      release_schema, release_array)
    real(c_double), intent(in), target :: a(:)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: n
    call blank_pair(schema, array)
    n = size(a, kind=c_int64_t)
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. size(valid, kind=c_int64_t) /= n) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, kind=c_int64_t) /= n) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_FLOAT64, 1_c_int32_t, n, 0_c_int64_t, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_float64_1

  subroutine export_float64_2(a, schema, array, status, valid, &
      release_schema, release_array)
    real(c_double), intent(in), target :: a(:, :)
    type(arrow_schema_t), intent(out) :: schema
    type(arrow_array_t), intent(out) :: array
    integer, intent(out) :: status
    logical(c_bool), intent(in), optional, target :: valid(:, :)
    integer(c_int32_t), intent(inout), optional, target :: release_schema
    integer(c_int32_t), intent(inout), optional, target :: release_array
    type(c_ptr) :: src, vp, sp, ap
    integer(c_int64_t) :: d1, d2, n
    call blank_pair(schema, array)
    d1 = size(a, 1, kind=c_int64_t)
    d2 = size(a, 2, kind=c_int64_t)
    if (d2 > 0_c_int64_t) then
      if (d1 > huge(d1) / d2) then
        status = ARROW_ERR_SHAPE
        return
      end if
    end if
    n = d1 * d2
    if (.not. is_contiguous(a)) then
      status = ARROW_ERR_LAYOUT
      return
    end if
    src = c_null_ptr
    if (n > 0) src = c_loc(a(1, 1))
    vp = c_null_ptr
    if (present(valid)) then
      if (.not. is_contiguous(valid) .or. &
          size(valid, 1, kind=c_int64_t) /= d1 .or. &
          size(valid, 2, kind=c_int64_t) /= d2) then
        status = ARROW_ERR_LAYOUT
        if (size(valid, 1, kind=c_int64_t) /= d1 .or. &
            size(valid, 2, kind=c_int64_t) /= d2) status = ARROW_ERR_SHAPE
        return
      end if
      if (n > 0) vp = c_loc(valid(1, 1))
    end if
    sp = c_null_ptr
    ap = c_null_ptr
    if (present(release_schema)) sp = c_loc(release_schema)
    if (present(release_array)) ap = c_loc(release_array)
    call arrow_export_raw(AK_FLOAT64, 2_c_int32_t, d1, d2, src, vp, sp, ap, &
        schema, array, status)
  end subroutine export_float64_2

  subroutine import_float64_1(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    real(c_double), pointer, intent(out) :: out(:)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: n1
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_FLOAT64, 1_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. c_associated(raw)) then
      n1 = int(d1)
      call c_f_pointer(raw, out, [n1])
    end if
    if (present(valid)) then
      if (d1 > 0 .and. c_associated(vraw)) then
        n1 = int(d1)
        call c_f_pointer(vraw, valid, [n1])
      end if
    end if
  end subroutine import_float64_1

  subroutine import_float64_2(schema, array, out, status, valid)
    type(arrow_schema_t), intent(inout) :: schema
    type(arrow_array_t), intent(inout) :: array
    real(c_double), pointer, intent(out) :: out(:, :)
    integer, intent(out) :: status
    logical(c_bool), pointer, optional, intent(out) :: valid(:, :)
    type(c_ptr) :: raw, vraw
    integer(c_int32_t) :: rank
    integer(c_int64_t) :: d1, d2
    integer :: shp(2)
    nullify(out)
    if (present(valid)) nullify(valid)
    call arrow_import_raw(AK_FLOAT64, 2_c_int32_t, schema, array, raw, vraw, &
        rank, d1, d2, present(valid), status)
    if (status /= ARROW_OK) return
    if (d1 > 0 .and. d2 > 0 .and. c_associated(raw)) then
      shp(1) = int(d1)
      shp(2) = int(d2)
      call c_f_pointer(raw, out, shp)
    end if
    if (present(valid)) then
      if (d1 > 0 .and. d2 > 0 .and. c_associated(vraw)) then
        shp(1) = int(d1)
        shp(2) = int(d2)
        call c_f_pointer(vraw, valid, shp)
      end if
    end if
  end subroutine import_float64_2

end module arrow_runtime
