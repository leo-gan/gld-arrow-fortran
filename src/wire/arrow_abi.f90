module arrow_abi
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public :: arrow_schema_t
  public :: arrow_array_t
  public :: c_malloc
  public :: c_free
  public :: c_memcpy
  public :: c_aligned_alloc
  public :: gld_abi_sizes
  public :: ptr_plus
  public :: ptr_minus

  type, bind(C) :: arrow_schema_t
    type(c_ptr) :: format
    type(c_ptr) :: name
    type(c_ptr) :: metadata
    integer(c_int64_t) :: flags
    integer(c_int64_t) :: n_children
    type(c_ptr) :: children
    type(c_ptr) :: dictionary
    type(c_funptr) :: release
    type(c_ptr) :: private_data
  end type arrow_schema_t

  type, bind(C) :: arrow_array_t
    integer(c_int64_t) :: length
    integer(c_int64_t) :: null_count
    integer(c_int64_t) :: offset
    integer(c_int64_t) :: n_buffers
    integer(c_int64_t) :: n_children
    type(c_ptr) :: buffers
    type(c_ptr) :: children
    type(c_ptr) :: dictionary
    type(c_funptr) :: release
    type(c_ptr) :: private_data
  end type arrow_array_t

  interface
    function c_malloc(nbytes) bind(C, name="malloc")
      import :: c_ptr, c_size_t
      integer(c_size_t), value :: nbytes
      type(c_ptr) :: c_malloc
    end function c_malloc

    subroutine c_free(ptr) bind(C, name="free")
      import :: c_ptr
      type(c_ptr), value :: ptr
    end subroutine c_free

    subroutine c_memcpy(dst, src, nbytes) bind(C, name="memcpy")
      import :: c_ptr, c_size_t
      type(c_ptr), value :: dst
      type(c_ptr), value :: src
      integer(c_size_t), value :: nbytes
    end subroutine c_memcpy

    function c_aligned_alloc(alignment, nbytes) bind(C, name="aligned_alloc")
      import :: c_ptr, c_size_t
      integer(c_size_t), value :: alignment
      integer(c_size_t), value :: nbytes
      type(c_ptr) :: c_aligned_alloc
    end function c_aligned_alloc

    subroutine gld_abi_sizes(schema_size, array_size, schema_off, array_off) &
        bind(C, name="gld_abi_sizes")
      import :: c_size_t
      integer(c_size_t), intent(out) :: schema_size
      integer(c_size_t), intent(out) :: array_size
      integer(c_size_t), intent(out) :: schema_off(9)
      integer(c_size_t), intent(out) :: array_off(10)
    end subroutine gld_abi_sizes
  end interface

contains

  function ptr_plus(base, offset) result(p)
    type(c_ptr), intent(in) :: base
    integer(c_size_t), intent(in) :: offset
    type(c_ptr) :: p
    integer(c_intptr_t) :: addr
    addr = transfer(base, addr) + int(offset, c_intptr_t)
    p = transfer(addr, p)
  end function ptr_plus

  function ptr_minus(base, offset) result(p)
    type(c_ptr), intent(in) :: base
    integer(c_size_t), intent(in) :: offset
    type(c_ptr) :: p
    integer(c_intptr_t) :: addr
    addr = transfer(base, addr) - int(offset, c_intptr_t)
    p = transfer(addr, p)
  end function ptr_minus

end module arrow_abi
