module arrow_kinds
  implicit none
  private

  public :: AK_BOOL
  public :: AK_INT8, AK_INT16, AK_INT32, AK_INT64
  public :: AK_UINT8, AK_UINT16, AK_UINT32, AK_UINT64
  public :: AK_FLOAT32, AK_FLOAT64
  public :: ARROW_FLAG_DICTIONARY_ORDERED
  public :: ARROW_FLAG_NULLABLE
  public :: ARROW_FLAG_MAP_KEYS_SORTED
  public :: ARROW_OK
  public :: ARROW_ERR_ENDIAN
  public :: ARROW_ERR_ALLOC
  public :: ARROW_ERR_TYPE
  public :: ARROW_ERR_SHAPE
  public :: ARROW_ERR_STATE
  public :: ARROW_ERR_OFFSET
  public :: ARROW_ERR_LAYOUT
  public :: ARROW_ERR_NULL
  public :: arrow_format_of
  public :: arrow_width_of

  integer, parameter :: AK_BOOL = 1
  integer, parameter :: AK_INT8 = 2
  integer, parameter :: AK_INT16 = 3
  integer, parameter :: AK_INT32 = 4
  integer, parameter :: AK_INT64 = 5
  integer, parameter :: AK_UINT8 = 6
  integer, parameter :: AK_UINT16 = 7
  integer, parameter :: AK_UINT32 = 8
  integer, parameter :: AK_UINT64 = 9
  integer, parameter :: AK_FLOAT32 = 10
  integer, parameter :: AK_FLOAT64 = 11

  integer, parameter :: ARROW_FLAG_DICTIONARY_ORDERED = 1
  integer, parameter :: ARROW_FLAG_NULLABLE = 2
  integer, parameter :: ARROW_FLAG_MAP_KEYS_SORTED = 4

  integer, parameter :: ARROW_OK = 0
  integer, parameter :: ARROW_ERR_ENDIAN = 1
  integer, parameter :: ARROW_ERR_ALLOC = 2
  integer, parameter :: ARROW_ERR_TYPE = 3
  integer, parameter :: ARROW_ERR_SHAPE = 4
  integer, parameter :: ARROW_ERR_STATE = 5
  integer, parameter :: ARROW_ERR_OFFSET = 6
  integer, parameter :: ARROW_ERR_LAYOUT = 7
  integer, parameter :: ARROW_ERR_NULL = 8

contains

  function arrow_format_of(type_id) result(fmt)
    integer, intent(in) :: type_id
    character(len=1) :: fmt
    select case (type_id)
    case (AK_BOOL)
      fmt = "b"
    case (AK_INT8)
      fmt = "c"
    case (AK_UINT8)
      fmt = "C"
    case (AK_INT16)
      fmt = "s"
    case (AK_UINT16)
      fmt = "S"
    case (AK_INT32)
      fmt = "i"
    case (AK_UINT32)
      fmt = "I"
    case (AK_INT64)
      fmt = "l"
    case (AK_UINT64)
      fmt = "L"
    case (AK_FLOAT32)
      fmt = "f"
    case (AK_FLOAT64)
      fmt = "g"
    case default
      fmt = "?"
    end select
  end function arrow_format_of

  function arrow_width_of(type_id) result(width)
    integer, intent(in) :: type_id
    integer :: width
    select case (type_id)
    case (AK_BOOL)
      width = 0
    case (AK_INT8, AK_UINT8)
      width = 1
    case (AK_INT16, AK_UINT16)
      width = 2
    case (AK_INT32, AK_UINT32, AK_FLOAT32)
      width = 4
    case (AK_INT64, AK_UINT64, AK_FLOAT64)
      width = 8
    case default
      width = -1
    end select
  end function arrow_width_of

end module arrow_kinds
