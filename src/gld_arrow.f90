module gld_arrow
  use arrow_abi, only: arrow_schema_t, arrow_array_t, gld_abi_sizes
  use arrow_kinds, only: ARROW_FLAG_DICTIONARY_ORDERED, ARROW_FLAG_NULLABLE, &
      ARROW_FLAG_MAP_KEYS_SORTED, ARROW_OK, ARROW_ERR_ENDIAN, ARROW_ERR_ALLOC, &
      ARROW_ERR_TYPE, ARROW_ERR_SHAPE, ARROW_ERR_STATE, ARROW_ERR_OFFSET, &
      ARROW_ERR_LAYOUT, ARROW_ERR_NULL
  use gld_arrow_version, only: ARROW_VERSION
  use arrow_runtime, only: &
      arrow_free, &
      arrow_host_little_endian, &
      arrow_export_bool, &
      arrow_export_int8, &
      arrow_export_int16, &
      arrow_export_int32, &
      arrow_export_int64, &
      arrow_export_uint8, &
      arrow_export_uint16, &
      arrow_export_uint32, &
      arrow_export_uint64, &
      arrow_export_float32, &
      arrow_export_float64, &
      arrow_import_bool, &
      arrow_import_int8, &
      arrow_import_int16, &
      arrow_import_int32, &
      arrow_import_int64, &
      arrow_import_uint8, &
      arrow_import_uint16, &
      arrow_import_uint32, &
      arrow_import_uint64, &
      arrow_import_float32, &
      arrow_import_float64
  implicit none
  public :: arrow_schema_t
  public :: arrow_array_t
  public :: gld_abi_sizes
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
  public :: ARROW_VERSION
  public :: arrow_free
  public :: arrow_host_little_endian
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
end module gld_arrow
