program export_array
  use, intrinsic :: iso_c_binding, only: c_double, c_int32_t, c_loc
  use gld_arrow
  implicit none
  real(c_double), target :: values(4)
  real(c_double), target :: grid(2, 3)
  real(c_double), pointer :: back(:)
  real(c_double), pointer :: back2(:, :)
  type(arrow_schema_t) :: schema
  type(arrow_array_t) :: array
  integer :: status
  integer(c_int32_t), target :: schema_hits, array_hits
  integer :: i, j

  values = [1.0d0, 2.5d0, -4.0d0, 8.0d0]
  schema_hits = 0
  array_hits = 0
  call arrow_export_float64(values, schema, array, status, &
      release_schema=schema_hits, release_array=array_hits)
  if (status /= ARROW_OK) error stop status
  call arrow_import_float64(schema, array, back, status)
  if (status /= ARROW_OK) error stop status
  if (schema_hits /= 1 .or. array_hits /= 1) error stop 2
  if (.not. associated(back)) error stop 3
  do i = 1, 4
    if (back(i) /= values(i)) error stop 4
  end do
  print "(A,4(1X,F6.1))", "float64", back
  call arrow_free(c_loc(back(1)))
  nullify(back)

  grid(1, :) = [1.0d0, 3.0d0, 5.0d0]
  grid(2, :) = [2.0d0, 4.0d0, 6.0d0]
  schema_hits = 0
  array_hits = 0
  call arrow_export_float64(grid, schema, array, status, &
      release_schema=schema_hits, release_array=array_hits)
  if (status /= ARROW_OK) error stop status
  if (array%length /= 6) error stop 5
  call arrow_import_float64(schema, array, back2, status)
  if (status /= ARROW_OK) error stop status
  if (schema_hits /= 1 .or. array_hits /= 1) error stop 6
  if (.not. associated(back2)) error stop 7
  do j = 1, 3
    do i = 1, 2
      if (back2(i, j) /= grid(i, j)) error stop 8
    end do
  end do
  print "(A,I0,A,I0)", "float64_2d ", size(back2, 1), " x ", size(back2, 2)
  call arrow_free(c_loc(back2(1, 1)))
end program export_array
