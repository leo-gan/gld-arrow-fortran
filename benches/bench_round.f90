program bench_round
  use, intrinsic :: iso_c_binding, only: c_double, c_bool, c_loc, c_int64_t
  use, intrinsic :: iso_fortran_env, only: int64
  use gld_arrow
  implicit none
  integer(c_int64_t), parameter :: N = 1048576
  integer, parameter :: REPS = 10
  real(c_double), allocatable, target :: values(:)
  real(c_double), pointer :: back(:)
  logical(c_bool), allocatable, target :: bits(:)
  logical(c_bool), pointer :: bits_back(:)
  type(arrow_schema_t) :: schema
  type(arrow_array_t) :: array
  integer :: stat, rep
  integer(int64) :: i, t0, t1, te, rate
  integer(int64) :: export_ticks, import_ticks
  real(c_double) :: ns, export_ns, import_ns

  allocate(values(N))
  do i = 1, N
    values(i) = real(i, c_double) * 0.5d0
  end do
  call arrow_export_float64(values, schema, array, stat)
  if (stat /= ARROW_OK) error stop stat
  call arrow_import_float64(schema, array, back, stat)
  if (stat /= ARROW_OK) error stop stat
  if (back(1) /= values(1) .or. back(N) /= values(N)) error stop 2
  call arrow_free(c_loc(back(1)))

  export_ticks = 0
  import_ticks = 0
  call system_clock(t0, rate)
  do rep = 1, REPS
    call system_clock(te)
    call arrow_export_float64(values, schema, array, stat)
    call system_clock(t1)
    export_ticks = export_ticks + (t1 - te)
    if (stat /= ARROW_OK) error stop stat
    call system_clock(te)
    call arrow_import_float64(schema, array, back, stat)
    call system_clock(t1)
    import_ticks = import_ticks + (t1 - te)
    if (stat /= ARROW_OK) error stop stat
    call arrow_free(c_loc(back(1)))
  end do
  call system_clock(t1, rate)
  ns = (real(t1 - t0, c_double) / real(rate, c_double)) / real(REPS, c_double) * 1.0d9
  export_ns = real(export_ticks, c_double) / real(rate, c_double) / real(REPS, c_double) * 1.0d9
  import_ns = real(import_ticks, c_double) / real(rate, c_double) / real(REPS, c_double) * 1.0d9
  write(*, "(A,I0,A,I0,A,F0.1,A,F0.1,A,F0.1)") "payload=float64 rank=1 n=", N, &
      " reps=", REPS, " ns_per_op=", ns, " export_ns=", export_ns, &
      " import_ns=", import_ns

  allocate(bits(N))
  do i = 1, N
    bits(i) = mod(i, 2_int64) == 1_int64
  end do
  call arrow_export_bool(bits, schema, array, stat)
  if (stat /= ARROW_OK) error stop stat
  call arrow_import_bool(schema, array, bits_back, stat)
  if (stat /= ARROW_OK) error stop stat
  if (bits_back(1) .neqv. bits(1) .or. bits_back(N) .neqv. bits(N)) error stop 3
  call arrow_free(c_loc(bits_back(1)))

  export_ticks = 0
  import_ticks = 0
  call system_clock(t0, rate)
  do rep = 1, REPS
    call system_clock(te)
    call arrow_export_bool(bits, schema, array, stat)
    call system_clock(t1)
    export_ticks = export_ticks + (t1 - te)
    if (stat /= ARROW_OK) error stop stat
    call system_clock(te)
    call arrow_import_bool(schema, array, bits_back, stat)
    call system_clock(t1)
    import_ticks = import_ticks + (t1 - te)
    if (stat /= ARROW_OK) error stop stat
    call arrow_free(c_loc(bits_back(1)))
  end do
  call system_clock(t1, rate)
  ns = (real(t1 - t0, c_double) / real(rate, c_double)) / real(REPS, c_double) * 1.0d9
  export_ns = real(export_ticks, c_double) / real(rate, c_double) / real(REPS, c_double) * 1.0d9
  import_ns = real(import_ticks, c_double) / real(rate, c_double) / real(REPS, c_double) * 1.0d9
  write(*, "(A,I0,A,I0,A,F0.1,A,F0.1,A,F0.1)") "payload=bool rank=1 n=", N, &
      " reps=", REPS, " ns_per_op=", ns, " export_ns=", export_ns, &
      " import_ns=", import_ns
end program bench_round
