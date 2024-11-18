module utils_m
  use iso_fortran_env
  use netcdf
  implicit none

  real(real32), parameter :: MISSING_VALUE = -1e30


contains

  subroutine check_file_exist(filename)
    character(len=*), intent(in) :: filename

    logical fexist

    inquire(file = trim(filename), exist = fexist)
    if (.not. fexist) then
      write(error_unit,'(3a)') "ERROR: file ", trim(filename), " does not exist."
      error stop
    end if

  end subroutine check_file_exist

  subroutine handle_error(error_flag, isfatal, err_string)
    ! Simple error handle for NetCDF
    integer(int32),   intent(in) :: error_flag
    logical,          intent(in), optional :: isfatal
    character(len=*), intent(in), optional :: err_string

    logical :: fatal

    fatal = .true.
    if (present(isfatal)) fatal = isfatal
    if (error_flag  /= nf90_noerr) then
      if (fatal) then
        write(error_unit,'(2a)') 'FATAL ERROR: ', nf90_strerror(error_flag)
        if (present(err_string)) write(error_unit,'(a)') trim(err_string)
        error stop
      end if
    end if

  end subroutine handle_error

  recursive subroutine quicksort(a, first, last)
    real(real32), intent(inout) ::  a(:)
    integer, intent(in) :: first, last

    real(real32) :: x, t
    integer i, j

    x = a((first + last)/2)
    i = first
    j = last
    do
      do while (a(i) < x)
        i = i + 1
      end do
      do while (x < a(j))
        j = j - 1
      end do
      if (i >= j) exit
      t = a(i);  a(i) = a(j);  a(j) = t
      i = i + 1
      j = j - 1
    end do
    if (first < i-1) call quicksort(a, first, i-1)
    if (j+1 < last)  call quicksort(a, j+1, last)

  end subroutine quicksort

  !-------------------------------------------------------------------------
  ! Return a string with the date and time in the following format:
  !  "day-name month-name hh:mm:ss YYYY zone"
  character(len=30) function date_time()
    character(len=3), parameter :: DAYS(7)    = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
    character(len=3), parameter :: MONTHS(12) = &
      ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    integer(kind=int64), parameter :: T(12) = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
    character(len=5)    :: zone
    integer(kind=int64) :: d, dt(8), yy

    call date_and_time(values=dt, zone=zone)

    ! Use Tomohiko Sakamoto's Algorithm to find day of week
    if (dt(2) <= 2) then
      yy = dt(1) - 1
    else
      yy = dt(1)
    end if
    d = mod(yy + int((yy - 1) / 4) - int((yy - 1) / 100) + int((yy - 1) / 400) + T(dt(2)) + dt(3), 7_int64) + 1

    write(date_time, '(a,1x,a,1x,i0.2,1x,i0.2,":",i0.2,":",i0.2,1x,i4,1x,a)') &
      DAYS(d), MONTHS(dt(2)), dt(3), dt(5), dt(6), dt(7), dt(1), zone

  end function date_time

  !-------------------------------------------------------------------------
  ! Returns the command used to invoke the program with the path prefix removed
  function get_mycommand() result(command)
    character(len=:), allocatable :: command

    integer :: i, length
    character(len=:), allocatable :: full_cmd

    call get_command(length=length)
    allocate(character(len=length) :: full_cmd)
    call get_command(full_cmd)

    i = 0
    do while (i < length)
      i = i + 1
      if (full_cmd(i:i) == " ") exit
    end do
    do while (i > 0)
      i = i - 1
      if (full_cmd(i:i) == "/") exit
    end do
    command = full_cmd(i+1:length)

  end function get_mycommand

end module utils_m
