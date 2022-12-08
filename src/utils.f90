module utils
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
        write(error_unit,'(2a)') 'FATAL ERROR:', nf90_strerror(error_flag)
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

end module utils
