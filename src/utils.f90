module utils
  use iso_fortran_env
  use netcdf
  implicit none

contains

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
        write(*,*) 'FATAL ERROR:', nf90_strerror(error_flag)
        if (present(err_string)) write(*,*) trim(err_string)
        stop
      end if
    end if

  end subroutine handle_error

end module utils
