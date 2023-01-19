program float_vgrid
  ! Zeta is double precision. Convert to  single and rewrite. This stops small floating point errors.
  use netcdf
  use, intrinsic :: iso_fortran_env
  use utils
  use M_CLI2
  implicit none

  real(real32), allocatable :: zeta_float(:)
  real(real64), allocatable :: zeta_dp(:)
  integer :: ierr, nzeta
  integer :: ncid, vid
  integer :: dids(1)
  character(len=:), allocatable :: help_text(:), file

  help_text = [character(len=80) :: &
    'usage: float_vgrid [--help] [--vgrid <vgrid>]                                   ', &
    '                                                                                ', &
    'Alter values in ocean vertical grid so they can be used with both single- and   ', &
    'double-precision topography file.                                               ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --vgrid <vgrid>  vertical grid (default ''ocean_vgrid.nc'')                   ', &
    '']

  call set_args('--vgrid "ocean_vgrid.nc"', help_text)

  call handle_error(nf90_open(sget('vgrid'), nf90_write,ncid))
  call handle_error(nf90_inq_varid(ncid,'zeta', vid))
  call handle_error(nf90_inquire_variable(ncid, vid, dimids=dids))
  call handle_error(nf90_inquire_dimension(ncid, dids(1), len=nzeta))
  allocate(zeta_dp(nzeta), zeta_float(nzeta))
  call handle_error(nf90_get_var(ncid, vid, zeta_dp))
  zeta_float = zeta_dp
  zeta_dp = zeta_float
  call handle_error(nf90_put_var(ncid, vid, zeta_dp))
  call handle_error(nf90_close(ncid))

end program float_vgrid
