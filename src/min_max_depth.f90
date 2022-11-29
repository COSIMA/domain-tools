program min_max_depth
  !
  ! Set depth to be a minimum level
  !
  ! USAGE:
  ! min_depth file_in file_out level
  !
  !
  use iso_fortran_env
  use netcdf
  use utils
  use topography
  use M_CLI2
  implicit none

  integer(int32) :: i,j
  integer(int32) :: im,ip,jm,jp

  integer(int32) :: ncid_lev, lev_id           ! NetCDF ids
  integer(int32) :: dids_lev(1)           ! NetCDF ids
  integer(int32) :: zlen                   ! length of zeta array

  real(real64)  ::  zeta
  real(real64), allocatable :: zeta_arr(:)
  character(len=:), allocatable :: file_in, file_out

  type(topography_t) :: topog

  ! Parse command line arguments
  call set_args('--input:i "unset" --output:o "unset" --level:l 0.0')

  file_in = sget('input')
  file_out = sget('output')

  ! Sanity checks
  if (file_in == 'unset') then
    write(*,*) 'ERROR: no input file specified'
    stop
  else if (file_out == 'unset') then
    write(*,*) 'ERROR: no output file specified'
    stop
  end if

  call check_file_exist(file_in)

  ! Get info on the grid from input
  topog = topography_t(file_in)
  topog%min_level = rget('level')

  call handle_error(nf90_open('ocean_vgrid.nc', nf90_nowrite, ncid_lev))
  call handle_error(nf90_inq_varid(ncid_lev, 'zeta', lev_id))
  call handle_error(nf90_get_var(ncid_lev, lev_id, zeta, start=[2*topog%min_level+1]))
  topog%min_depth = zeta

  call handle_error(nf90_inquire_variable(ncid_lev, lev_id, dimids=dids_lev))
  call handle_error(nf90_inquire_dimension(ncid_lev, dids_lev(1), len=zlen))
  call handle_error(nf90_get_var(ncid_lev, lev_id, zeta, start=[zlen]))
  topog%max_depth = zeta

  call handle_error(nf90_close(ncid_lev))

  write(*,*) 'Setting minimum depth to ', topog%min_depth
  write(*,*) 'Setting maximum depth to ', topog%max_depth

  ! Reset depth
  do j = 1, topog%nyt
    do i = 1, topog%nxt
      if (topog%depth(i, j) > 0.0) then
        topog%depth(i, j) = min(max(topog%depth(i, j), topog%min_depth), topog%max_depth)
      else
        topog%depth(i, j) = MISSING_VALUE
      end if
    end do
  end do

  call topog%write(file_out)

end program min_max_depth
