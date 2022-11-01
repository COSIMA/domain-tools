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
  implicit none

  integer(int32) :: i,j,min_level
  integer(int32) :: im,ip,jm,jp
  integer(int32) :: nxt,nyt,nzt                ! Size of model T grid

  integer(int32) :: ncid_out,depth_id_out           ! NetCDF ids
  integer(int32) :: ncid_topo, depth_id           ! NetCDF ids
  integer(int32) :: ncid_lev, lev_id           ! NetCDF ids
  integer(int32) :: dids_topo_out(2)           ! NetCDF ids
  integer(int32) :: dids_topo(2)           ! NetCDF ids
  integer(int32) :: dids_lev(1)           ! NetCDF ids
  integer(int32) :: zlen                   ! length of zeta array

  real(real32), allocatable, dimension(:,:)   :: depth
  real(real64)  ::  zeta
  real(real64), dimension(:), allocatable  ::  zeta_arr
  real(real32)  :: min_depth, max_depth
  character*128 :: file_in,file_out,level

  real(real32), parameter :: missing_value = -1e30

  if (command_argument_count() /= 3) then
    write(*,*) 'ERROR: Incorrect number of arguments'
    write(*,*) 'Usage: min_depth file_in file_out level'
  end if

  call get_command_argument(1,file_in)
  call get_command_argument(2,file_out)
  call get_command_argument(3,level)
  read(level,*) min_level

  ! Get info on the grid from input
  call handle_error(nf90_open('ocean_vgrid.nc', nf90_nowrite, ncid_lev))
  call handle_error(nf90_inq_varid(ncid_lev, 'zeta', lev_id))
  call handle_error(nf90_get_var(ncid_lev, lev_id, zeta, start=[2*min_level+1]))
  min_depth = zeta

  call handle_error(nf90_inquire_variable(ncid_lev, lev_id, dimids=dids_lev))
  call handle_error(nf90_inquire_dimension(ncid_lev, dids_lev(1), len=zlen))
  call handle_error(nf90_get_var(ncid_lev, lev_id, zeta, start=[zlen]))

  max_depth = zeta

  call handle_error(nf90_close(ncid_lev))

  write(*,*) 'Setting minimum depth to ', min_depth
  write(*,*) 'Setting maximum depth to ', max_depth

  call handle_error(nf90_open(trim(file_in), nf90_nowrite, ncid_topo))
  call handle_error(nf90_inq_dimid(ncid_topo, 'xx', dids_topo(1)))
  call handle_error(nf90_inq_dimid(ncid_topo, 'yy', dids_topo(2)))
  call handle_error(nf90_inquire_dimension(ncid_topo, dids_topo(1), len=nxt))
  call handle_error(nf90_inquire_dimension(ncid_topo, dids_topo(2), len=nyt))
  call handle_error(nf90_inq_varid(ncid_topo, 'depth', depth_id))

  allocate(depth(nxt, nyt))

  call handle_error(nf90_get_var(ncid_topo, depth_id, depth))
  call handle_error(nf90_close(ncid_topo))

  ! Reset depth
  do j = 1, nyt
    do i = 1, nxt
      if (depth(i, j) > 0.0) then
        depth(i, j) = min(max(depth(i, j), min_depth), max_depth)
      else
        depth(i, j) = missing_value
      end if
    end do
  end do

  call handle_error(nf90_create(trim(file_out), ior(nf90_netcdf4, nf90_clobber), ncid_out))
  call handle_error(nf90_def_dim(ncid_out, 'xx', nxt,dids_topo_out(1)))
  call handle_error(nf90_def_dim(ncid_out, 'yy', nyt,dids_topo_out(2)))
  call handle_error(nf90_def_var(ncid_out, 'depth', nf90_float, dids_topo_out, depth_id_out, chunksizes=[nxt/10, nyt/10], &
    deflate_level=1, shuffle=.true.))
  call handle_error(nf90_put_att(ncid_out, depth_id_out, 'missing_value', missing_value))
  call handle_error(nf90_put_att(ncid_out, depth_id_out, 'long_name', 'depth'))
  call handle_error(nf90_put_att(ncid_out, depth_id_out, 'units', 'm'))
  call handle_error(nf90_put_att(ncid_out, depth_id,'lakes_removed', 'yes'))
  call handle_error(nf90_put_att(ncid_out, depth_id,'minimum_depth', min_depth))
  call handle_error(nf90_put_att(ncid_out, depth_id,'minimum_levels', min_level))
  call handle_error(nf90_put_att(ncid_out, nf90_global,'original_file', trim(file_in)))
  call handle_error(nf90_enddef(ncid_out))
  call handle_error(nf90_put_var(ncid_out, depth_id_out, depth))
  call handle_error(nf90_close(ncid_out))

contains

  subroutine handle_error(error_flag, isfatal, err_string)
    ! Simple error handle for NetCDF
    integer(int32), intent(in) :: error_flag
    logical, intent(in), optional :: isfatal
    character(*), intent(in), optional :: err_string

    logical            :: fatal

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
    
end program min_max_depth
