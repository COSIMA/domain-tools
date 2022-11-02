program deseas
  !
  ! Get rid of isolated seas
  !
  ! Simple diffusion algorithm. Sweep from SW to NE and then back.
  !
  !
  ! Usage: deseas file_in file_out
  use iso_fortran_env
  use netcdf
  use utils
  implicit none

  integer(int32) :: i, j, counter, its, its1, its2, sea_num, iblock, jblock, counter2
  integer(int32) :: im, ip, jm, jp, land
  integer(int32) :: nxt, nyt                ! Size of model T grid

  integer(int32) :: ncid_out, depth_id_out  ! NetCDF ids
  integer(int32) :: ncid_topo, depth_id     ! NetCDF ids
  integer(int32) :: dids_topo_out(2)        ! NetCDF ids
  integer(int32) :: dids_topo(2)            ! NetCDF ids

  real(real32), allocatable   :: depth(:,:)
  integer(int16), allocatable :: sea(:,:)
  character(len=128) :: file_in, file_out

  real(real32), parameter :: MY_MISS = -1e30

  logical :: choke_west, choke_east, choke_north, choke_south

  if (command_argument_count() .ne. 2) then
    write(*,*) 'ERROR: Wrong number of arguments'
    write(*,*) 'Usage:  deseas file_in file_out'
    stop
  end if

  call get_command_argument(1, file_in)
  call get_command_argument(2, file_out)

  ! Get info on the grid from input
  call handle_error(nf90_open(trim(file_in), nf90_nowrite, ncid_topo))
  call handle_error(nf90_inq_dimid(ncid_topo, 'nx', dids_topo(1)))
  call handle_error(nf90_inq_dimid(ncid_topo, 'ny', dids_topo(2)))
  call handle_error(nf90_inquire_dimension(ncid_topo, dids_topo(1), len=nxt))
  call handle_error(nf90_inquire_dimension(ncid_topo, dids_topo(2), len=nyt))

  call handle_error(nf90_inq_varid(ncid_topo, 'depth', depth_id))

  allocate(depth(nxt,nyt),sea(nxt, nyt))
  call handle_error(nf90_get_var(ncid_topo,depth_id, depth))
  call handle_error(nf90_close(ncid_topo))

  ! Do
  land = nxt + nyt + 1
  sea = land
  do j = 1, nyt
    do i = 1, nxt
      if (depth(i, j) > 0.0) sea(i, j) = i + j
    end do
    if (all(depth(:, j) > 0.0)) sea(:, j) = 0  ! Southern Ocean all water across
  end do

  do its = 1, 150   ! Only need high number after massive editing session with fjords. Normally 10 or so sweeps works.
    counter = 0
    sea_num = 1

    ! Get number of seas
    do j = 2, nyt - 1
      i = 1
      jm = j - 1
      jp = j + 1
      if (sea(i, j) < land  .and. sea(i, j) > 0) then
        if (sea(i, j) >= sea_num) then
          sea(i, j) = sea_num
          sea_num = max(min(sea_num+1, sea(nxt, j), sea(i, j-1), sea(i+1, j), sea(i, j+1)), sea_num)
        end if
      end if

      do i = 2, nxt - 1
        im = i - 1
        ip = i + 1
        if (sea(i, j) < land  .and. sea(i, j) > 0) then
          if (sea(i, j) >= sea_num) then
            sea(i, j) = sea_num
            sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(i+1, j), sea(i, j+1)), sea_num)
          end if
        end if
      end do

      i = nxt
      if (sea(i, j) < land  .and. sea(i, j) > 0) then
        if (sea(i, j) >= sea_num) then
          sea(i, j) = sea_num
          sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(1, j), sea(i, j+1)), sea_num)
        end if
      end if
    end do

    j = nyt
    do i = 2, nxt - 1
      if (sea(i, j) < land  .and. sea(i, j) > 0) then
        if (sea(i,j) >= sea_num) then
          sea(i,j) = sea_num
          sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(i+1, j)), sea_num)
        end if
      end if
    end do
                
    ! Diffuse lowest values surrounding a point.

    ! Forward sweep.
    do j = 2, nyt
      jm = j - 1
      jp = min(j+1, nyt)
      i = 1
      im = nxt
      ip = 2
      if (sea(i, j) < land .and. sea(i, j) > 0) then
        sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
        counter = counter + 1
      end if
      do i = 2, nxt - 1
        im = i - 1
        ip = i + 1
        if (sea(i, j) < land .and. sea(i, j) > 0) then
          if (all([sea(im, j), sea(ip, j), sea(i, j), sea(i, jm), sea(i, jp)] == land)) then
            sea(i, j) = land
          else
            !get chokes
            choke_east = .not. (any(sea(i:ip, jp) == land) .and. any(sea(i:ip, jm) == land))
            choke_west = .not. (any(sea(im:i, jp) == land) .and. any(sea(im:i, jm) == land))
            choke_south = .not. (any(sea(im, jm:j) == land) .and. any(sea(ip, jm:j) == land))
            choke_north = .not. (any(sea(im, j:jp) == land) .and. any(sea(ip, j:jp) == land))
            sea(i, j) = min(minval([sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp)], &
              mask=[choke_west, choke_east, choke_south, choke_north]), land)
          end if
          counter = counter + 1
        end if
      end do
      i = nxt
      im = 1
      ip = i - 1
      if (sea(i, j) < land .and. sea(i, j) > 0) then
        sea(i,j)=min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
        counter = counter + 1
      end if
    end do

    ! Backward sweep
    do j = nyt, 2, -1
      jm = j - 1
      jp = min(j+1, nyt)
      i = 1
      im = nxt
      ip = 2
      if (sea(i, j) < land .and. sea(i, j) > 0) then
        sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
        counter = counter + 1
      end if
      do i = nxt - 1, 2, -1
        im = i - 1
        ip = i + 1
        if (sea(i, j) < land .and. sea(i, j) > 0) then
          if (all([sea(im, j), sea(ip, j), sea(i, j), sea(i, jm), sea(i, jp)] == land)) then
            sea(i, j) = land
          else
            !get chokes
            choke_east = .not. (any(sea(i:ip, jp) == land) .and. any(sea(i:ip, jm) == land))
            choke_west = .not. (any(sea(im:i, jp) == land) .and. any(sea(im:i, jm) == land))
            choke_south = .not. (any(sea(im, jm:j) == land) .and. any(sea(ip, jm:j) == land))
            choke_north = .not. (any(sea(im, j:jp) == land) .and. any(sea(ip, j:jp) == land))
            sea(i, j) = min(minval([sea(im, j), sea(ip, j), sea(i, jm), sea(i,jp)], &
              mask=[choke_west, choke_east, choke_south, choke_north]), land)
          end if
          counter = counter + 1
        end if
      end do
      i = nxt
      im = 1
      ip = i - 1
      if (sea(i, j) < land .and. sea(i, j) > 0) then
        sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
        counter = counter + 1
      end if
    end do
    write(*,*) counter, sea_num

    ! If we only have one sea or no changes are made we are finished.
    if (counter == 0 .or. sea_num == 1) exit
  end do

  ! Write out new topography
  do j = 1, nyt
    do i = 1, nxt
      if (sea(i, j) > 0) depth(i, j) = MY_MISS
    end do
  end do


  write(*,*) 'Wring'
  call handle_error(nf90_create(trim(file_out), ior(nf90_netcdf4, nf90_clobber), ncid_out))
  call handle_error(nf90_def_dim(ncid_out, 'xx', nxt, dids_topo_out(1)))
  call handle_error(nf90_def_dim(ncid_out, 'yy', nyt,dids_topo_out(2)))
  call handle_error(nf90_def_var(ncid_out, 'depth', nf90_float, dids_topo_out, depth_id_out, chunksizes=[nxt/10, nyt/10], &
    deflate_level=1, shuffle=.true.))
  call handle_error(nf90_put_att(ncid_out, depth_id_out, 'missing_value', MY_MISS))
  call handle_error(nf90_put_att(ncid_out, depth_id_out, 'long_name', 'depth'))
  call handle_error(nf90_put_att(ncid_out, depth_id_out, 'units', 'm'))
  call handle_error(nf90_put_att(ncid_out, nf90_global, 'original_file', trim(file_in)))
  call handle_error(nf90_put_att(ncid_out, depth_id_out, 'lakes_removed', 'yes'))
  call handle_error(nf90_enddef(ncid_out))
  write(*,*) 'putting'
  call handle_error(nf90_put_var(ncid_out, depth_id_out, depth))
  call handle_error(nf90_close(ncid_out))

  call handle_error(nf90_create(trim('sea_num'), ior(nf90_netcdf4, nf90_clobber), ncid_out))
  call handle_error(nf90_def_dim(ncid_out, 'xx', nxt, dids_topo_out(1)))
  call handle_error(nf90_def_dim(ncid_out, 'yy', nyt, dids_topo_out(2)))
  call handle_error(nf90_def_var(ncid_out, 'sea_num', nf90_short, dids_topo_out, depth_id_out, chunksizes=[nxt/10, nyt/10], &
    deflate_level=1, shuffle=.true.))
  call handle_error(nf90_enddef(ncid_out))
  call handle_error(nf90_put_var(ncid_out, depth_id_out, sea))
  call handle_error(nf90_close(ncid_out))

end program deseas
