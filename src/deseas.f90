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
  use topography
  use M_CLI2
  implicit none

  integer(int32) :: i, j, counter, its, its1, its2, sea_num, iblock, jblock, counter2
  integer(int32) :: im, ip, jm, jp, land

  type(topography_t) :: topog

  integer(int32) :: ncid, sea_id, dids(2)  ! NetCDF ids

  integer(int16), allocatable :: sea(:,:)
  character(len=:), allocatable :: file_in, file_out

  logical :: choke_west, choke_east, choke_north, choke_south

  ! Parse command line arguments
  call set_args('--input:i "unset" --output:o "unset"')

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

  allocate(sea(topog%nxt, topog%nyt))

  ! Do
  write(*,*) "Removing seas"
  land = topog%nxt + topog%nyt + 1
  sea = land
  do j = 1, topog%nyt
    do i = 1, topog%nxt
      if (topog%depth(i, j) > 0.0) sea(i, j) = i + j
    end do
    if (all(topog%depth(:, j) > 0.0)) sea(:, j) = 0  ! Southern Ocean all water across
  end do

  do its = 1, 150   ! Only need high number after massive editing session with fjords. Normally 10 or so sweeps works.
    counter = 0
    sea_num = 1

    ! Get number of seas
    do j = 2, topog%nyt - 1
      i = 1
      jm = j - 1
      jp = j + 1
      if (sea(i, j) < land  .and. sea(i, j) > 0) then
        if (sea(i, j) >= sea_num) then
          sea(i, j) = sea_num
          sea_num = max(min(sea_num+1, sea(topog%nxt, j), sea(i, j-1), sea(i+1, j), sea(i, j+1)), sea_num)
        end if
      end if

      do i = 2, topog%nxt - 1
        im = i - 1
        ip = i + 1
        if (sea(i, j) < land  .and. sea(i, j) > 0) then
          if (sea(i, j) >= sea_num) then
            sea(i, j) = sea_num
            sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(i+1, j), sea(i, j+1)), sea_num)
          end if
        end if
      end do

      i = topog%nxt
      if (sea(i, j) < land  .and. sea(i, j) > 0) then
        if (sea(i, j) >= sea_num) then
          sea(i, j) = sea_num
          sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(1, j), sea(i, j+1)), sea_num)
        end if
      end if
    end do

    j = topog%nyt
    do i = 2, topog%nxt - 1
      if (sea(i, j) < land  .and. sea(i, j) > 0) then
        if (sea(i,j) >= sea_num) then
          sea(i,j) = sea_num
          sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(i+1, j)), sea_num)
        end if
      end if
    end do
                
    ! Diffuse lowest values surrounding a point.

    ! Forward sweep.
    do j = 2, topog%nyt
      jm = j - 1
      jp = min(j+1, topog%nyt)
      i = 1
      im = topog%nxt
      ip = 2
      if (sea(i, j) < land .and. sea(i, j) > 0) then
        sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
        counter = counter + 1
      end if
      do i = 2, topog%nxt - 1
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
      i = topog%nxt
      im = 1
      ip = i - 1
      if (sea(i, j) < land .and. sea(i, j) > 0) then
        sea(i,j)=min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
        counter = counter + 1
      end if
    end do

    ! Backward sweep
    do j = topog%nyt, 2, -1
      jm = j - 1
      jp = min(j+1, topog%nyt)
      i = 1
      im = topog%nxt
      ip = 2
      if (sea(i, j) < land .and. sea(i, j) > 0) then
        sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
        counter = counter + 1
      end if
      do i = topog%nxt - 1, 2, -1
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
      i = topog%nxt
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

  write(*,*) "Done"

  ! Write out new topography
  do j = 1, topog%nyt
    do i = 1, topog%nxt
      if (sea(i, j) > 0) then
        topog%depth(i, j) = MISSING_VALUE
        topog%frac(i, j) = MISSING_VALUE
      end if
    end do
  end do
  topog%lakes_removed = "yes"
  call topog%write(file_out)

  call handle_error(nf90_create(trim('sea_num.nc'), ior(nf90_netcdf4, nf90_clobber), ncid))
  call handle_error(nf90_def_dim(ncid, 'nx', topog%nxt, dids(1)))
  call handle_error(nf90_def_dim(ncid, 'ny', topog%nyt, dids(2)))
  call handle_error(nf90_def_var(ncid, 'sea_num', nf90_short, dids, sea_id, chunksizes=[topog%nxt/10, topog%nyt/10], &
    deflate_level=1, shuffle=.true.))
  call handle_error(nf90_enddef(ncid))
  call handle_error(nf90_put_var(ncid, sea_id, sea))
  call handle_error(nf90_close(ncid))

end program deseas
