program fix_nonadvective
  ! Find non advective columns
  ! Write out corrdinates
  use netcdf
  implicit none

  real, allocatable :: topog(:,:), topog_halo(:,:)
  integer, allocatable :: num_levels(:,:)
  real, allocatable :: zw(:), zeta(:)
  integer :: ierr, i, j, k, ni, nj, nzeta, nz, its, counter
  integer :: ncid, vid
  integer :: dids(2)
  logical :: se, sw, ne, nw   ! .TRUE. if C-cell centre is shallower than T cell centre.
  logical :: changes_made = .false.
  integer :: kse, ksw, kne, knw, kmu_max
  integer :: im, ip, jm, jp

  character(len=128) :: file_in, file_out

  if (command_argument_count() .ne. 2 ) then
    write(*,*) 'ERROR: Wrong number of arguments'
    write(*,*) 'Usage:  deseas file_in file_out'
    stop
  endif
  call get_command_argument(1, file_in)
  call get_command_argument(2, file_out)

  call execute_command_line('/bin/cp '//trim(file_in)//' '//trim(file_out))

  ierr = nf90_open('ocean_vgrid.nc', nf90_nowrite, ncid)
  ierr = nf90_inq_varid(ncid, 'zeta', vid)
  ierr = nf90_inquire_variable(ncid, vid, dimids=dids)
  ierr = nf90_inquire_dimension(ncid, dids(1), len=nzeta)
  nz = nzeta/2
  write(*,*) 'Zeta dimensions', nzeta, nz
  allocate(zeta(nzeta), zw(0:nz))
  ierr = nf90_get_var(ncid, vid, zeta)
  ierr = nf90_close(ncid)
  zw(:) = zeta(1:nzeta:2)

  ierr = nf90_open(trim(file_out), nf90_write, ncid)
  ierr = nf90_inq_varid(ncid,'depth', vid)
  ierr = nf90_inquire_variable(ncid, vid, dimids=dids)
  ierr = nf90_inquire_dimension(ncid, dids(1), len=ni)
  ierr = nf90_inquire_dimension(ncid, dids(2), len=nj)
  write(*,*) 'depth dimensions', ni, nj
  allocate(topog(ni, nj))
  allocate(topog_halo(0:ni+1, nj+1))
  allocate(num_levels(0:ni+1, nj+1))

  ierr = nf90_get_var(ncid, vid, topog)
  do its = 1, 20
    counter = 0
    num_levels = 0

    topog_halo = 0
    topog_halo(1:ni, 1:nj) = topog
    topog_halo(0, 1:nj) = topog(ni, :)
    topog_halo(ni+1, 1:nj)=topog(1, :)
    topog_halo(1:ni, nj+1) = topog(ni:1:-1, nj)
    do j = 1, nj + 1
      do i = 0, ni + 1
        if (topog_halo(i, j) > 0.0) then
          kloop: do k = 2, nz
            if (zw(k) >= topog_halo(i, j)) then
              num_levels(i, j) = k
              exit kloop
            end if
          end do kloop
        end if
      end do
    end do

    do j = 2, nj - 1
      do i = 1, ni
        if (topog_halo(i, j) > 0.5) then
          sw = topog_halo(i-1, j) < 0.5 .or. topog_halo(i-1, j-1) < 0.5 .or. topog_halo(i, j-1) < 0.5
          se = topog_halo(i, j-1) < 0.5 .or. topog_halo(i+1, j-1) < 0.5 .or. topog_halo(i+1, j) < 0.5
          ne = topog_halo(i+1, j) < 0.5 .or. topog_halo(i, j+1) < 0.5 .or. topog_halo(i+1, j+1) < 0.5
          nw = topog_halo(i-1, j) < 0.5 .or. topog_halo(i-1, j+1) < 0.5 .or. topog_halo(i, j+1) < 0.5
          if (all([se, sw, ne, nw])) then
            topog_halo(i, j) = 0.0
            num_levels(i, j) = 0
            counter = counter + 1
            write(*,*) i, j, 0.0 ,'  ! nonadvective'
          end if
        end if
      end do
    end do

    write(*,*) '1', counter

    do j = 2, nj
      jm = j - 1
      jp = j + 1
      do i = 1, ni
        im = i - 1
        ip = i + 1
        if (num_levels(i, j) > 0) then
          ksw = minval([num_levels(im, jm), num_levels(i, jm), num_levels(im, j)])
          kse = minval([num_levels(i, jm), num_levels(ip, jm), num_levels(ip, j)])
          knw = minval([num_levels(im, j), num_levels(im,jp), num_levels(i, jp)])
          kne = minval([num_levels(ip, j), num_levels(i,jp), num_levels(ip, jp)])

          kmu_max = maxval([ksw, kse, knw, kne])

          if (num_levels(i, j) > kmu_max) then
            num_levels(i, j) = kmu_max
            topog_halo(i, j) = zw(kmu_max)
            counter = counter + 1
          end if
        end if
      end do
    end do
    if (counter > 0) changes_made = .true.
    write(*,*) counter
    topog = topog_halo(1:ni, 1:nj)
    if (counter == 0) exit
  end do
  ierr = nf90_redef(ncid)
  ierr = nf90_put_att(ncid, vid, 'nonadvective_cells_removed', 'yes')
  if (changes_made) then
    ierr=nf90_put_att(ncid, vid, 'lakes_removed', 'no')
  end if
  ierr = nf90_enddef(ncid)
  ierr = nf90_put_var(ncid, vid, topog)
  ierr = nf90_close(ncid)

end program fix_nonadvective
