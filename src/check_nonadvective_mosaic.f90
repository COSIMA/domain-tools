program check_nonadvective
  ! Find non advective columns
  ! Write out corrdinates
  !
  ! Usage:
  !
  ! check_nodadvective_mosaic file_in
  use iso_fortran_env
  use netcdf
  implicit none

  integer(int32) :: ierr, i, j, k, ni, nj, nzeta, nz, im, ip, jm, jp

  real(real32), dimension(:,:), allocatable :: topog
  real(real32), dimension(:), allocatable :: zw, zeta

  integer(int32), dimension(:,:), allocatable :: num_levels

  integer(int32) :: ncid, vid
  integer(int32), dimension(2) :: dids

  character*256 :: file_in

  logical :: se, sw, ne, nw
  integer :: kse, ksw, kne, knw, kmu_max

  call get_command_argument(1, file_in)

  if (len_trim(file_in) == 0) then
    write(*,*) 'ERROR:: Must provide a file'
    stop 1
  end if

  call handle_error(nf90_open(trim(file_in), nf90_nowrite, ncid))
  call handle_error(nf90_inq_varid(ncid, 'depth', vid))
  call handle_error(nf90_inquire_variable(ncid, vid, dimids=dids))
  call handle_error(nf90_inquire_dimension(ncid, dids(1), len=ni))
  call handle_error(nf90_inquire_dimension(ncid, dids(2), len=nj))

  allocate(topog(ni, nj))
  allocate(num_levels(ni, nj))

  call handle_error(nf90_get_var(ncid, vid, topog))
  call handle_error(nf90_close(ncid))
  call handle_error(nf90_open('ocean_vgrid.nc', nf90_nowrite, ncid))
  call handle_error(nf90_inq_varid(ncid, 'zeta', vid))
  call handle_error(nf90_inquire_variable(ncid, vid, dimids=dids))
  call handle_error(nf90_inquire_dimension(ncid, dids(1), len=nzeta))

  nz = nzeta/2
  print *, 'depth dimensions', ni, nj
  print *, 'Zeta dimensions', nzeta, nz

  allocate(zeta(nzeta), zw(0:nz))

  call handle_error(nf90_get_var(ncid, vid, zeta))

  zw(:) = zeta(1:nzeta:2)
  num_levels = 0

  ! Get number of levels

  do j = 1, nj
    do i = 1, ni
      if (topog(i, j) > 0.0) then
        kloop: do k = 2, nz
          if (zw(k) >= topog(i, j)) then
            num_levels(i, j) = k
            exit kloop
          end if
        end do kloop
      end if
    end do
  end do
      

  do j = 2, nj - 1

    jm = j - 1
    jp = j + 1

    do i = 1, ni

      im = i - 1
      ip = i + 1
      if (im == 0) im = ni
      if (ip == 0) ip = 1

      if (topog(i, j) > 0.5) then
        sw = topog(im, j) < 0.5 .or. topog(im, jm) < 0.5 .or. topog(i, jm) < 0.5
        se = topog(i, jm) < 0.5 .or. topog(ip, jm) < 0.5 .or. topog(ip, j) < 0.5
        ne = topog(i, jp) < 0.5 .or. topog(i, jp) < 0.5 .or. topog(ip, jp) < 0.5
        nw = topog(im, j) < 0.5 .or. topog(i-1, jp) < 0.5 .or. topog(i, jp) < 0.5
!        sw=topog_halo(im,j)<0.5 .or. topog_halo(im,jm)<0.5 .or. topog_halo(i,jm)<0.5
!        se=topog_halo(i,jm)<0.5 .or. topog_halo(ip,jm)<0.5 .or. topog_halo(ip,j)<0.5
!        ne=topog_halo(i,jp)<0.5 .or. topog_halo(i,jp)<0.5 .or. topog_halo(ip,jp)<0.5
!        nw=topog_halo(im,j)<0.5 .or. topog_halo(i-1,jp)<0.5 .or. topog_halo(i,jp)<0.5
        if (all([se, sw, ne, nw])) then
          write(*,*) i, j, 0.0, '  ! nonadvective'
        end if
      end if
    end do
  end do

  ! Northern fold
  j = nj

  ! Can do x check normally
  do i = 1, ni

    im = i - 1
    ip = i + 1
    if (im == 0) im = ni
    if (ip == 0) ip = 1

    if (topog(i, j) > 0.5) then
      if (topog(im, j) < 0.5 .and. topog(ip, j) < 0.5) then
        write(*,*) i, j, '  Surface East west nonadvective'
      end if
    end if
  end do

  ! The point "North" of i,nj is ni-i+1,nj
  do i = 1, ni
    if (topog(i, j) > 0.5) then
      if (topog(i, nj-1) < 0.5 .and. topog(ni-i+1, nj) < 0.5) then
        write(*,*) i, j, '  Surface North south nonadvective'
      end if
    end if
  end do

  ! Check deeper levels
  do j = 2, nj - 1

    jm = j - 1
    jp = j + 1

    do i = 1, ni

      im = i - 1
      ip = i + 1
      if (im == 0) im = ni
      if (ip == ni + 1) ip = 1

      ksw = minval([num_levels(im, jm), num_levels(i, jm), num_levels(im, j)])
      kse = minval([num_levels(i, jm), num_levels(ip, jm), num_levels(ip, j)])
      knw = minval([num_levels(im, j), num_levels(im, jp), num_levels(i, jp)])
      kne = minval([num_levels(ip, j), num_levels(i, jp), num_levels(ip, jp)])

      kmu_max = maxval([ksw, kse, knw, kne])
      if (num_levels(i, j) > 0) then
        if (num_levels(i, j) > kmu_max) then
          write(*,*) i, j, '   nonadvective, Deep', num_levels(i, j), kmu_max
!          if (num_levels(im, j) < num_levels(i, j) .and. num_levels(ip, j) < num_levels(i, j)) then
!            write(*,*) i, j, '   East west nonadvective, Deep', num_levels(im, j), num_levels(i, j), num_levels(ip, j)
!          else if (num_levels(i, jm) < num_levels(i, j) .and. num_levels(i, jp) < num_levels(i, j)) then
!            write(*,*) i, j, '   North south nonadvective, Deep', num_levels(im, j), num_levels(i, j), num_levels(ip, j)
        end if
      end if
    end do

  end do

  j = nj

  ! Can do x check normally
  do i = 1, ni

    im = i - 1
    ip = i + 1
    if (im == 0) im = ni
    if (ip == ni + 1) ip = 1

    if (num_levels(i, j) > 0) then
      if (num_levels(im, j) < num_levels(i, j) .and. num_levels(ip, j) < num_levels(i, j)) then
        write(*,*) i, j, '   East west nonadvective, Deep', num_levels(im, j), num_levels(i, j), num_levels(ip, j)
      end if
    end if
  end do


  ! The point "North" of i,nj is ni-i+1,nj
  do i = 1, ni
    if (num_levels(i, nj) > 0) then
      if (num_levels(i, nj) < num_levels(i, nj) .and. num_levels(ni-i+1, nj) < num_levels(i, nj)) then
        write(*,*) i, j, '   North south nonadvective, Deep', num_levels(i, nj-1:nj), num_levels(ni-i+1, nj)
      end if
    end if
  end do

contains

  subroutine handle_error(error_flag,isfatal,err_string)
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

end program check_nonadvective
