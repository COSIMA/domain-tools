program fix_nonadvective
  ! Find non advective columns
  ! Write out corrdinates
  use iso_fortran_env
  use netcdf
  use utils
  use topography
  use M_CLI2
  implicit none

  real(real32), allocatable :: depth_halo(:,:)
  integer(int32), allocatable :: num_levels(:,:)
  real(real32), allocatable :: zw(:), zeta(:)
  integer(int32) :: ierr, i, j, k, ni, nj, nzeta, nz, its, counter
  integer(int32) :: ncid, vid
  integer(int32) :: dids(2)
  logical :: se, sw, ne, nw   ! .TRUE. if C-cell centre is shallower than T cell centre.
  logical :: changes_made = .false.
  integer(int32) :: kse, ksw, kne, knw, kmu_max
  integer(int32) :: im, ip, jm, jp

  character(len=:), allocatable :: file_in, file_out
  type(topography_t) :: topog

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
  topog = topography_t(file_in)

  call handle_error(nf90_open('ocean_vgrid.nc', nf90_nowrite, ncid))
  call handle_error(nf90_inq_varid(ncid, 'zeta', vid))
  call handle_error(nf90_inquire_variable(ncid, vid, dimids=dids))
  call handle_error(nf90_inquire_dimension(ncid, dids(1), len=nzeta))
  nz = nzeta/2
  write(*,*) 'Zeta dimensions', nzeta, nz
  allocate(zeta(nzeta), zw(0:nz))
  call handle_error(nf90_get_var(ncid, vid, zeta))
  call handle_error(nf90_close(ncid))
  zw(:) = zeta(1:nzeta:2)

  write(*,*) 'depth dimensions', topog%nxt, topog%nyt
  allocate(depth_halo(0:topog%nxt+1, topog%nyt+1))
  allocate(num_levels(0:topog%nxt+1, topog%nyt+1))

  do its = 1, 20
    counter = 0
    num_levels = 0

    depth_halo = 0
    depth_halo(1:topog%nxt, 1:topog%nyt) = topog%depth
    depth_halo(0, 1:topog%nyt) = topog%depth(topog%nxt, :)
    depth_halo(topog%nxt+1, 1:topog%nyt)=topog%depth(1, :)
    depth_halo(1:topog%nxt, topog%nyt+1) = topog%depth(topog%nxt:1:-1, topog%nyt)
    do j = 1, topog%nyt + 1
      do i = 0, topog%nxt + 1
        if (depth_halo(i, j) > 0.0) then
          kloop: do k = 2, nz
            if (zw(k) >= depth_halo(i, j)) then
              num_levels(i, j) = k
              exit kloop
            end if
          end do kloop
        end if
      end do
    end do

    do j = 2, topog%nyt - 1
      do i = 1, topog%nxt
        if (depth_halo(i, j) > 0.5) then
          sw = depth_halo(i-1, j) < 0.5 .or. depth_halo(i-1, j-1) < 0.5 .or. depth_halo(i, j-1) < 0.5
          se = depth_halo(i, j-1) < 0.5 .or. depth_halo(i+1, j-1) < 0.5 .or. depth_halo(i+1, j) < 0.5
          ne = depth_halo(i+1, j) < 0.5 .or. depth_halo(i, j+1) < 0.5 .or. depth_halo(i+1, j+1) < 0.5
          nw = depth_halo(i-1, j) < 0.5 .or. depth_halo(i-1, j+1) < 0.5 .or. depth_halo(i, j+1) < 0.5
          if (all([se, sw, ne, nw])) then
            depth_halo(i, j) = 0.0
            topog%frac(i, j) = 0.0
            num_levels(i, j) = 0
            counter = counter + 1
            write(*,*) i, j, 0.0 ,'  ! nonadvective'
          end if
        end if
      end do
    end do

    write(*,*) '1', counter

    do j = 2, topog%nyt
      jm = j - 1
      jp = j + 1
      do i = 1, topog%nxt
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
            depth_halo(i, j) = zw(kmu_max)
            counter = counter + 1
          end if
        end if
      end do
    end do
    if (counter > 0) changes_made = .true.
    write(*,*) counter
    topog%depth = depth_halo(1:topog%nxt, 1:topog%nyt)
    if (counter == 0) exit
  end do

  topog%nonadvective_cells_removed = 'yes'
  topog%lakes_removed = 'no'
  call topog%write(file_out)

end program fix_nonadvective
