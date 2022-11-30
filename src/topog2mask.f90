program topog2mask
  use iso_fortran_env
  use topography
  use M_CLI2
  implicit none

  type(topography_t) :: topog
  character(len=:), allocatable :: topog_file, mask_file
  real(real32) :: sea_area_fraction

  ! Parse command line arguments
  call set_args('--topography:t "unset" --mask:m "unset" --fraction 0.0')

  topog_file = sget('topography')
  mask_file = sget('mask')
  sea_area_fraction = dget('fraction')

  ! Sanity checks
  if (topog_file == 'unset') then
    write(*,*) 'ERROR: no topography file specified'
    stop
  else if (mask_file == 'unset') then
    write(*,*) 'ERROR: no mask file specified'
    stop
  end if

  call check_file_exist(topog_file)

  if (sea_area_fraction < 0.0 .and. sea_area_fraction > 1.0) then
    write(*,*) "ERROR: sea area fraction must be between 0 and 1"
    stop
  end if

  ! Get info on the grid from input
  topog = topography_t(topog_file)

  ! Compute and output the mask
  call topog%mask(mask_file, sea_area_fraction)

end program topog2mask
