module topog2mask_m
  use iso_fortran_env
  use topography
  use M_CLI2
  implicit none

contains

  subroutine topog2mask(topog_file, mask_file, sea_area_fraction)
    character(len=*), intent(in) :: topog_file, mask_file
    real(real32), intent(in) :: sea_area_fraction

    type(topography_t) :: topog

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

  end subroutine topog2mask

end module topog2mask_m
