program float_vgrid
  ! Zeta is double precision. Convert to  single and rewrite. This stops small floating point errors.
  use M_CLI2
  use vgrid
  use utils
  implicit none

  type(vgrid_t) :: vgrid
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

  file = sget('vgrid')
  call check_file_exist(file)

  vgrid = vgrid_t(file)
  call vgrid%float()
  call vgrid%update_history(get_mycommand())
  call vgrid%write(file)

end program float_vgrid
