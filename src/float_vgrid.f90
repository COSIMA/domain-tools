! Copyright 2020-2023 Micael Oliveira, Andrew Kiss, Russ Fiedler
!
! This Source Code Form is subject to the terms of the Mozilla Public
! License, v. 2.0. If a copy of the MPL was not distributed with this
! file, You can obtain one at https://mozilla.org/MPL/2.0/.

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

  call set_args('--vgrid "ocean_vgrid.nc" --vgrid_type "mom5"', help_text)

  file = sget('vgrid')
  call check_file_exist(file)

  vgrid = vgrid_t(file, sget('vgrid_type'))
  call vgrid%float()
  call vgrid%update_history(get_mycommand())
  call vgrid%write(file)

end program float_vgrid
