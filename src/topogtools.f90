program topogtools
  use, intrinsic :: iso_fortran_env
  use M_CLI2
  use gen_topo_m
  use topography
  implicit none

  character(len=:), allocatable :: name
  character(len=:), allocatable :: help_general(:), help_gen_topo(:), help_deseas(:), help_min_max_depth(:)
  character(len=:), allocatable :: help_fix_nonadvective(:), help_check_nonadvective(:), help_mask(:)
  character(len=:), allocatable :: file_in, file_out, hgrid, vgrid
  type(topography_t) :: topog
  real(real32) :: sea_area_fraction
  integer :: ii

  ! Help texts
  help_general = [character(len=80) :: &
    'usage: topogtools [--help] <command> [<args>]                                   ', &
    '                                                                                ', &
    'Collection of tools to edit and manipulate ocean model topographies.            ', &
    'See ''topogtools --help <command>'' to read about a specific subcommand.          ', &
    '                                                                                ', &
    'Available commands:                                                             ', &
    '  gen_topo - Generate a new topography file from a bathymetry                   ', &
    '  deseas - Remove enclosed seas                                                 ', &
    '  min_max_depth - Set minimum and maximum depth                                 ', &
    '  check_nonadvective - Check for non-advective cells                            ', &
    '  fix_nonadvective - Fix non-advective cells                                    ', &
    '  mask - Generate mask                                                          ', &
    '']
  help_gen_topo = [character(len=80) :: &
    'usage: topogtools gen_topo --input <input_file> --output <output_file>          ', &
    '                           --hgrid <grid> [<options>]                           ', &
    '                                                                                ', &
    'Generate a new topography from <input_file> on the tracer points of <grid> and  ', &
    'writes the result to <output_file>. Note that <grid> must be a super-grid       ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --tripolar                  horizontal grid is a tripolar grid              ', &
    '    --longitude-offset <value>  offset (in degrees) between the central         ', &
    '                                longitude of the ocean horizontal grid and of   ', &
    '                                the bathymetry grid (default ''0.0'')             ', &
    '']
  help_deseas = [character(len=80) :: &
    'usage: topogtools deseas --input <input_file> --output <output_file>            ', &
    '                                                                                ', &
    'Remove enclosed seas from <input_file> and writes the result to <output_file>.  ', &
    '']
  help_min_max_depth = [character(len=80) :: &
    'usage: topogtools min_max_depth --input <input_file> --output <output_file>     ', &
    '                                --level <level> [--vgrid <vgrid>]               ', &
    '                                                                                ', &
    'Set minimum depth to the depth at a specified level and set maximum depth to    ', &
    'deepest in <vgrid>. <level> is the minimum number of depth levels (e.g. 4).     ', &
    'Can produce non-advective cells.                                                ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --vgrid <vgrid>  vertical grid (default ''ocean_vgrid.nc'')                   ', &
    '']
  help_fix_nonadvective = [character(len=80) :: &
    'usage: topogtools fix_nonadvective --input <input_file> --output <output_file>  ', &
    '                                   [--vgrid <vgrid>]                            ', &
    '                                                                                ', &
    'Fix cells that are non-advective on a B grid.                                   ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --vgrid <vgrid>  vertical grid (default ''ocean_vgrid.nc'')                   ', &
    '']
  help_check_nonadvective = [character(len=80) :: &
    'usage: topogtools check_nonadvective --input <input_file> [--vgrid <vgrid>]     ', &
    '                                                                                ', &
    'Check for cells that are nonadvective on a B grid.                              ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --vgrid <vgrid>  vertical grid (default ''ocean_vgrid.nc'')                   ', &
    '']
  help_mask = [character(len=80) :: &
    'usage: topogtools mask  --input <input_file> --output <output_file>             ', &
    '                        [--fraction <frac>]                                     ', &
    '                                                                                ', &
    'Creates a land mask from a topography.                                          ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --fraction <frac>  cells with a fraction of sea area smaller than <frac>    ', &
    '                       will be set as land (default ''0.0'')                    ', &
    '']

  ! Read command line
  name = get_subcommand()
  select case (name)
  case ('gen_topo')
    call set_args('--input:i "unset" --output:o "unset" --hgrid "ocean_hgrid.nc" --tripolar F --longitude-offset 0.0', &
      help_gen_topo)
  case ('deseas')
    call set_args('--input:i "unset" --output:o "unset"', help_deseas)
  case ('min_max_depth')
    call set_args('--input:i "unset" --output:o "unset" --vgrid "ocean_vgrid.nc" --level 0', help_min_max_depth)
  case ('fix_nonadvective')
    call set_args('--input:i "unset" --output:o "unset" --vgrid "ocean_vgrid.nc"', help_fix_nonadvective)
  case ('check_nonadvective')
    call set_args('--input:i "unset" --vgrid "ocean_vgrid.nc"', help_check_nonadvective)
  case ('mask')
    call set_args('--input:i "unset" --output:o "unset" --fraction 0.0', help_mask)
  case ('')
    ! Print help in case the user specified the --help flag
    call set_args(' ', help_general)
    ! Also print even if the user did not specify --help
    if (.not. lget('help')) then
      write(output_unit,'(g0)') (trim(help_general(ii)), ii=1, size(help_general))
      stop
    end if
  case default
    write(error_unit,'(3a)') "topogtools: '", trim(name), "' is not a topogtools subcommand. See 'topogtools --help'."
    error stop
  end select

  ! Sanity checks for common arguments
  file_in = sget('input')
  if (file_in == 'unset') then
    write(error_unit,'(a)') 'ERROR: no input file specified'
    error stop
  end if
  call check_file_exist(file_in)

  select case (name)
  case ('gen_topo', 'deseas', 'min_max_depth', 'fix_nonadvective', 'mask')
    file_out = sget('output')
    if (file_out == 'unset') then
      write(error_unit,'(a)') 'ERROR: no output file specified'
      error stop
    end if
  end select

  select case (name)
  case ('min_max_depth', 'fix_nonadvective', 'check_nonadvective')
    vgrid = sget('vgrid')
    call check_file_exist(vgrid)
  end select

  ! Run subcommand
  select case (name)
  case ('gen_topo')
    hgrid = sget('hgrid')
    call check_file_exist(hgrid)
    call gen_topo(file_in, file_out, hgrid, lget('tripolar'), rget('longitude-offset'))

  case ('deseas')
    topog = topography_t(file_in)
    call topog%deseas()
    call topog%update_history(get_mycommand())
    call topog%write(file_out)

  case ('min_max_depth')
    topog = topography_t(file_in)
    call topog%min_max_depth(vgrid, iget('level'))
    call topog%update_history(get_mycommand())
    call topog%write(file_out)

  case ('fix_nonadvective')
    topog = topography_t(file_in)
    call topog%nonadvective(vgrid, fix=.true.)
    call topog%update_history(get_mycommand())
    call topog%write(file_out)

  case ('check_nonadvective')
    topog = topography_t(file_in)
    call topog%nonadvective(vgrid, fix=.false.)

  case ('mask')
    sea_area_fraction = rget('fraction')
    if (sea_area_fraction < 0.0 .and. sea_area_fraction > 1.0) then
      write(error_unit,'(a)') "ERROR: sea area fraction must be between 0 and 1"
      error stop
    end if
    topog = topography_t(file_in)
    call topog%mask(file_out, sea_area_fraction)

  end select

end program topogtools
