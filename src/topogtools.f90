program topogtools
  use, intrinsic :: iso_fortran_env
  use M_CLI2
  use gen_topo_m
  use topography
  implicit none

  character(len=5), PARAMETER :: VERSION = "1.0.0"

  character(len=:), allocatable :: name
  character(len=:), allocatable :: help_general(:), help_gen_topo(:), help_deseas(:), help_min_max_depth(:), help_cutoff(:)
  character(len=:), allocatable :: help_fill_fraction(:), help_fix_nonadvective(:), help_check_nonadvective(:), help_mask(:)
  character(len=80) :: version_text(1)
  character(len=:), allocatable :: file_in, file_out, hgrid, vgrid
  type(topography_t) :: topog
  real(real32) :: sea_area_fraction
  real(real64) :: cutoff
  integer :: ii

  version_text(1) = 'topogtools version '//VERSION

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
    '  fill_fraction - Fill cells where sea area fraction is smaller than some value ', &
    '  check_nonadvective - Check for non-advective cells                            ', &
    '  fix_nonadvective - Fix non-advective cells                                    ', &
    '  mask - Generate mask                                                          ', &
    '  cut_off_T_cells - Cut off T cells below a certain cell size                   ', &
    '']
  help_gen_topo = [character(len=80) :: &
    'usage: topogtools gen_topo --input <input_file> --output <output_file>          ', &
    '                           --hgrid <grid> [<options>]                           ', &
    '                                                                                ', &
    'Generate a new topography from <input_file> on the tracer points of <grid> and  ', &
    'writes the result to <output_file>. Note that <grid> must be a super-grid.      ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --tripolar                  set this option if the horizontal grid is a     ', &
    '                                tripolar grid                                   ', &
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
    '                                --level <level>                                 ', &
    '                                [--vgrid <vgrid> --vgrid_type <type>]           ', &
    '                                                                                ', &
    'Set minimum depth to the depth at a specified level and set maximum depth to    ', &
    'deepest in <vgrid>. <level> is the minimum number of depth levels (e.g. 4).     ', &
    'Can produce non-advective cells.                                                ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --vgrid <vgrid>      vertical grid (default ''ocean_vgrid.nc'')               ', &
    '    --vgrid_type <type>  can be ''mom5'' or ''mom6'' (default ''mom5'')               ', &
    '']
  help_fill_fraction = [character(len=80) :: &
    'usage: topogtools fill_fraction --input <input_file> --output <output_file>     ', &
    '                                --fraction <frac>                               ', &
    '                                                                                ', &
    'Cells with a fraction of sea area smaller than <frac> will have their depth set ', &
    'to zero. Can produce non-advective cells and/or new seas.                       ', &
    '']
  help_fix_nonadvective = [character(len=80) :: &
    'usage: topogtools fix_nonadvective --input <input_file> --output <output_file>  ', &
    '                                   [--vgrid <vgrid> --vgrid_type <type>         ', &
    '                                    --potholes --coastal_cells]                 ', &
    '                                                                                ', &
    'Fix non-advective cells. There are two types of fixes available: potholes and   ', &
    'non-advective coastal cells. Fixes to non-advective coastal cells should only be', &
    'needed when using a B-grid.                                                     ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --vgrid <vgrid>  vertical grid (default ''ocean_vgrid.nc'')                   ', &
    '    --vgrid_type <type>  can be ''mom5'' or ''mom6'' (default ''mom5'')               ', &
    '    --potholes       fix potholes                                               ', &
    '    --coastal-cells  fix non-advective coastal cells                            ', &
    '']
  help_check_nonadvective = [character(len=80) :: &
    'usage: topogtools check_nonadvective --input <input_file>                       ', &
    '                                   [--vgrid <vgrid> --vgrid_type <type>         ', &
    '                                    --potholes --coastal_cells]                 ', &
    '                                                                                ', &
    'Check for non-advective cells. There are two types of checks available: potholes', &
    'and non-advective coastal cells. Checking for non-advective coastal cells should', &
    'only be needed when using a B-grid.                                             ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '    --vgrid <vgrid>  vertical grid (default ''ocean_vgrid.nc'')                   ', &
    '    --vgrid_type <type>  can be ''mom5'' or ''mom6'' (default ''mom5'')               ', &
    '    --potholes       check for potholes                                         ', &
    '    --coastal-cells  check for non-advective coastal cells                      ', &
    '']
  help_mask = [character(len=80) :: &
    'usage: topogtools mask  --input <input_file> --output <output_file>             ', &
    '                                                                                ', &
    'Creates a land mask from a topography.                                          ', &
    '']

  help_cutoff = [character(len=80) :: &
    'usage: topogtools cut_off_T_cells --input <input_file> --output <output_file>   ', &
    '                                  --hgrid <grid> --cutoff <cutoff_value>        ', &
    '                                                                                ', &
    'Convert ocean cells into land if their y size is less than <cutoff_value> in    ', &
    'kilometres. Writes the result to <output_file>.                                 ', &
    '                                                                                ', &
    'Options                                                                         ', &
    '   --hgrid <hgrid_file>     horizontal supergrid (default ''ocean_hgrid.nc'')   ', &
    '']

  ! Read command line
  name = get_subcommand()
  select case (name)
  case ('gen_topo')
    call set_args('--input:i "unset" --output:o "unset" --hgrid "ocean_hgrid.nc" --tripolar F --longitude-offset 0.0', &
      help_gen_topo, version_text)
  case ('deseas')
    call set_args('--input:i "unset" --output:o "unset"', help_deseas, version_text)
  case ('min_max_depth')
    call set_args('--input:i "unset" --output:o "unset" --vgrid "ocean_vgrid.nc" --vgrid_type "mom5" --level 0', &
      help_min_max_depth, version_text)
  case('fill_fraction')
    call set_args('--input:i "unset" --output:o "unset" --fraction 0.0', help_fill_fraction, version_text)
  case ('fix_nonadvective')
    call set_args('--input:i "unset" --output:o "unset" --vgrid "ocean_vgrid.nc" --vgrid_type "mom5" --potholes F &
      &--coastal-cells F', help_fix_nonadvective, version_text)
  case ('check_nonadvective')
    call set_args('--input:i "unset" --vgrid "ocean_vgrid.nc" --vgrid_type "mom5" --potholes F --coastal-cells F', &
      help_check_nonadvective, version_text)
  case ('mask')
    call set_args('--input:i "unset" --output:o "unset"', help_mask, version_text)
  case ('cut_off_T_cells')
    call set_args('--input:i "unset" --output:o "unset" --hgrid "ocean_hgrid.nc" --cutoff 0.0', help_cutoff, version_text)
  case ('')
    ! Print help in case the user specified the --help flag
    call set_args(' ', help_general, version_text)
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
  case ('gen_topo', 'deseas', 'min_max_depth', 'fill_fraction', 'fix_nonadvective', 'mask')
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
    call topog%min_max_depth(vgrid, sget('vgrid_type'), iget('level'))
    call topog%update_history(get_mycommand())
    call topog%write(file_out)

  case ('fill_fraction')
    sea_area_fraction = rget('fraction')
    if (sea_area_fraction <= 0.0 .or. sea_area_fraction >= 1.0) then
      write(error_unit,'(a)') "ERROR: sea area fraction must be larger than 0 and smaller than 1"
      error stop
    end if
    topog = topography_t(file_in)
    call topog%fill_fraction(sea_area_fraction)
    call topog%update_history(get_mycommand())
    call topog%write(file_out)

  case ('fix_nonadvective')
    topog = topography_t(file_in)
    call topog%nonadvective(vgrid, sget('vgrid_type'), lget('potholes'), lget('coastal-cells'), fix=.true.)
    call topog%update_history(get_mycommand())
    call topog%write(file_out)

  case ('check_nonadvective')
    topog = topography_t(file_in)
    call topog%nonadvective(vgrid, sget('vgrid_type'), lget('potholes'), lget('coastal-cells'), fix=.false.)

  case ('mask')
    topog = topography_t(file_in)
    call topog%mask(file_out)

  case ('cut_off_T_cells')
    hgrid = sget('hgrid')
    call check_file_exist(hgrid)
    cutoff = rget('cutoff')
    if (cutoff <= 0.0) then
      write(error_unit,'(a)') "ERROR: cutoff value must be larger than 0 "
      error stop
    end if
    file_out = sget('output')
    if (file_out == 'unset') then
      write(error_unit,'(a)') 'ERROR: no output file specified'
      error stop
    end if  
    topog = topography_t(file_in)
    call topog%cut_off_T_cells(hgrid, cutoff)
    call topog%update_history(get_mycommand())
    call topog%write(file_out)

  end select

end program topogtools
