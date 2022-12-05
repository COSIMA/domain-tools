program topogtools
  use, intrinsic :: iso_fortran_env
  use M_CLI2
  use check_nonadvective_m
  use deseas_m
  use fix_nonadvective_m
  use min_max_depth_m
  use topog2mask_m
  implicit none

  character(len=:), allocatable :: name
  character(len=:), allocatable :: help_text(:)
  integer :: ii

  name = get_subcommand()

  select case (name)
  case ('gen_topo')
    call set_args('--input:i "unset" --output:o "unset" --hgrid "unset" --tripolar F --longitude-offset 0.0')

  case ('deseas')
    call set_args('--input:i "unset" --output:o "unset"')
    call deseas(sget('input'), sget('output'))

  case ('min_max_depth')
    call set_args('--input:i "unset" --output:o "unset" --vgrid "ocean_vgrid.nc" --level:l 0')
    call min_max_depth(sget('input'), sget('output'), sget('vgrid'), iget('level'))

  case ('fix_nonadvective')
    call set_args('--input:i "unset" --output:o "unset" --vgrid "ocean_vgrid.nc"')
    call fix_nonadvective(sget('input'), sget('output'), sget('vgrid'))

  case ('check_nonadvective')
    call set_args('--input:i "unset" --vgrid "ocean_vgrid.nc"')
    call check_nonadvective(sget('input'), sget('vgrid'))

  case ('topog2mask')
    call set_args('--input:i "unset" --mask:m "unset" --fraction 0.0')
    call topog2mask(sget('input'), sget('mask'), rget('fraction'))

  case ('')
    ! general help for "topogtools"
    help_text=[character(len=80) :: &
      "usage: topogtools [--help] <command> [<args>]", &
      "", &
      "Collection of tools to edit and manipulate ocean model topographies.", &
      "See 'topogtools --help <command>' to read about a specific subcommand.", &
      "", &
      "Available coomands:", &
      "  gen_topo - Generate a new topography file from a bathymetry", &
      "  deseas - Remove enclosed seas", &
      "  min_max_depth - Set minimum and maximum depth", &
      "  check_nonadvective - Check for cells that are nonadvective", &
      "  fix_nonadvective - Fix cells that are non-advective", &
      "  topog2mask - Generate mask", &
      "" ]

    ! Print help in case the user specified the --help flag
    call set_args(' ', help_text)

    ! Also print even if the user did not specify --help
    if (.not. lget('help')) then
      write(output_unit,'(g0)') (trim(help_text(ii)), ii=1, size(help_text))
      stop
    end if

  case default
    write(*,'(3a)') "topogtools: '", trim(name), "' is not a topogtools subcommand. See 'topogtools --help'."
    stop
  end select

end program topogtools
