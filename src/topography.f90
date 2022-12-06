module topography
  use iso_fortran_env
  use netcdf
  use utils
  implicit none

  type topography_t
    ! Dimensions
    integer(int32) :: nxt = 0
    integer(int32) :: nyt = 0
    ! Depth variable and attributes
    real(real32), allocatable :: depth(:,:)
    character(len=3) :: lakes_removed = "no "
    real(real32) :: min_depth = -1.0
    integer :: min_level = 0
    real(real32) :: max_depth = -1.0
    character(len=3) :: nonadvective_cells_removed = 'yes'
    ! Sea area fraction variable
    real(real32), allocatable :: frac(:,:)
    ! Global attributes
    character(len=:), allocatable :: original_file
  contains
    procedure :: write => topography_write
    procedure :: copy => topography_copy
    generic   :: assignment(=) => copy
    procedure :: min_max_depth => topography_min_max_depth
    procedure :: mask => topography_mask
  end type topography_t

  interface topography_t
    module procedure topography_constructor
  end interface topography_t

contains

  !-------------------------------------------------------------------------
  type(topography_t) function topography_constructor(filename) result(topog)
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, depth_id, frac_id, dids(2) ! NetCDF ids

    write(*,*) "Reading topography from file '", trim(filename), "'"

    topog%original_file = filename

    ! Open file
    call handle_error(nf90_open(trim(filename), nf90_nowrite, ncid))

    ! Get dimensions
    call handle_error(nf90_inq_dimid(ncid, 'nx', dids(1)))
    call handle_error(nf90_inq_dimid(ncid, 'ny', dids(2)))
    call handle_error(nf90_inquire_dimension(ncid, dids(1), len=topog%nxt))
    call handle_error(nf90_inquire_dimension(ncid, dids(2), len=topog%nyt))

    ! Get depth
    allocate(topog%depth(topog%nxt, topog%nyt))
    call handle_error(nf90_inq_varid(ncid, 'depth', depth_id))
    call handle_error(nf90_get_var(ncid, depth_id, topog%depth))
    call handle_error(nf90_get_att(ncid, depth_id, 'lakes_removed', topog%lakes_removed), isfatal=.false.)
    call handle_error(nf90_get_att(ncid, depth_id, 'minimum_depth', topog%min_depth), isfatal=.false.)
    call handle_error(nf90_get_att(ncid, depth_id, 'minimum_levels', topog%min_level), isfatal=.false.)
    call handle_error(nf90_get_att(ncid, depth_id, 'maximum_depth', topog%max_depth), isfatal=.false.)
    call handle_error(nf90_get_att(ncid, depth_id, 'nonadvective_cells_removed', topog%nonadvective_cells_removed), isfatal=.false.)

    ! Get sea area fraction
    call handle_error(nf90_inq_varid(ncid, 'sea_area_fraction', frac_id))
    allocate(topog%frac(topog%nxt, topog%nyt))
    call handle_error(nf90_get_var(ncid, frac_id, topog%frac))

    ! Close file
    call handle_error(nf90_close(ncid))

  end function topography_constructor

  !-------------------------------------------------------------------------
  subroutine topography_copy(topog_out, topog_in)
    class(topography_t), intent(out) :: topog_out
    class(topography_t), intent(in)  :: topog_in

    ! Dimensions
    topog_out%nxt = topog_in%nxt
    topog_out%nyt = topog_in%nyt

    ! Depth variable and attributes
    allocate(topog_out%depth, source=topog_in%depth)
    topog_out%lakes_removed = topog_in%lakes_removed
    topog_out%min_depth = topog_in%min_depth
    topog_out%min_level = topog_in%min_level
    topog_out%max_depth = topog_in%max_depth
    topog_out%nonadvective_cells_removed = topog_in%nonadvective_cells_removed

    ! Sea area fraction
    allocate(topog_out%frac, source=topog_in%frac)

    ! Global attributes
    topog_out%original_file = topog_in%original_file

  end subroutine topography_copy

  !-------------------------------------------------------------------------
  subroutine topography_write(this, filename)
    class(topography_t), intent(in) :: this
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, depth_id, frac_id, dids(2) ! NetCDF ids

    write(*,*) "Writing topography to file '", trim(filename), "'"

    ! Open file
    call handle_error(nf90_create(trim(filename), ior(nf90_netcdf4, nf90_clobber), ncid))

    ! Write dimensions
    call handle_error(nf90_def_dim(ncid, 'nx', this%nxt, dids(1)))
    call handle_error(nf90_def_dim(ncid, 'ny', this%nyt, dids(2)))

    ! Write depth
    call handle_error(nf90_def_var(ncid, 'depth', nf90_float, dids, depth_id, chunksizes=[this%nxt/10, this%nyt/10], &
      deflate_level=1, shuffle=.true.))
    call handle_error(nf90_def_var_fill(ncid, depth_id, 0, MISSING_VALUE))
    call handle_error(nf90_put_att(ncid, depth_id, 'long_name', 'depth'))
    call handle_error(nf90_put_att(ncid, depth_id, 'units', 'm'))
    call handle_error(nf90_put_att(ncid, depth_id, 'lakes_removed', this%lakes_removed))
    if (this%min_depth > 0.0) then
      call handle_error(nf90_put_att(ncid, depth_id, 'minimum_depth', this%min_depth))
      call handle_error(nf90_put_att(ncid, depth_id, 'minimum_levels', this%min_level))
    end if
    if (this%max_depth > 0.0) then
      call handle_error(nf90_put_att(ncid, depth_id, 'maximum_depth', this%max_depth))
    end if
    call handle_error(nf90_put_att(ncid, depth_id, 'nonadvective_cells_removed', this%nonadvective_cells_removed))
    call handle_error(nf90_put_var(ncid, depth_id, this%depth))

    ! Write frac
    call handle_error(nf90_def_var(ncid, 'sea_area_fraction', nf90_float, dids, frac_id, chunksizes=[this%nxt/10, this%nyt/10], &
      deflate_level=1, shuffle=.true.))
    call handle_error(nf90_def_var_fill(ncid, frac_id, 0, MISSING_VALUE))
    call handle_error(nf90_put_var(ncid, frac_id, this%frac))

    ! Write global attributes
    call handle_error(nf90_put_att(ncid, nf90_global, 'original_file', trim(this%original_file)))

    ! Close file
    call handle_error(nf90_enddef(ncid))

  end subroutine topography_write

  !-------------------------------------------------------------------------
  subroutine topography_min_max_depth(this, vgrid, level)
    class(topography_t), intent(inout) :: this
    character(len=*), intent(in) :: vgrid
    integer, intent(in) :: level

    integer(int32) :: i,j
    integer(int32) :: im,ip,jm,jp

    integer(int32) :: ncid_lev, lev_id           ! NetCDF ids
    integer(int32) :: dids_lev(1)           ! NetCDF ids
    integer(int32) :: zlen                   ! length of zeta array

    real(real64)  ::  zeta
    real(real64), allocatable :: zeta_arr(:)

    this%min_level = level

    call handle_error(nf90_open(trim(vgrid), nf90_nowrite, ncid_lev))
    call handle_error(nf90_inq_varid(ncid_lev, 'zeta', lev_id))
    call handle_error(nf90_get_var(ncid_lev, lev_id, zeta, start=[2*this%min_level+1]))
    this%min_depth = zeta

    call handle_error(nf90_inquire_variable(ncid_lev, lev_id, dimids=dids_lev))
    call handle_error(nf90_inquire_dimension(ncid_lev, dids_lev(1), len=zlen))
    call handle_error(nf90_get_var(ncid_lev, lev_id, zeta, start=[zlen]))
    this%max_depth = zeta

    call handle_error(nf90_close(ncid_lev))

    write(*,*) 'Setting minimum depth to ', this%min_depth
    write(*,*) 'Setting maximum depth to ', this%max_depth

    ! Reset depth
    do j = 1, this%nyt
      do i = 1, this%nxt
        if (this%depth(i, j) > 0.0) then
          this%depth(i, j) = min(max(this%depth(i, j), this%min_depth), this%max_depth)
        else
          this%depth(i, j) = MISSING_VALUE
        end if
      end do
    end do

  end subroutine topography_min_max_depth

  !-------------------------------------------------------------------------
  subroutine topography_mask(this, filename, sea_area_fraction)
    class(topography_t), intent(in) :: this
    character(len=*), intent(in) :: filename
    real(real32), intent(in) :: sea_area_fraction

    integer(int32) :: ncid, mask_id, dids(2) ! NetCDF ids
    real(real32), allocatable :: mask(:, :)

    write(*,*) "Calculating mask"

    allocate(mask(this%nxt, this%nyt))

    where (this%depth <= 0.0 .or. this%frac < sea_area_fraction) ! Land
      mask = 0.0
    elsewhere ! Ocean
      mask = 1.0
    end where

    write(*,*) "Writing mask to file '", trim(filename), "'"

    ! Open file
    call handle_error(nf90_create(trim(filename), ior(nf90_netcdf4, nf90_clobber), ncid))

    ! Write dimensions
    call handle_error(nf90_def_dim(ncid, 'nx', this%nxt, dids(1)))
    call handle_error(nf90_def_dim(ncid, 'ny', this%nyt, dids(2)))

    ! Write mask
    call handle_error(nf90_def_var(ncid, 'mask', nf90_float, dids, mask_id, chunksizes=[this%nxt/10, this%nyt/10], &
      deflate_level=1, shuffle=.true.))
    call handle_error(nf90_put_var(ncid, mask_id, mask))

    call handle_error(nf90_put_att(ncid, nf90_global, 'history', 'Created from topography file '//trim(this%original_file)))

    ! Close file
    call handle_error(nf90_enddef(ncid))

    deallocate(mask)

  end subroutine topography_mask

end module topography
