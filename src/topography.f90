module topography
  use iso_fortran_env
  use netcdf
  use utils
  use vgrid
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
    ! Coordinates
    real(real64), allocatable :: geolon_t(:,:), geolat_t(:,:)
    ! Global attributes
    character(len=:), allocatable :: original_file
    character(len=:), allocatable :: history
  contains
    procedure :: write => topography_write
    procedure :: write_coordinates => topography_write_coordinates
    procedure :: copy => topography_copy
    generic   :: assignment(=) => copy
    procedure :: update_history => topography_update_history
    procedure :: number_seas => topography_number_seas
    procedure :: deseas => topography_deseas
    procedure :: fill_fraction => topography_fill_fraction
    procedure :: nonadvective => topography_nonadvective
    procedure :: min_max_depth => topography_min_max_depth
    procedure :: mask => topography_mask
    procedure :: min_dy => topography_min_dy
  end type topography_t

  interface topography_t
    module procedure topography_constructor
  end interface topography_t

contains

  !-------------------------------------------------------------------------
  type(topography_t) function topography_constructor(filename) result(topog)
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, depth_id, frac_id, geolon_id, geolat_id, dids(2), history_len ! NetCDF ids

    write(output_unit,'(3a)') "Reading topography from file '", trim(filename), "'"

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

    ! Get coordinates
    call handle_error(nf90_inq_varid(ncid, 'geolon_t', geolon_id))
    allocate(topog%geolon_t(topog%nxt, topog%nyt))
    call handle_error(nf90_get_var(ncid, geolon_id, topog%geolon_t))
    call handle_error(nf90_inq_varid(ncid, 'geolat_t', geolat_id))
    allocate(topog%geolat_t(topog%nxt, topog%nyt))
    call handle_error(nf90_get_var(ncid, geolat_id, topog%geolat_t))

    ! History (might not be present)
    if (nf90_inquire_attribute(ncid, nf90_global, 'history', len=history_len) == nf90_noerr) then
      allocate(character(len=history_len) :: topog%history)
      call handle_error(nf90_get_att(ncid, nf90_global, 'history', topog%history))
    end if

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

    ! Coordinates
    allocate(topog_out%geolon_t, source=topog_in%geolon_t)
    allocate(topog_out%geolat_t, source=topog_in%geolat_t)

    ! Global attributes
    topog_out%original_file = topog_in%original_file
    if (allocated(topog_in%history)) then
      topog_out%history = topog_in%history
    end if

  end subroutine topography_copy

  !-------------------------------------------------------------------------
  subroutine topography_write(this, filename)
    class(topography_t), intent(in) :: this
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, depth_id, frac_id, dids(2) ! NetCDF ids

    write(output_unit,'(3a)') "Writing topography to file '", trim(filename), "'"

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

    ! Write coordinates
    call this%write_coordinates(ncid, dids)

    ! Write global attributes
    call handle_error(nf90_put_att(ncid, nf90_global, 'original_file', trim(this%original_file)))
    call handle_error(nf90_put_att(ncid, nf90_global, 'history', trim(this%history)))

    ! Close file
    call handle_error(nf90_enddef(ncid))
    call handle_error(nf90_close(ncid))

  end subroutine topography_write

  !-------------------------------------------------------------------------
  subroutine topography_write_coordinates(this, ncid, dids)
    class(topography_t), intent(in) :: this
    integer(int32), intent(in) :: ncid, dids(2)

    integer(int32) :: geolon_id, geolat_id ! NetCDF ids

    ! Write coordinates
    call handle_error(nf90_def_var(ncid, 'geolon_t', nf90_float, dids, geolon_id, chunksizes=[this%nxt/10, this%nyt/10], &
      deflate_level=1, shuffle=.true.))
    call handle_error(nf90_put_att(ncid, geolon_id, 'long_name', 'tracer longitude'))
    call handle_error(nf90_put_att(ncid, geolon_id, 'units', 'degrees_E'))
    call handle_error(nf90_put_var(ncid, geolon_id, this%geolon_t))

    call handle_error(nf90_def_var(ncid, 'geolat_t', nf90_float, dids, geolat_id, chunksizes=[this%nxt/10, this%nyt/10], &
      deflate_level=1, shuffle=.true.))
    call handle_error(nf90_put_att(ncid, geolat_id, 'long_name', 'tracer latitude'))
    call handle_error(nf90_put_att(ncid, geolat_id, 'units', 'degrees_N'))
    call handle_error(nf90_put_var(ncid, geolat_id, this%geolat_t))

  end subroutine topography_write_coordinates

  !-------------------------------------------------------------------------
  subroutine topography_update_history(this, command)
    use iso_c_binding
    class(topography_t), intent(inout) :: this
    character(len=*), intent(in) :: command

    character(len=:), allocatable :: new_history, old_history

    new_history = date_time() // ": " // trim(command)
    if (allocated(this%history)) then
      old_history = this%history
      deallocate(this%history)
      this%history = trim(new_history) // C_NEW_LINE // old_history
    else
      this%history = trim(new_history)
    end if

  end subroutine topography_update_history

  !-------------------------------------------------------------------------
  subroutine topography_number_seas(this, sea_number, number_of_seas, silent)
    class(topography_t), intent(in) :: this
    integer(int16), intent(out), target, optional :: sea_number(:,:)
    integer(int32), intent(out), optional :: number_of_seas
    logical, intent(in), optional :: silent

    integer(int32) :: i, j, counter, its, sea_num
    integer(int32) :: im, ip, jm, jp, land

    integer(int16) :: new_sea
    integer(int16), pointer :: sea(:,:)

    logical :: silent_, choke_west, choke_east, choke_north, choke_south

    integer(int16), parameter :: MAX_ITER = 150

    if (present(sea_number)) then
      sea => sea_number
    else
      allocate(sea(this%nxt, this%nyt))
    end if
    if (present(silent)) then
      silent_ = silent
    else
      silent_ = .false.
    end if

    ! Do
    land = this%nxt + this%nyt + 1
    sea = land
    do j = 1, this%nyt
      do i = 1, this%nxt
        if (this%depth(i, j) > 0.0) sea(i, j) = i + j
      end do
      if (all(this%depth(:, j) > 0.0)) sea(:, j) = 0  ! Southern Ocean all water across
    end do

    if (.not. silent_) write(output_unit,'(a)') "       # Iter   # Changes      # Seas"

    do its = 1, MAX_ITER   ! Only need high number after massive editing session with fjords. Normally 10 or so sweeps works.
      counter = 0
      sea_num = 0

      ! Get number of seas
      do j = 2, this%nyt - 1
        i = 1
        if (sea(i, j) < land  .and. sea(i, j) > 0) then
          if (sea(i, j) > sea_num) then
            sea_num = max(min(sea_num+1, sea(this%nxt, j), sea(i, j-1), sea(i+1, j), sea(i, j+1)), sea_num)
            sea(i, j) = sea_num
          end if
        end if

        do i = 2, this%nxt - 1
          if (sea(i, j) < land  .and. sea(i, j) > 0) then
            if (sea(i, j) > sea_num) then
              sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(i+1, j), sea(i, j+1)), sea_num)
              sea(i, j) = sea_num
            end if
          end if
        end do

        i = this%nxt
        if (sea(i, j) < land  .and. sea(i, j) > 0) then
          if (sea(i, j) > sea_num) then
            sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(1, j), sea(i, j+1)), sea_num)
            sea(i, j) = sea_num
          end if
        end if
      end do

      j = this%nyt
      do i = 2, this%nxt - 1
        if (sea(i, j) < land  .and. sea(i, j) > 0) then
          if (sea(i,j) > sea_num) then
            sea_num = max(min(sea_num+1, sea(i-1, j), sea(i, j-1), sea(i+1, j)), sea_num)
            sea(i,j) = sea_num
          end if
        end if
      end do

      ! Diffuse lowest values surrounding a point.

      ! Forward sweep.
      do j = 2, this%nyt
        jm = j - 1
        jp = min(j+1, this%nyt)
        i = 1
        im = this%nxt
        ip = 2
        if (sea(i, j) < land .and. sea(i, j) > 0) then
          sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
          counter = counter + 1
        end if
        do i = 2, this%nxt - 1
          im = i - 1
          ip = i + 1
          if (sea(i, j) < land .and. sea(i, j) > 0) then
            !get chokes
            choke_east = .not. (any(sea(i:ip, jp) == land) .and. any(sea(i:ip, jm) == land))
            choke_west = .not. (any(sea(im:i, jp) == land) .and. any(sea(im:i, jm) == land))
            choke_south = .not. (any(sea(im, jm:j) == land) .and. any(sea(ip, jm:j) == land))
            choke_north = .not. (any(sea(im, j:jp) == land) .and. any(sea(ip, j:jp) == land))
            new_sea = min(minval([sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp)], &
              mask=[choke_west, choke_east, choke_south, choke_north]), land)
            if (sea(i, j) /= new_sea) then
              sea(i, j) = new_sea
              counter = counter + 1
            end if
          end if
        end do
        i = this%nxt
        ip = 1
        im = i - 1
        if (sea(i, j) < land .and. sea(i, j) > 0) then
          sea(i,j)=min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
          counter = counter + 1
        end if
      end do

      ! Backward sweep
      do j = this%nyt, 2, -1
        jm = j - 1
        jp = min(j+1, this%nyt)
        i = 1
        im = this%nxt
        ip = 2
        if (sea(i, j) < land .and. sea(i, j) > 0) then
          sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
          counter = counter + 1
        end if
        do i = this%nxt - 1, 2, -1
          im = i - 1
          ip = i + 1
          if (sea(i, j) < land .and. sea(i, j) > 0) then
            !get chokes
            choke_east = .not. (any(sea(i:ip, jp) == land) .and. any(sea(i:ip, jm) == land))
            choke_west = .not. (any(sea(im:i, jp) == land) .and. any(sea(im:i, jm) == land))
            choke_south = .not. (any(sea(im, jm:j) == land) .and. any(sea(ip, jm:j) == land))
            choke_north = .not. (any(sea(im, j:jp) == land) .and. any(sea(ip, j:jp) == land))
            new_sea = min(minval([sea(im, j), sea(ip, j), sea(i, jm), sea(i,jp)], &
              mask=[choke_west, choke_east, choke_south, choke_north]), land)
            if (sea(i, j) /= new_sea) then
              sea(i, j) = new_sea
              counter = counter + 1
            end if
          end if
        end do
        i = this%nxt
        ip = 1
        im = i - 1
        if (sea(i, j) < land .and. sea(i, j) > 0) then
          sea(i,j) = min(sea(im, j), sea(ip, j), sea(i, jm), sea(i, jp))
          counter = counter + 1
        end if
      end do

      if (.not. silent_) write(output_unit,*) its, counter, sea_num + 1

      ! If we only have one sea or no changes are made we are finished.
      if (counter == 0 .or. sea_num + 1 == 1) exit
      if (its == MAX_ITER) then
        write(output_unit, '(a)') "WARNING: could not number all the seas. Algorithm reached maximum number of iterations."
      end if
    end do

    if (present(sea_number)) then
      nullify(sea)
    else
      deallocate(sea)
    end if

    if (present(number_of_seas)) then
      number_of_seas = sea_num + 1
    end if

  end subroutine topography_number_seas

  !-------------------------------------------------------------------------
  subroutine topography_deseas(this)
    class(topography_t), intent(inout) :: this

    integer(int32) :: ncid, sea_id, dids(2)  ! NetCDF ids
    integer(int16), allocatable :: sea(:,:)

    allocate(sea(this%nxt, this%nyt))

    write(output_unit,'(a)') "Numbering seas"

    call this%number_seas(sea_number=sea)

    call handle_error(nf90_create(trim('sea_num.nc'), ior(nf90_netcdf4, nf90_clobber), ncid))
    call handle_error(nf90_def_dim(ncid, 'nx', this%nxt, dids(1)))
    call handle_error(nf90_def_dim(ncid, 'ny', this%nyt, dids(2)))
    call handle_error(nf90_def_var(ncid, 'sea_num', nf90_short, dids, sea_id, chunksizes=[this%nxt/10, this%nyt/10], &
      deflate_level=1, shuffle=.true.))
    call handle_error(nf90_enddef(ncid))
    call handle_error(nf90_put_var(ncid, sea_id, sea))
    call handle_error(nf90_close(ncid))

    write(output_unit,'(a)') "Removing seas"
    where(sea > 0)
      this%depth = MISSING_VALUE
      this%frac = MISSING_VALUE
    end where
    this%lakes_removed = "yes"

    deallocate(sea)

  end subroutine topography_deseas

  !-------------------------------------------------------------------------
  subroutine topography_min_max_depth(this, vgrid_file, vgrid_type, level)
    class(topography_t), intent(inout) :: this
    character(len=*), intent(in) :: vgrid_file, vgrid_type
    integer, intent(in) :: level

    integer(int32) :: i,j
    type(vgrid_t) :: vgrid

    this%min_level = level

    vgrid = vgrid_t(vgrid_file, vgrid_type)
    this%min_depth = real(vgrid%zeta(this%min_level), real32)
    this%max_depth = real(vgrid%zeta(vgrid%nlevels), real32)

    write(output_unit,'(a,f7.2,a)') 'Setting minimum depth to ', this%min_depth, ' m'
    write(output_unit,'(a,f7.2,a)') 'Setting maximum depth to ', this%max_depth, ' m'

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

 subroutine topography_min_dy(this, hgrid, cutoff)
    class(topography_t), intent(inout) :: this
    character(len=*), intent(in) :: hgrid
    real(real64), intent(in) :: cutoff

    integer(int32) :: i, j
    integer(int32) :: ncid_hgrid, dy_id          ! NetCDF ids for hgrid and dy
    integer(int32) :: dids_dy(2)                 ! NetCDF ids for dimensions
    integer(int32) :: ny_len, nxp_len, nx_len    ! dimensions for hgrid
    real(real64), allocatable :: dy(:,:)         ! To store dy variable from hgrid

    ! Read hgrid to get dy
    write(output_unit,'(3a)') "Attempting to open file '", trim(hgrid), "'"
    call handle_error(nf90_open(trim(hgrid), nf90_nowrite, ncid_hgrid))
    call handle_error(nf90_inq_varid(ncid_hgrid, 'dy', dy_id))
    call handle_error(nf90_inquire_variable(ncid_hgrid, dy_id, dimids=dids_dy))
    call handle_error(nf90_inquire_dimension(ncid_hgrid, dids_dy(1), len=ny_len))
    call handle_error(nf90_inquire_dimension(ncid_hgrid, dids_dy(2), len=nxp_len))

    ! Allocate memory for dy based on its dimensions
    allocate(dy(nxp_len, ny_len))

    ! Read the dy variable from hgrid
    call handle_error(nf90_get_var(ncid_hgrid, dy_id, dy))
    call handle_error(nf90_close(ncid_hgrid))

    ! Calculate T cell size based on dy
    ! For each point, the T cell size is a sum of dy(2*i-1, 2*j) and dy(2*i, 2*j)    
    ! Apply cutoff to depth based on the provided T-cell cutoff value in meters
    do j = 1, ny_len / 2
      do i = 1, (nxp_len - 1) / 2
          if (dy(2 * i - 1, 2 * j) + dy(2 * i, 2 * j) < cutoff) then  !Input cutoff in meters
            this%depth(i, j) = MISSING_VALUE  ! Set values below cutoff to zero or another value as needed
          end if
        end do
    end do   

end subroutine topography_min_dy

  !-------------------------------------------------------------------------
  subroutine topography_fill_fraction(this, sea_area_fraction)
    class(topography_t), intent(inout) :: this
    real(real32), intent(in) :: sea_area_fraction

    integer(int32) :: nseas

    write(output_unit,'(a,f7.2)') "Filling cells that have a sea area fraction smaller than ", sea_area_fraction

    if (any(this%frac < sea_area_fraction)) then
      where (this%frac < sea_area_fraction)
        this%depth = 0.0
        this%frac = 0.0
      end where

      if (this%lakes_removed == 'yes') then
        ! Check if new seas have been created
        call this%number_seas(number_of_seas = nseas, silent=.true.)
        if (nseas > 1) then
          write(output_unit,'(a)') "WARNING: new seas have been created. To fix, rerun deseas again."
          this%lakes_removed = 'no'
        end if
      end if
    end if

  end subroutine topography_fill_fraction

  !-------------------------------------------------------------------------
  subroutine topography_nonadvective(this, vgrid_file, vgrid_type, potholes, coastal, fix)
    class(topography_t), intent(inout) :: this
    character(len=*), intent(in) :: vgrid_file, vgrid_type
    logical, intent(in) :: potholes, coastal, fix

    real(real32), allocatable :: depth_halo(:,:)
    type(vgrid_t) :: vgrid
    integer(int32), allocatable :: num_levels(:,:)
    real(real32), allocatable :: zw(:)
    integer(int32) :: passes, i, j, k, its, coastal_counter, potholes_counter
    logical :: se, sw, ne, nw   ! .TRUE. if C-cell centre is shallower than T cell centre.
    logical :: changes_made = .false.
    integer(int32) :: kse, ksw, kne, knw, kmu_max
    integer(int32) :: im, ip, jm, jp
    integer(int32) :: nseas

    vgrid = vgrid_t(vgrid_file, vgrid_type)
    write(output_unit,*) 'Zeta dimensions', 2*vgrid%nlevels + 1, vgrid%nlevels
    allocate(zw(0:vgrid%nlevels))
    zw = real(vgrid%zeta)

    write(output_unit,*) 'depth dimensions', this%nxt, this%nyt
    allocate(depth_halo(0:this%nxt+1, this%nyt+1))
    allocate(num_levels(0:this%nxt+1, this%nyt+1))

    if (fix) then
      passes = 20
      write(output_unit,'(a)') "Fixing non-advective cells"
    else
      write(output_unit,'(a)') "Checking for non-advective cells"
      passes = 1
    end if
    do its = 1, passes
      if (fix) write(output_unit,'(a,i0)') " Pass # ", its
      coastal_counter = 0
      potholes_counter = 0
      num_levels = 0

      depth_halo = 0
      depth_halo(1:this%nxt, 1:this%nyt) = this%depth
      depth_halo(0, 1:this%nyt) = this%depth(this%nxt, :)
      depth_halo(this%nxt+1, 1:this%nyt)=this%depth(1, :)
      depth_halo(1:this%nxt, this%nyt+1) = this%depth(this%nxt:1:-1, this%nyt)
      do j = 1, this%nyt + 1
        do i = 0, this%nxt + 1
          if (depth_halo(i, j) > 0.0) then
            kloop: do k = 2, vgrid%nlevels
              if (zw(k) >= depth_halo(i, j)) then
                num_levels(i, j) = k
                exit kloop
              end if
            end do kloop
          end if
        end do
      end do

      if (coastal) then
        if (.not. fix) then
          write(output_unit,'(a)') "                 Coastal cells"
          write(output_unit,'(a)') "          i           j       Depth"
        end if
        do j = 2, this%nyt - 1
          do i = 1, this%nxt
            if (depth_halo(i, j) > 0.5) then
              sw = depth_halo(i-1, j) < 0.5 .or. depth_halo(i-1, j-1) < 0.5 .or. depth_halo(i, j-1) < 0.5
              se = depth_halo(i, j-1) < 0.5 .or. depth_halo(i+1, j-1) < 0.5 .or. depth_halo(i+1, j) < 0.5
              ne = depth_halo(i+1, j) < 0.5 .or. depth_halo(i, j+1) < 0.5 .or. depth_halo(i+1, j+1) < 0.5
              nw = depth_halo(i-1, j) < 0.5 .or. depth_halo(i-1, j+1) < 0.5 .or. depth_halo(i, j+1) < 0.5
              if (all([se, sw, ne, nw])) then
                if (fix) then
                  depth_halo(i, j) = 0.0
                  this%frac(i, j) = 0.0
                  num_levels(i, j) = 0
                else
                  write(output_unit,*) i, j, 0.0 ,'  ! nonadvective'
                end if
                coastal_counter = coastal_counter + 1
              end if
            end if
          end do
        end do

        write(output_unit,'(a,i0,a)') '  Found ', coastal_counter, ' non-advective coastal cells'
      end if

      if (potholes) then
        if (.not. fix) then
          write(output_unit,'(a)') "                        Potholes"
          write(output_unit,'(a)') "          i           j         Level    Max. halo level"
        end if
        do j = 2, this%nyt
          jm = j - 1
          jp = j + 1
          do i = 1, this%nxt
            im = i - 1
            ip = i + 1
            if (num_levels(i, j) > 0) then
              ksw = minval([num_levels(im, jm), num_levels(i, jm), num_levels(im, j)])
              kse = minval([num_levels(i, jm), num_levels(ip, jm), num_levels(ip, j)])
              knw = minval([num_levels(im, j), num_levels(im,jp), num_levels(i, jp)])
              kne = minval([num_levels(ip, j), num_levels(i,jp), num_levels(ip, jp)])

              kmu_max = maxval([ksw, kse, knw, kne])

              if (num_levels(i, j) > kmu_max) then
                if (fix) then
                  num_levels(i, j) = kmu_max
                  depth_halo(i, j) = zw(kmu_max)
                else
                  write(output_unit,*) i, j, num_levels(i, j), kmu_max
                end if
                potholes_counter = potholes_counter + 1
              end if
            end if
          end do
        end do
        write(output_unit,'(a,i0,a)') '  Found ', potholes_counter, ' non-advective potholes'
      end if
      if ((coastal_counter > 0 .or. potholes_counter > 0) .and. fix) changes_made = .true.
      this%depth = depth_halo(1:this%nxt, 1:this%nyt)
      if (coastal_counter == 0 .and. potholes_counter == 0 .and. fix) exit
    end do

    if (fix .and. (coastal .or. potholes)) then
      this%nonadvective_cells_removed = 'yes'
      if (changes_made .and. this%lakes_removed == 'yes') then
        ! Check if new lakes were created new lakes
        call this%number_seas(number_of_seas = nseas, silent=.true.)
        if (nseas > 1) then
          write(output_unit,'(a)') "WARNING: new seas have been created. To fix, rerun deseas again."
          this%lakes_removed = 'no'
        end if
      end if
    end if

  end subroutine topography_nonadvective

  !-------------------------------------------------------------------------
  subroutine topography_mask(this, filename)
    class(topography_t), intent(in) :: this
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, mask_id, dids(2) ! NetCDF ids
    real(real32), allocatable :: mask(:, :)

    write(output_unit,'(a)') "Calculating mask"

    allocate(mask(this%nxt, this%nyt))

    where (this%depth <= 0.0) ! Land
      mask = 0.0
    elsewhere ! Ocean
      mask = 1.0
    end where

    write(output_unit,'(3a)') "Writing mask to file '", trim(filename), "'"

    ! Open file
    call handle_error(nf90_create(trim(filename), ior(nf90_netcdf4, nf90_clobber), ncid))

    ! Write dimensions
    call handle_error(nf90_def_dim(ncid, 'nx', this%nxt, dids(1)))
    call handle_error(nf90_def_dim(ncid, 'ny', this%nyt, dids(2)))

    ! Write coordinates
    call this%write_coordinates(ncid, dids)

    ! Write mask
    call handle_error(nf90_def_var(ncid, 'mask', nf90_float, dids, mask_id, chunksizes=[this%nxt/10, this%nyt/10], &
      deflate_level=1, shuffle=.true.))
    call handle_error(nf90_put_var(ncid, mask_id, mask))

    call handle_error(nf90_put_att(ncid, nf90_global, 'history', date_time()//": "//get_mycommand()))

    ! Close file
    call handle_error(nf90_enddef(ncid))
    call handle_error(nf90_close(ncid))

    deallocate(mask)

  end subroutine topography_mask

end module topography
