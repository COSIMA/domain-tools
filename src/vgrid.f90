module vgrid
  use iso_fortran_env
  use netcdf
  use utils
  implicit none

  private
  public :: vgrid_t

  type vgrid_t
    ! Vertical grid variable
    integer :: nlevels = 0
    real(real64), allocatable :: zeta(:)
    character(len=:), allocatable :: author
    ! Global attributes
    character(len=:), allocatable :: original_file
    character(len=:), allocatable :: history
  contains
    procedure :: copy => vgrid_copy
    generic   :: assignment(=) => copy
    procedure :: write => vgrid_write
    procedure :: update_history => vgrid_update_history
    procedure :: float => vgrid_float
  end type vgrid_t

  interface vgrid_t
    module procedure vgrid_constructor
  end interface vgrid_t

contains

  !-------------------------------------------------------------------------
  type(vgrid_t) function vgrid_constructor(filename) result(vgrid)
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, zeta_id, did(1), author_len, history_len ! NetCDF ids

    write(output_unit,'(3a)') "Reading vgrid from file '", trim(filename), "'"

    vgrid%original_file = filename

    ! Open file
    call handle_error(nf90_open(trim(filename), nf90_nowrite, ncid))

    ! Get dimension
    call handle_error(nf90_inq_dimid(ncid, 'nzv', did(1)))
    call handle_error(nf90_inquire_dimension(ncid, did(1), len=vgrid%nlevels))

    ! Get zeta
    allocate(vgrid%zeta(vgrid%nlevels))
    call handle_error(nf90_inq_varid(ncid, 'zeta', zeta_id))
    call handle_error(nf90_get_var(ncid, zeta_id, vgrid%zeta))
    if (nf90_inquire_attribute(ncid, zeta_id, 'author', len=author_len) == nf90_noerr) then
      allocate(character(len=author_len) :: vgrid%author)
      call handle_error(nf90_get_att(ncid, zeta_id, 'author', vgrid%author))
    end if

    ! History (might not be present)
    if (nf90_inquire_attribute(ncid, nf90_global, 'history', len=history_len) == nf90_noerr) then
      allocate(character(len=history_len) :: vgrid%history)
      call handle_error(nf90_get_att(ncid, nf90_global, 'history', vgrid%history))
    end if

    ! Close file
    call handle_error(nf90_close(ncid))

  end function vgrid_constructor

  !-------------------------------------------------------------------------
  subroutine vgrid_copy(vgrid_out, vgrid_in)
    class(vgrid_t), intent(out) :: vgrid_out
    class(vgrid_t), intent(in)  :: vgrid_in

    ! Dimension
    vgrid_out%nlevels = vgrid_in%nlevels

    ! Zeta variable and attributes
    allocate(vgrid_out%zeta, source=vgrid_in%zeta)
    if (allocated(vgrid_in%author)) then
      vgrid_out%author = vgrid_in%author
    end if

    ! Global attributes
    vgrid_out%original_file = vgrid_in%original_file
    if (allocated(vgrid_in%history)) then
      vgrid_out%history = vgrid_in%history
    end if

  end subroutine vgrid_copy

  !-------------------------------------------------------------------------
  subroutine vgrid_write(this, filename)
    class(vgrid_t), intent(in) :: this
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, zeta_id, did(1) ! NetCDF ids

    write(output_unit,'(3a)') "Writing vgrid to file '", trim(filename), "'"

    ! Open file
    call handle_error(nf90_create(trim(filename), ior(nf90_netcdf4, nf90_clobber), ncid))

    ! Write dimension
    call handle_error(nf90_def_dim(ncid, 'nzv', this%nlevels, did(1)))

    ! Write zeta
    call handle_error(nf90_def_var(ncid, 'zeta', nf90_double, did, zeta_id))
    call handle_error(nf90_put_att(ncid, zeta_id, 'units', 'meters'))
    call handle_error(nf90_put_att(ncid, zeta_id, 'standard_name', 'vertical_grid_vertex'))
    call handle_error(nf90_put_att(ncid, zeta_id, 'long_name', 'vgrid'))
    call handle_error(nf90_put_att(ncid, zeta_id, 'author', trim(this%author)))
    call handle_error(nf90_put_var(ncid, zeta_id, this%zeta))

    ! Write global attributes
    call handle_error(nf90_put_att(ncid, nf90_global, 'original_file', trim(this%original_file)))
    call handle_error(nf90_put_att(ncid, nf90_global, 'history', trim(this%history)))

    ! Close file
    call handle_error(nf90_enddef(ncid))
    call handle_error(nf90_close(ncid))

  end subroutine vgrid_write

  !-------------------------------------------------------------------------
  subroutine vgrid_update_history(this, command)
    use iso_c_binding
    class(vgrid_t), intent(inout) :: this
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

  end subroutine vgrid_update_history

  !-------------------------------------------------------------------------
  subroutine vgrid_float(this)
    class(vgrid_t), intent(inout)  :: this

    real(real32), allocatable :: zeta_float(:)

    allocate(zeta_float(this%nlevels))
    zeta_float = this%zeta
    this%zeta = zeta_float

  end subroutine vgrid_float


end module vgrid
