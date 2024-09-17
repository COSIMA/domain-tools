module vgrid
  use iso_fortran_env
  use netcdf
  use utils
  implicit none

  private
  public :: vgrid_t

  type vgrid_t
    ! Vertical grid variable
    character(len=:), allocatable :: type
    integer :: nlevels = 0
    real(real64), allocatable :: zeta(:)
    real(real64), allocatable :: zeta_super(:)
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
  type(vgrid_t) function vgrid_constructor(filename, type) result(vgrid)
    character(len=*), intent(in) :: filename, type

    integer(int32) :: ncid, zeta_id, did(1), zeta_len, author_len, history_len ! NetCDF ids and dims
    real(real64), allocatable :: zeta_var(:)

    write(output_unit,'(3a)') "Reading vgrid from file '", trim(filename), "'"

    vgrid%original_file = filename
    vgrid%type = type
    select case (type)
    case ("mom5", "mom6")
    case default
      write(error_unit,'(3a)') "ERROR: '", type, "' is not a valid vertical grid type."
      error stop
    end select

    ! Open file
    call handle_error(nf90_open(trim(filename), nf90_nowrite, ncid))

    ! Get dimension
    call handle_error(nf90_inq_dimid(ncid, 'nzv', did(1)))
    call handle_error(nf90_inquire_dimension(ncid, did(1), len=zeta_len))

    ! Get zeta
    allocate(zeta_var(zeta_len))
    call handle_error(nf90_inq_varid(ncid, 'zeta', zeta_id))
    call handle_error(nf90_get_var(ncid, zeta_id, zeta_var))
    if (nf90_inquire_attribute(ncid, zeta_id, 'author', len=author_len) == nf90_noerr) then
      allocate(character(len=author_len) :: vgrid%author)
      call handle_error(nf90_get_att(ncid, zeta_id, 'author', vgrid%author))
    end if

    ! History (might not be present)
    if (nf90_inquire_attribute(ncid, nf90_global, 'history', len=history_len) == nf90_noerr) then
      allocate(character(len=history_len) :: vgrid%history)
      call handle_error(nf90_get_att(ncid, nf90_global, 'history', vgrid%history))
    end if

    ! Handle the different types of grids
    select case (type)
    case ("mom5")
      if (mod(zeta_len, 2) == 0) then
        write(error_unit,'(a)') "ERROR: MOM5 vertical grid has an even number of points, which should never happen."
        error stop
      end if
      vgrid%nlevels = zeta_len/2
      allocate(vgrid%zeta_super(zeta_len))
      allocate(vgrid%zeta(0:vgrid%nlevels))
      vgrid%zeta_super = zeta_var
      vgrid%zeta = zeta_var(1:zeta_len:2)

    case ("mom6")
      vgrid%nlevels = zeta_len - 1
      allocate(vgrid%zeta(0:vgrid%nlevels))
      vgrid%zeta = zeta_var
    end select

    ! Close file
    call handle_error(nf90_close(ncid))

  end function vgrid_constructor

  !-------------------------------------------------------------------------
  subroutine vgrid_copy(vgrid_out, vgrid_in)
    class(vgrid_t), intent(out) :: vgrid_out
    class(vgrid_t), intent(in)  :: vgrid_in

    vgrid_out%type = vgrid_in%type

    ! Dimension
    vgrid_out%nlevels = vgrid_in%nlevels

    ! Zeta variable and attributes
    allocate(vgrid_out%zeta, source=vgrid_in%zeta)
    if (allocated(vgrid_in%zeta_super)) then
      allocate(vgrid_out%zeta_super, source=vgrid_in%zeta_super)
    end if
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
    class(vgrid_t), target, intent(in) :: this
    character(len=*), intent(in) :: filename

    integer(int32) :: ncid, zeta_id, zeta_len, did(1) ! NetCDF ids
    real(real64), pointer :: zeta_var(:)

    write(output_unit,'(3a)') "Writing vgrid to file '", trim(filename), "'"

    ! Handle the different types of grids
    select case (this%type)
    case ("mom5")
      zeta_len = 2*this%nlevels + 1
      zeta_var(1:zeta_len) => this%zeta_super(1:zeta_len)

    case ("mom6")
      zeta_len = this%nlevels + 1
      zeta_var(1:zeta_len) => this%zeta(0:zeta_len - 1)
    end select

    ! Open file
    call handle_error(nf90_create(trim(filename), ior(nf90_netcdf4, nf90_clobber), ncid))

    ! Write dimension
    call handle_error(nf90_def_dim(ncid, 'nzv', zeta_len, did(1)))

    ! Write zeta
    call handle_error(nf90_def_var(ncid, 'zeta', nf90_double, did, zeta_id))
    call handle_error(nf90_put_att(ncid, zeta_id, 'units', 'meters'))
    call handle_error(nf90_put_att(ncid, zeta_id, 'standard_name', 'vertical_grid_vertex'))
    call handle_error(nf90_put_att(ncid, zeta_id, 'long_name', 'vgrid'))
    call handle_error(nf90_put_att(ncid, zeta_id, 'author', trim(this%author)))
    call handle_error(nf90_put_var(ncid, zeta_id, zeta_var))

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

    if (allocated(this%zeta_super)) then
      allocate(zeta_float(2*this%nlevels+1))
      zeta_float = real(this%zeta_super, real32)
      this%zeta_super = zeta_float
      deallocate(zeta_float)
    end if

    allocate(zeta_float(0:this%nlevels))
    zeta_float = real(this%zeta, real32)
    this%zeta = zeta_float
    deallocate(zeta_float)

  end subroutine vgrid_float


end module vgrid
