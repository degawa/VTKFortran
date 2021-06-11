!| VTK file XMl writer, appended.
module vtk_fortran_vtk_file_xml_writer_appended
    use, intrinsic :: iso_fortran_env
    use penf, only: str, BYI1P, BYI2P, BYI4P, BYI8P, BYR4P, BYR8P
    use stringifor
    use vtk_fortran_dataarray_encoder
    use vtk_fortran_parameters
    use vtk_fortran_vtk_file_xml_writer_abstract

    implicit none
    private
    public :: xml_writer_appended

    type, extends(xml_writer_abstract) :: xml_writer_appended
        !! VTK file XML writer, appended.
        type(string) :: encoding
            !! Appended data encoding: "raw" or "base64".
        integer(int32) :: scratch = 0_int32
            !! Scratch logical unit.
    contains
        ! deferred methods
        procedure, pass(self) :: initialize
            !! Initialize writer.
        procedure, pass(self) :: finalize
            !! Finalize writer.
        procedure, pass(self) :: write_dataarray1_rank1_real64
            !! Write dataarray 1, rank 1, real64.
        procedure, pass(self) :: write_dataarray1_rank1_real32
            !! Write dataarray 1, rank 1, real32.
        procedure, pass(self) :: write_dataarray1_rank1_int64
            !! Write dataarray 1, rank 1, int64.
        procedure, pass(self) :: write_dataarray1_rank1_int32
            !! Write dataarray 1, rank 1, int32.
        procedure, pass(self) :: write_dataarray1_rank1_int16
            !! Write dataarray 1, rank 1, int16.
        procedure, pass(self) :: write_dataarray1_rank1_int8
            !! Write dataarray 1, rank 1, int8.
        procedure, pass(self) :: write_dataarray1_rank2_real64
            !! Write dataarray 1, rank 2, real64.
        procedure, pass(self) :: write_dataarray1_rank2_real32
            !! Write dataarray 1, rank 2, real32.
        procedure, pass(self) :: write_dataarray1_rank2_int64
            !! Write dataarray 1, rank 2, int64.
        procedure, pass(self) :: write_dataarray1_rank2_int32
            !! Write dataarray 1, rank 2, int32.
        procedure, pass(self) :: write_dataarray1_rank2_int16
            !! Write dataarray 1, rank 2, int16.
        procedure, pass(self) :: write_dataarray1_rank2_int8
            !! Write dataarray 1, rank 2, int8.
        procedure, pass(self) :: write_dataarray1_rank3_real64
            !! Write dataarray 1, rank 3, real64.
        procedure, pass(self) :: write_dataarray1_rank3_real32
            !! Write dataarray 1, rank 3, real32.
        procedure, pass(self) :: write_dataarray1_rank3_int64
            !! Write dataarray 1, rank 3, int64.
        procedure, pass(self) :: write_dataarray1_rank3_int32
            !! Write dataarray 1, rank 3, int32.
        procedure, pass(self) :: write_dataarray1_rank3_int16
            !! Write dataarray 1, rank 3, int16.
        procedure, pass(self) :: write_dataarray1_rank3_int8
            !! Write dataarray 1, rank 3, int8.
        procedure, pass(self) :: write_dataarray1_rank4_real64
            !! Write dataarray 1, rank 4, real64.
        procedure, pass(self) :: write_dataarray1_rank4_real32
            !! Write dataarray 1, rank 4, real32.
        procedure, pass(self) :: write_dataarray1_rank4_int64
            !! Write dataarray 1, rank 4, int64.
        procedure, pass(self) :: write_dataarray1_rank4_int32
            !! Write dataarray 1, rank 4, int32.
        procedure, pass(self) :: write_dataarray1_rank4_int16
            !! Write dataarray 1, rank 4, int16.
        procedure, pass(self) :: write_dataarray1_rank4_int8
            !! Write dataarray 1, rank 4, int8.
        procedure, pass(self) :: write_dataarray3_rank1_real64
            !! Write dataarray 3, rank 1, real64.
        procedure, pass(self) :: write_dataarray3_rank1_real32
            !! Write dataarray 3, rank 1, real32.
        procedure, pass(self) :: write_dataarray3_rank1_int64
            !! Write dataarray 3, rank 1, int64.
        procedure, pass(self) :: write_dataarray3_rank1_int32
            !! Write dataarray 3, rank 1, int32.
        procedure, pass(self) :: write_dataarray3_rank1_int16
            !! Write dataarray 3, rank 1, int16.
        procedure, pass(self) :: write_dataarray3_rank1_int8
            !! Write dataarray 3, rank 1, int8.
        procedure, pass(self) :: write_dataarray3_rank3_real64
            !! Write dataarray 3, rank 3, real64.
        procedure, pass(self) :: write_dataarray3_rank3_real32
            !! Write dataarray 3, rank 3, real32.
        procedure, pass(self) :: write_dataarray3_rank3_int64
            !! Write dataarray 3, rank 3, int64.
        procedure, pass(self) :: write_dataarray3_rank3_int32
            !! Write dataarray 3, rank 3, int32.
        procedure, pass(self) :: write_dataarray3_rank3_int16
            !! Write dataarray 3, rank 3, int16.
        procedure, pass(self) :: write_dataarray3_rank3_int8
            !! Write dataarray 3, rank 3, int8.
        procedure, pass(self) :: write_dataarray_appended
            !! Write appended.
        ! private methods
        procedure, pass(self), private :: ioffset_update
            !! Update ioffset count.
        procedure, pass(self), private :: open_scratch_file
            !! Open scratch file.
        procedure, pass(self), private :: close_scratch_file
            !! Close scratch file.
        generic, private :: write_on_scratch_dataarray => &
            write_on_scratch_dataarray1_rank1, &
            write_on_scratch_dataarray1_rank2, &
            write_on_scratch_dataarray1_rank3, &
            write_on_scratch_dataarray1_rank4, &
            write_on_scratch_dataarray3_rank1_real64, &
            write_on_scratch_dataarray3_rank1_real32, &
            write_on_scratch_dataarray3_rank1_int64, &
            write_on_scratch_dataarray3_rank1_int32, &
            write_on_scratch_dataarray3_rank1_int16, &
            write_on_scratch_dataarray3_rank1_int8, &
            write_on_scratch_dataarray3_rank2_real64, &
            write_on_scratch_dataarray3_rank2_real32, &
            write_on_scratch_dataarray3_rank2_int64, &
            write_on_scratch_dataarray3_rank2_int32, &
            write_on_scratch_dataarray3_rank2_int16, &
            write_on_scratch_dataarray3_rank2_int8, &
            write_on_scratch_dataarray3_rank3_real64, &
            write_on_scratch_dataarray3_rank3_real32, &
            write_on_scratch_dataarray3_rank3_int64, &
            write_on_scratch_dataarray3_rank3_int32, &
            write_on_scratch_dataarray3_rank3_int16, &
            write_on_scratch_dataarray3_rank3_int8
            !! Write dataarray.
        procedure, pass(self), private :: write_on_scratch_dataarray1_rank1
            !! Write dataarray, data 1 rank 1.
        procedure, pass(self), private :: write_on_scratch_dataarray1_rank2
            !! Write dataarray, data 1 rank 2.
        procedure, pass(self), private :: write_on_scratch_dataarray1_rank3
            !! Write dataarray, data 1 rank 3.
        procedure, pass(self), private :: write_on_scratch_dataarray1_rank4
            !! Write dataarray, data 1 rank 4.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_real64
            !! Write dataarray, comp 3 rank 1, real64.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_real32
            !! Write dataarray, comp 3 rank 1, real32.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_int64
            !! Write dataarray, comp 3 rank 1, int64.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_int32
            !! Write dataarray, comp 3 rank 1, int32.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_int16
            !! Write dataarray, comp 3 rank 1, int16.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank1_int8
            !! Write dataarray, comp 3 rank 1, int8.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_real64
            !! Write dataarray, comp 3 rank 2, real64.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_real32
            !! Write dataarray, comp 3 rank 2, real32.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_int64
            !! Write dataarray, comp 3 rank 2, int64.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_int32
            !! Write dataarray, comp 3 rank 2, int32.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_int16
            !! Write dataarray, comp 3 rank 2, int16.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank2_int8
            !! Write dataarray, comp 3 rank 2, int8.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_real64
            !! Write dataarray, comp 3 rank 3, real64.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_real32
            !! Write dataarray, comp 3 rank 3, real32.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_int64
            !! Write dataarray, comp 3 rank 3, int64.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_int32
            !! Write dataarray, comp 3 rank 3, int32.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_int16
            !! Write dataarray, comp 3 rank 3, int16.
        procedure, pass(self), private :: write_on_scratch_dataarray3_rank3_int8
            !! Write dataarray, comp 3 rank 3, int8.
    end type xml_writer_appended
contains
    !| Initialize writer.
    function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
                        is_volatile, mesh_kind) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: format
            !! File format: ASCII.
        character(*), intent(in) :: filename
            !! File name.
        character(*), intent(in) :: mesh_topology
            !! Mesh topology.
        integer(int32), intent(in), optional :: nx1
            !! Initial node of x axis.
        integer(int32), intent(in), optional :: nx2
            !! Final node of x axis.
        integer(int32), intent(in), optional :: ny1
            !! Initial node of y axis.
        integer(int32), intent(in), optional :: ny2
            !! Final node of y axis.
        integer(int32), intent(in), optional :: nz1
            !! Initial node of z axis.
        integer(int32), intent(in), optional :: nz2
            !! Final node of z axis.
        character(*), intent(in), optional :: mesh_kind
            !! Kind of mesh data: Float64, Float32, ecc.
        logical, intent(in), optional :: is_volatile
            !! Flag to check volatile writer.
        integer(int32) :: error
            !! Error status.

        self%topology = trim(adjustl(mesh_topology))
        self%format_ch = 'appended'
        self%encoding = format
        self%encoding = self%encoding%upper()
        select case (self%encoding%chars())
        case ('RAW')
            self%encoding = 'raw'
        case ('BINARY-APPENDED')
            self%encoding = 'base64'
        end select
        call self%open_xml_file(filename=filename)
        call self%write_header_tag
        call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
        self%ioffset = 0
        call self%open_scratch_file
        error = self%error
    end function initialize

    !| Finalize writer.
    function finalize(self) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int32) :: error
            !! Error status.

        call self%write_end_tag(name=self%topology%chars())
        call self%write_dataarray_appended
        call self%write_end_tag(name='VTKFile')
        call self%close_xml_file
        call self%close_scratch_file
        error = self%error
    end function finalize

    !| Update ioffset count.
    elemental subroutine ioffset_update(self, n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: n_byte
            !! Number of bytes saved.

        if (self%encoding == 'raw') then
            self%ioffset = self%ioffset + BYI4P + n_byte
        else
            self%ioffset = self%ioffset + ((n_byte + BYI4P + 2_int32)/3_int32)*4_int32
        end if
    end subroutine ioffset_update

    !| Open scratch file.
    subroutine open_scratch_file(self)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.

        open (newunit=self%scratch, &
              form='UNFORMATTED', &
              access='STREAM', &
              action='READWRITE', &
              status='SCRATCH', &
              iostat=self%error)
    end subroutine open_scratch_file

    !| Close scratch file.
    subroutine close_scratch_file(self)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.

        close (unit=self%scratch, iostat=self%error)
    end subroutine close_scratch_file

    ! write_dataarray methods
    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank1_real64(self, data_name, x, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float64'
        n_components = 1
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank1_real64

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank1_real32(self, data_name, x, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float32'
        n_components = 1
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank1_real32

    function write_dataarray1_rank1_int64(self, data_name, x, is_tuples) result(error)
        !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int64).
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int64'
        n_components = 1
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank1_int64

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank1_int32(self, data_name, x, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int32'
        n_components = 1
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank1_int32

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank1_int16(self, data_name, x, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int16'
        n_components = 1
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank1_int16

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank1_int8(self, data_name, x, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int8'
        n_components = 1
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank1_int8

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank2_real64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank2_real64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank2_real32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank2_real32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
    function write_dataarray1_rank2_int64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank2_int64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank2_int32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank2_int32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank2_int16(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int16'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank2_int16

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank2_int8(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int8'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank2_int8

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank3_real64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank3_real64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank3_real32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank3_real32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
    function write_dataarray1_rank3_int64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank3_int64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank3_int32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank3_int32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank3_int16(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int16'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank3_int16

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank3_int8(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int8'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank3_int8

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank4_real64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank4_real64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank4_real32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank4_real32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
    function write_dataarray1_rank4_int64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank4_int64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank4_int32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank4_int32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank4_int16(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int16'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank4_int16

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank4_int8(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int8'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
        error = self%error
    end function write_dataarray1_rank4_int8

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real64).
    function write_dataarray3_rank1_real64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:)
            !! X component of data variable.
        real(real64), intent(in) :: y(1:)
            !! Y component of data variable.
        real(real64), intent(in) :: z(1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead "NumberOfComponents" attribute.
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float64'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank1_real64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real32).
    function write_dataarray3_rank1_real32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:)
            !! X component of data variable.
        real(real32), intent(in) :: y(1:)
            !! Y component of data variable.
        real(real32), intent(in) :: z(1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float32'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank1_real32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int64).
    function write_dataarray3_rank1_int64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:)
            !! X component of data variable.
        integer(int64), intent(in) :: y(1:)
            !! Y component of data variable.
        integer(int64), intent(in) :: z(1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int64'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank1_int64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int32).
    function write_dataarray3_rank1_int32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:)
            !! X component of data variable.
        integer(int32), intent(in) :: y(1:)
            !! Y component of data variable.
        integer(int32), intent(in) :: z(1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int32'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank1_int32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int16).
    function write_dataarray3_rank1_int16(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:)
            !! X component of data variable.
        integer(int16), intent(in) :: y(1:)
            !! Y component of data variable.
        integer(int16), intent(in) :: z(1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int16'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank1_int16

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int8).
    function write_dataarray3_rank1_int8(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:)
            !! X component of data variable.
        integer(int8), intent(in) :: y(1:)
            !! Y component of data variable.
        integer(int8), intent(in) :: z(1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int8'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank1_int8

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real64).
    function write_dataarray3_rank3_real64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! X component of data variable.
        real(real64), intent(in) :: y(1:, 1:, 1:)
            !! Y component of data variable.
        real(real64), intent(in) :: z(1:, 1:, 1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float64'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank3_real64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real32).
    function write_dataarray3_rank3_real32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! X component of data variable.
        real(real32), intent(in) :: y(1:, 1:, 1:)
            !! Y component of data variable.
        real(real32), intent(in) :: z(1:, 1:, 1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Float32'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank3_real32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int64).
    function write_dataarray3_rank3_int64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! X component of data variable.
        integer(int64), intent(in) :: y(1:, 1:, 1:)
            !! Y component of data variable.
        integer(int64), intent(in) :: z(1:, 1:, 1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int64'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank3_int64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int32).
    function write_dataarray3_rank3_int32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! X component of data variable.
        integer(int32), intent(in) :: y(1:, 1:, 1:)
            !! Y component of data variable.
        integer(int32), intent(in) :: z(1:, 1:, 1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int32'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank3_int32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int16).
    function write_dataarray3_rank3_int16(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! X component of data variable.
        integer(int16), intent(in) :: y(1:, 1:, 1:)
            !! Y component of data variable.
        integer(int16), intent(in) :: z(1:, 1:, 1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int16'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank3_int16

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int8).
    function write_dataarray3_rank3_int8(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! X component of data variable.
        integer(int8), intent(in) :: y(1:, 1:, 1:)
            !! Y component of data variable.
        integer(int8), intent(in) :: z(1:, 1:, 1:)
            !! Z component of data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.

        data_type = 'Int8'
        n_components = 3
        call self%write_dataarray_tag_appended(data_type=data_type, number_of_components=n_components, data_name=data_name, &
                                               is_tuples=is_tuples)
        call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x, y=y, z=z))
        error = self%error
    end function write_dataarray3_rank3_int8

    !| Do nothing, ascii data cannot be appended.
    subroutine write_dataarray_appended(self)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        type(string) :: tag_attributes
            !! Tag attributes.
        integer(int32) :: n_byte
            !! Bytes count.
        character(len=2) :: dataarray_type
            !! Dataarray type = R8,R4,I8,I4,I2,I1.
        integer(int32) :: dataarray_dim
            !! Dataarray dimension.
        real(real64), allocatable :: dataarray_real64(:)
            !! Dataarray buffer of real64.
        real(real32), allocatable :: dataarray_real32(:)
            !! Dataarray buffer of real32.
        integer(int64), allocatable :: dataarray_int64(:)
            !! Dataarray buffer of int64.
        integer(int32), allocatable :: dataarray_int32(:)
            !! Dataarray buffer of int32.
        integer(int16), allocatable :: dataarray_int16(:)
            !! Dataarray buffer of int16.
        integer(int8), allocatable :: dataarray_int8(:)
            !! Dataarray buffer of int8.

        call self%write_start_tag(name='AppendedData', attributes='encoding="'//self%encoding%chars()//'"')
        write (unit=self%xml, iostat=self%error) '_'
        endfile (unit=self%scratch, iostat=self%error)
        rewind (unit=self%scratch, iostat=self%error)
        do
            call read_dataarray_from_scratch
            if (self%error == 0) call write_dataarray_on_xml
            if (is_iostat_end(self%error)) exit
        end do
        close (unit=self%scratch, iostat=self%error)
        write (unit=self%xml, iostat=self%error) end_rec
        call self%write_end_tag(name='AppendedData')
    contains
        subroutine read_dataarray_from_scratch
            !! Read the current dataaray from scratch file.

            read (unit=self%scratch, iostat=self%error, end=10) n_byte, dataarray_type, dataarray_dim
            select case (dataarray_type)
            case ('R8')
                if (allocated(dataarray_real64)) deallocate (dataarray_real64); allocate (dataarray_real64(1:dataarray_dim))
                read (unit=self%scratch, iostat=self%error) dataarray_real64
            case ('R4')
                if (allocated(dataarray_real32)) deallocate (dataarray_real32); allocate (dataarray_real32(1:dataarray_dim))
                read (unit=self%scratch, iostat=self%error) dataarray_real32
            case ('I8')
                if (allocated(dataarray_int64)) deallocate (dataarray_int64); allocate (dataarray_int64(1:dataarray_dim))
                read (unit=self%scratch, iostat=self%error) dataarray_int64
            case ('I4')
                if (allocated(dataarray_int32)) deallocate (dataarray_int32); allocate (dataarray_int32(1:dataarray_dim))
                read (unit=self%scratch, iostat=self%error) dataarray_int32
            case ('I2')
                if (allocated(dataarray_int16)) deallocate (dataarray_int16); allocate (dataarray_int16(1:dataarray_dim))
                read (unit=self%scratch, iostat=self%error) dataarray_int16
            case ('I1')
                if (allocated(dataarray_int8)) deallocate (dataarray_int8); allocate (dataarray_int8(1:dataarray_dim))
                read (unit=self%scratch, iostat=self%error) dataarray_int8
            case default
                self%error = 1
                write (stderr, '(A)') ' error: bad dataarray_type = '//dataarray_type
                write (stderr, '(A)') ' bytes = '//trim(str(n=n_byte))
                write (stderr, '(A)') ' dataarray dimension = '//trim(str(n=dataarray_dim))
            end select
10          return
        end subroutine read_dataarray_from_scratch

        subroutine write_dataarray_on_xml
            !! Write the current dataaray on xml file.
            character(len=:), allocatable :: code
                !! Dataarray encoded with Base64 codec.

            if (self%encoding == 'raw') then
                select case (dataarray_type)
                case ('R8')
                    write (unit=self%xml, iostat=self%error) n_byte, dataarray_real64
                    deallocate (dataarray_real64)
                case ('R4')
                    write (unit=self%xml, iostat=self%error) n_byte, dataarray_real32
                    deallocate (dataarray_real32)
                case ('I8')
                    write (unit=self%xml, iostat=self%error) n_byte, dataarray_int64
                    deallocate (dataarray_int64)
                case ('I4')
                    write (unit=self%xml, iostat=self%error) n_byte, dataarray_int32
                    deallocate (dataarray_int32)
                case ('I2')
                    write (unit=self%xml, iostat=self%error) n_byte, dataarray_int16
                    deallocate (dataarray_int16)
                case ('I1')
                    write (unit=self%xml, iostat=self%error) n_byte, dataarray_int8
                    deallocate (dataarray_int8)
                end select
            else
                select case (dataarray_type)
                case ('R8')
                    code = encode_binary_dataarray(x=dataarray_real64)
                    write (unit=self%xml, iostat=self%error) code
                case ('R4')
                    code = encode_binary_dataarray(x=dataarray_real32)
                    write (unit=self%xml, iostat=self%error) code
                case ('I8')
                    code = encode_binary_dataarray(x=dataarray_int64)
                    write (unit=self%xml, iostat=self%error) code
                case ('I4')
                    code = encode_binary_dataarray(x=dataarray_int32)
                    write (unit=self%xml, iostat=self%error) code
                case ('I2')
                    code = encode_binary_dataarray(x=dataarray_int16)
                    write (unit=self%xml, iostat=self%error) code
                case ('I1')
                    code = encode_binary_dataarray(x=dataarray_int8)
                    write (unit=self%xml, iostat=self%error) code
                end select
            end if
        end subroutine write_dataarray_on_xml
    end subroutine write_dataarray_appended

    ! write_on_scratch_dataarray methods
    !| Write a dataarray with 1 components of rank 1.
    function write_on_scratch_dataarray1_rank1(self, x) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        class(*), intent(in) :: x(1:)
            !! Data variable.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)
        select type (x)
        type is (real(real64))
            n_byte = nn*BYR8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (real(real32))
            n_byte = nn*BYR4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int64))
            n_byte = nn*BYI8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int32))
            n_byte = nn*BYI4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int16))
            n_byte = nn*BYI2P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I2', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int8))
            n_byte = nn*BYI1P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I1', nn
            write (unit=self%scratch, iostat=self%error) x
        end select
    end function write_on_scratch_dataarray1_rank1

    !| Write a dataarray with 1 components of rank 2.
    function write_on_scratch_dataarray1_rank2(self, x) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        class(*), intent(in) :: x(1:, 1:)
            !! Data variable.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)
        select type (x)
        type is (real(real64))
            n_byte = nn*BYR8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (real(real32))
            n_byte = nn*BYR4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int64))
            n_byte = nn*BYI8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int32))
            n_byte = nn*BYI4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int16))
            n_byte = nn*BYI2P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I2', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int8))
            n_byte = nn*BYI1P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I1', nn
            write (unit=self%scratch, iostat=self%error) x
        end select
    end function write_on_scratch_dataarray1_rank2

    !| Write a dataarray with 1 components of rank 3.
    function write_on_scratch_dataarray1_rank3(self, x) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        class(*), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
        select type (x)
        type is (real(real64))
            n_byte = nn*BYR8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (real(real32))
            n_byte = nn*BYR4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int64))
            n_byte = nn*BYI8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int32))
            n_byte = nn*BYI4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int16))
            n_byte = nn*BYI2P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I2', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int8))
            n_byte = nn*BYI1P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I1', nn
            write (unit=self%scratch, iostat=self%error) x
        end select
    end function write_on_scratch_dataarray1_rank3

    !| Write a dataarray with 1 components of rank 4.
    function write_on_scratch_dataarray1_rank4(self, x) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        class(*), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: nn
            !! Number of elements.

        nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
        select type (x)
        type is (real(real64))
            n_byte = nn*BYR8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (real(real32))
            n_byte = nn*BYR4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'R4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int64))
            n_byte = nn*BYI8P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I8', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int32))
            n_byte = nn*BYI4P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I4', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int16))
            n_byte = nn*BYI2P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I2', nn
            write (unit=self%scratch, iostat=self%error) x
        type is (integer(int8))
            n_byte = nn*BYI1P
            write (unit=self%scratch, iostat=self%error) n_byte, 'I1', nn
            write (unit=self%scratch, iostat=self%error) x
        end select
    end function write_on_scratch_dataarray1_rank4

    !| Write a dataarray with 3 components of rank 1 (real64).
    function write_on_scratch_dataarray3_rank1_real64(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        real(real64), intent(in) :: x(1:)
            !! X component.
        real(real64), intent(in) :: y(1:)
            !! Y component.
        real(real64), intent(in) :: z(1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1, size(x, dim=1))])
    end function write_on_scratch_dataarray3_rank1_real64

    !| Write a dataarray with 3 components of rank 1 (real32).
    function write_on_scratch_dataarray3_rank1_real32(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        real(real32), intent(in) :: x(1:)
            !! X component.
        real(real32), intent(in) :: y(1:)
            !! Y component.
        real(real32), intent(in) :: z(1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1, size(x, dim=1))])
    end function write_on_scratch_dataarray3_rank1_real32

    !| Write a dataarray with 3 components of rank 1 (int64).
    function write_on_scratch_dataarray3_rank1_int64(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int64), intent(in) :: x(1:)
            !! X component.
        integer(int64), intent(in) :: y(1:)
            !! Y component.
        integer(int64), intent(in) :: z(1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1, size(x, dim=1))])
    end function write_on_scratch_dataarray3_rank1_int64

    !| Write a dataarray with 3 components of rank 1 (int32).
    function write_on_scratch_dataarray3_rank1_int32(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: x(1:)
            !! X component.
        integer(int32), intent(in) :: y(1:)
            !! Y component.
        integer(int32), intent(in) :: z(1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1, size(x, dim=1))])
    end function write_on_scratch_dataarray3_rank1_int32

    !| Write a dataarray with 3 components of rank 1 (int16).
    function write_on_scratch_dataarray3_rank1_int16(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int16), intent(in) :: x(1:)
            !! X component.
        integer(int16), intent(in) :: y(1:)
            !! Y component.
        integer(int16), intent(in) :: z(1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1, size(x, dim=1))])
    end function write_on_scratch_dataarray3_rank1_int16

    !| Write a dataarray with 3 components of rank 1 (int8).
    function write_on_scratch_dataarray3_rank1_int8(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int8), intent(in) :: x(1:)
            !! X component.
        integer(int8), intent(in) :: y(1:)
            !! Y component.
        integer(int8), intent(in) :: z(1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(x(n), y(n), z(n), n=1, size(x, dim=1))])
    end function write_on_scratch_dataarray3_rank1_int8

    !| Write a dataarray with 3 components of rank 2 (real64).
    function write_on_scratch_dataarray3_rank2_real64(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        real(real64), intent(in) :: x(1:, 1:)
            !! X component.
        real(real64), intent(in) :: y(1:, 1:)
            !! Y component.
        real(real64), intent(in) :: z(1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[((x(n1, n2), y(n1, n2), z(n1, n2), &
                                                      n1=1, size(x, dim=1)), &
                                                     n2=1, size(x, dim=2))])
    end function write_on_scratch_dataarray3_rank2_real64

    !| Write a dataarray with 3 components of rank 2 (real32).
    function write_on_scratch_dataarray3_rank2_real32(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        real(real32), intent(in) :: x(1:, 1:)
            !! X component.
        real(real32), intent(in) :: y(1:, 1:)
            !! Y component.
        real(real32), intent(in) :: z(1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[((x(n1, n2), y(n1, n2), z(n1, n2), &
                                                      n1=1, size(x, dim=1)), &
                                                     n2=1, size(x, dim=2))])
    end function write_on_scratch_dataarray3_rank2_real32

    !| Write a dataarray with 3 components of rank 2 (int64).
    function write_on_scratch_dataarray3_rank2_int64(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int64), intent(in) :: x(1:, 1:)
            !! X component.
        integer(int64), intent(in) :: y(1:, 1:)
            !! Y component.
        integer(int64), intent(in) :: z(1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[((x(n1, n2), y(n1, n2), z(n1, n2), &
                                                      n1=1, size(x, dim=1)), &
                                                     n2=1, size(x, dim=2))])
    end function write_on_scratch_dataarray3_rank2_int64

    !| Write a dataarray with 3 components of rank 2 (int32).
    function write_on_scratch_dataarray3_rank2_int32(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: x(1:, 1:)
            !! X component.
        integer(int32), intent(in) :: y(1:, 1:)
            !! Y component.
        integer(int32), intent(in) :: z(1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[((x(n1, n2), y(n1, n2), z(n1, n2), &
                                                      n1=1, size(x, dim=1)), &
                                                     n2=1, size(x, dim=2))])
    end function write_on_scratch_dataarray3_rank2_int32

    !| Write a dataarray with 3 components of rank 2 (int16).
    function write_on_scratch_dataarray3_rank2_int16(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int16), intent(in) :: x(1:, 1:)
            !! X component.
        integer(int16), intent(in) :: y(1:, 1:)
            !! Y component.
        integer(int16), intent(in) :: z(1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[((x(n1, n2), y(n1, n2), z(n1, n2), &
                                                      n1=1, size(x, dim=1)), &
                                                     n2=1, size(x, dim=2))])
    end function write_on_scratch_dataarray3_rank2_int16

    !| Write a dataarray with 3 components of rank 2 (int8).
    function write_on_scratch_dataarray3_rank2_int8(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int8), intent(in) :: x(1:, 1:)
            !! X component.
        integer(int8), intent(in) :: y(1:, 1:)
            !! Y component.
        integer(int8), intent(in) :: z(1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[((x(n1, n2), y(n1, n2), z(n1, n2), &
                                                      n1=1, size(x, dim=1)), &
                                                     n2=1, size(x, dim=2))])
    end function write_on_scratch_dataarray3_rank2_int8

    !| Write a dataarray with 3 components of rank 3 (real64).
    function write_on_scratch_dataarray3_rank3_real64(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        real(real64), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        real(real64), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), &
                                                       n1=1, size(x, dim=1)), n2=1, size(x, dim=2)), n3=1, size(x, dim=3))])
    end function write_on_scratch_dataarray3_rank3_real64

    !| Write a dataarray with 3 components of rank 3 (real32).
    function write_on_scratch_dataarray3_rank3_real32(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        real(real32), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        real(real32), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), &
                                                       n1=1, size(x, dim=1)), n2=1, size(x, dim=2)), n3=1, size(x, dim=3))])
    end function write_on_scratch_dataarray3_rank3_real32

    !| Write a dataarray with 3 components of rank 3 (int64).
    function write_on_scratch_dataarray3_rank3_int64(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int64), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int64), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), &
                                                       n1=1, size(x, dim=1)), n2=1, size(x, dim=2)), n3=1, size(x, dim=3))])
    end function write_on_scratch_dataarray3_rank3_int64

    !| Write a dataarray with 3 components of rank 3 (int32).
    function write_on_scratch_dataarray3_rank3_int32(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int32), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int32), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), &
                                                       n1=1, size(x, dim=1)), n2=1, size(x, dim=2)), n3=1, size(x, dim=3))])
    end function write_on_scratch_dataarray3_rank3_int32

    !| Write a dataarray with 3 components of rank 3 (int16).
    function write_on_scratch_dataarray3_rank3_int16(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int16), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int16), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), &
                                                       n1=1, size(x, dim=1)), n2=1, size(x, dim=2)), n3=1, size(x, dim=3))])
    end function write_on_scratch_dataarray3_rank3_int16

    !| Write a dataarray with 3 components of rank 3 (int8).
    function write_on_scratch_dataarray3_rank3_int8(self, x, y, z) result(n_byte)
        class(xml_writer_appended), intent(inout) :: self
            !! Writer.
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! X component.
        integer(int8), intent(in) :: y(1:, 1:, 1:)
            !! Y component.
        integer(int8), intent(in) :: z(1:, 1:, 1:)
            !! Z component.
        integer(int32) :: n_byte
            !! Number of bytes
        integer(int32) :: n1
            !! Counter.
        integer(int32) :: n2
            !! Counter.
        integer(int32) :: n3
            !! Counter.

        n_byte = self%write_on_scratch_dataarray(x=[(((x(n1, n2, n3), y(n1, n2, n3), z(n1, n2, n3), &
                                                       n1=1, size(x, dim=1)), n2=1, size(x, dim=2)), n3=1, size(x, dim=3))])
    end function write_on_scratch_dataarray3_rank3_int8
end module vtk_fortran_vtk_file_xml_writer_appended
