!| VTK file XMl writer, ascii local.
module vtk_fortran_vtk_file_xml_writer_ascii_local
    use, intrinsic :: iso_fortran_env
    use stringifor
    use vtk_fortran_dataarray_encoder
    use vtk_fortran_vtk_file_xml_writer_abstract

    implicit none
    private
    public :: xml_writer_ascii_local

    type, extends(xml_writer_abstract) :: xml_writer_ascii_local
        !! VTK file XML writer, ascii local.
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
    end type xml_writer_ascii_local
contains
    !| Initialize writer.
    function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
                        is_volatile, mesh_kind) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
        self%format_ch = format
        self%format_ch = self%format_ch%lower()
        call self%open_xml_file(filename=filename)
        call self%write_header_tag
        call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
        error = self%error
    end function initialize

    !| Finalize writer.
    function finalize(self) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        integer(int32) :: error
            !! Error status.

        call self%write_end_tag(name=self%topology%chars())
        call self%write_end_tag(name='VTKFile')
        call self%close_xml_file
        error = self%error
    end function finalize

    ! write_dataarray methods
    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank1_real64(self, data_name, x, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float64'
        n_components = 1
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank1_real64

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank1_real32(self, data_name, x, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float32'
        n_components = 1
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank1_real32

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int64).
    function write_dataarray1_rank1_int64(self, data_name, x, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int64'
        n_components = 1
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank1_int64

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank1_int32(self, data_name, x, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int32'
        n_components = 1
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank1_int32

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank1_int16(self, data_name, x, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int16'
        n_components = 1
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank1_int16

    !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank1_int8(self, data_name, x, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:)
            !! Data variable.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int8'
        n_components = 1
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank1_int8

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank2_real64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank2_real64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank2_real32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank2_real32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
    function write_dataarray1_rank2_int64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank2_int64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank2_int32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank2_int32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank2_int16(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int16'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank2_int16

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank2_int8(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int8'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank2_int8

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank3_real64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank3_real64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank3_real32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank3_real32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
    function write_dataarray1_rank3_int64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank3_int64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank3_int32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank3_int32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank3_int16(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int16'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank3_int16

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank3_int8(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int8'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank3_int8

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
    function write_dataarray1_rank4_real64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank4_real64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
    function write_dataarray1_rank4_real32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        real(real32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank4_real32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
    function write_dataarray1_rank4_int64(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int64), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int64'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank4_int64

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
    function write_dataarray1_rank4_int32(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int32), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int32'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank4_int32

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
    function write_dataarray1_rank4_int16(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int16), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int16'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank4_int16

    !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
    function write_dataarray1_rank4_int8(self, data_name, x, one_component, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        integer(int8), intent(in) :: x(1:, 1:, 1:, 1:)
            !! Data variable.
        logical, intent(in), optional :: one_component
            !! Force one component.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int8'
        n_components = size(x, dim=1)
        if (present(one_component)) then
            if (one_component) n_components = 1
        end if
        code = encode_ascii_dataarray(x=x)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray1_rank4_int8

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real64).
    function write_dataarray3_rank1_real64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float64'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank1_real64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real32).
    function write_dataarray3_rank1_real32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float32'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank1_real32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int64).
    function write_dataarray3_rank1_int64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int64'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank1_int64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int32).
    function write_dataarray3_rank1_int32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int32'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank1_int32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int16).
    function write_dataarray3_rank1_int16(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int16'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank1_int16

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int8).
    function write_dataarray3_rank1_int8(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int8'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank1_int8

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real64).
    function write_dataarray3_rank3_real64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float64'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank3_real64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real32).
    function write_dataarray3_rank3_real32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Float32'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank3_real32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int64).
    function write_dataarray3_rank3_int64(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int64'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank3_int64

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int32).
    function write_dataarray3_rank3_int32(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int32'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank3_int32

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int16).
    function write_dataarray3_rank3_int16(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int16'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank3_int16

    !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int8).
    function write_dataarray3_rank3_int8(self, data_name, x, y, z, is_tuples) result(error)
        class(xml_writer_ascii_local), intent(inout) :: self
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
            !! Use "NumberOfTuples" instead of "NumberOfComponents".
        integer(int32) :: error
            !! Error status.
        character(len=:), allocatable :: data_type
            !! Data type.
        integer(int32) :: n_components
            !! Number of components.
        character(len=:), allocatable :: code
            !! Data variable encoded, ascii or Base64 codec.

        data_type = 'Int8'
        n_components = 3
        code = encode_ascii_dataarray(x=x, y=y, z=z)
        call self%write_dataarray_tag(data_type=data_type, number_of_components=n_components, &
                                      data_name=data_name, data_content=code, &
                                      is_tuples=is_tuples)
        error = self%error
    end function write_dataarray3_rank3_int8

    !! Do nothing, ascii data cannot be appended.
    subroutine write_dataarray_appended(self)
        class(xml_writer_ascii_local), intent(inout) :: self
            !! Writer.
    end subroutine write_dataarray_appended
end module vtk_fortran_vtk_file_xml_writer_ascii_local
