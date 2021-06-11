!| VTK file abstract XML writer.
module vtk_fortran_vtk_file_xml_writer_abstract
    use, intrinsic :: iso_fortran_env
    use foxy
    use penf, only: str, endian, endianL
    use stringifor
    use vtk_fortran_parameters

    implicit none
    private
    public :: xml_writer_abstract

    type, abstract :: xml_writer_abstract
        !! VTK file abstract XML writer.
        type(string) :: format_ch
            !! Output format, string code.
        type(string) :: topology
            !! Mesh topology.
        integer(int32) :: indent = 0_int32
            !! Indent count.
        integer(int64) :: ioffset = 0_int64
            !! Offset count.
        integer(int32) :: xml = 0_int32
            !! XML Logical unit.
        integer(int32) :: vtm_block(1:2) = [-1_int32, -1_int32]
            !! Block indexes.
        integer(int32) :: error = 0_int32
            !! IO Error status.
        type(xml_tag) :: tag
            !! XML tags handler.
        logical :: is_volatile = .false.
            !! Flag to check volatile writer.
        type(string) :: xml_volatile
            !! XML file volatile (not a physical file).
    contains
        ! public methods (some deferred)
        procedure, pass(self) :: close_xml_file
            !! Close xml file.
        procedure, pass(self) :: open_xml_file
            !! Open xml file.
        procedure, pass(self) :: free
            !! Free allocated memory.
        procedure, pass(self) :: get_xml_volatile
            !! Return the XML volatile string file.
        procedure, pass(self) :: write_connectivity
            !! Write connectivity.
        procedure, pass(self) :: write_dataarray_location_tag
            !! Write dataarray location tag.
        procedure, pass(self) :: write_dataarray_tag
            !! Write dataarray tag.
        procedure, pass(self) :: write_dataarray_tag_appended
            !! Write dataarray appended tag.
        procedure, pass(self) :: write_end_tag
        !| Write `</tag_name>` end tag.
        procedure, pass(self) :: write_header_tag
            !! Write header tag.
        procedure, pass(self) :: write_parallel_open_block
            !! Write parallel open block.
        procedure, pass(self) :: write_parallel_close_block
            !! Write parallel close block.
        procedure, pass(self) :: write_parallel_dataarray
            !! Write parallel dataarray.
        procedure, pass(self) :: write_parallel_geo
            !! Write parallel geo.
        procedure, pass(self) :: write_self_closing_tag
            !! Write self closing tag.
        procedure, pass(self) :: write_start_tag
            !! Write start tag.
        procedure, pass(self) :: write_tag
            !! Write tag.
        procedure, pass(self) :: write_topology_tag
            !! Write topology tag.
        procedure(initialize_interface), deferred, pass(self) :: initialize
            !! Initialize writer.
        procedure(finalize_interface), deferred, pass(self) :: finalize
            !! Finalize writer.
        generic :: write_dataarray => &
            write_dataarray1_rank1_real64, &
            write_dataarray1_rank1_real32, &
            write_dataarray1_rank1_int64, &
            write_dataarray1_rank1_int32, &
            write_dataarray1_rank1_int16, &
            write_dataarray1_rank1_int8, &
            write_dataarray1_rank2_real64, &
            write_dataarray1_rank2_real32, &
            write_dataarray1_rank2_int64, &
            write_dataarray1_rank2_int32, &
            write_dataarray1_rank2_int16, &
            write_dataarray1_rank2_int8, &
            write_dataarray1_rank3_real64, &
            write_dataarray1_rank3_real32, &
            write_dataarray1_rank3_int64, &
            write_dataarray1_rank3_int32, &
            write_dataarray1_rank3_int16, &
            write_dataarray1_rank3_int8, &
            write_dataarray1_rank4_real64, &
            write_dataarray1_rank4_real32, &
            write_dataarray1_rank4_int64, &
            write_dataarray1_rank4_int32, &
            write_dataarray1_rank4_int16, &
            write_dataarray1_rank4_int8, &
            write_dataarray3_rank1_real64, &
            write_dataarray3_rank1_real32, &
            write_dataarray3_rank1_int64, &
            write_dataarray3_rank1_int32, &
            write_dataarray3_rank1_int16, &
            write_dataarray3_rank1_int8, &
            write_dataarray3_rank3_real64, &
            write_dataarray3_rank3_real32, &
            write_dataarray3_rank3_int64, &
            write_dataarray3_rank3_int32, &
            write_dataarray3_rank3_int16, &
            write_dataarray3_rank3_int8, &
            write_dataarray_location_tag
            !! Write data (array).
        generic :: write_fielddata => &
            write_fielddata1_rank0, &
            write_fielddata_tag
            !! Write FieldData tag.
        generic :: write_geo => &
            write_geo_strg_data1_rank2_real64, &
            write_geo_strg_data1_rank2_real32, &
            write_geo_strg_data1_rank4_real64, &
            write_geo_strg_data1_rank4_real32, &
            write_geo_strg_data3_rank1_real64, &
            write_geo_strg_data3_rank1_real32, &
            write_geo_strg_data3_rank3_real64, &
            write_geo_strg_data3_rank3_real32, &
            write_geo_rect_data3_rank1_real64, &
            write_geo_rect_data3_rank1_real32, &
            write_geo_unst_data1_rank2_real64, &
            write_geo_unst_data1_rank2_real32, &
            write_geo_unst_data3_rank1_real64, &
            write_geo_unst_data3_rank1_real32
            !! Write mesh.
        generic :: write_parallel_block_files => &
            write_parallel_block_file, &
            write_parallel_block_files_array, &
            write_parallel_block_files_string
            !! Write block list of files.
        generic :: write_piece => &
            write_piece_start_tag, &
            write_piece_start_tag_unst, &
            write_piece_end_tag
            !! Write Piece start/end tag.

        ! deferred methods
        procedure(write_dataarray1_rank1_real64_interface), deferred, pass(self) :: write_dataarray1_rank1_real64
            !! Data 1, rank 1, real64.
        procedure(write_dataarray1_rank1_real32_interface), deferred, pass(self) :: write_dataarray1_rank1_real32
            !! Data 1, rank 1, real32.
        procedure(write_dataarray1_rank1_int64_interface), deferred, pass(self) :: write_dataarray1_rank1_int64
            !! Data 1, rank 1, int64.
        procedure(write_dataarray1_rank1_int32_interface), deferred, pass(self) :: write_dataarray1_rank1_int32
            !! Data 1, rank 1, int32.
        procedure(write_dataarray1_rank1_int16_interface), deferred, pass(self) :: write_dataarray1_rank1_int16
            !! Data 1, rank 1, int16.
        procedure(write_dataarray1_rank1_int8_interface), deferred, pass(self) :: write_dataarray1_rank1_int8
            !! Data 1, rank 1, int8.
        procedure(write_dataarray1_rank2_real64_interface), deferred, pass(self) :: write_dataarray1_rank2_real64
            !! Data 1, rank 2, real64.
        procedure(write_dataarray1_rank2_real32_interface), deferred, pass(self) :: write_dataarray1_rank2_real32
            !! Data 1, rank 2, real32.
        procedure(write_dataarray1_rank2_int64_interface), deferred, pass(self) :: write_dataarray1_rank2_int64
            !! Data 1, rank 2, int64.
        procedure(write_dataarray1_rank2_int32_interface), deferred, pass(self) :: write_dataarray1_rank2_int32
            !! Data 1, rank 2, int32.
        procedure(write_dataarray1_rank2_int16_interface), deferred, pass(self) :: write_dataarray1_rank2_int16
            !! Data 1, rank 2, int16.
        procedure(write_dataarray1_rank2_int8_interface), deferred, pass(self) :: write_dataarray1_rank2_int8
            !! Data 1, rank 2, int8.
        procedure(write_dataarray1_rank3_real64_interface), deferred, pass(self) :: write_dataarray1_rank3_real64
            !! Data 1, rank 3, real64.
        procedure(write_dataarray1_rank3_real32_interface), deferred, pass(self) :: write_dataarray1_rank3_real32
            !! Data 1, rank 3, real32.
        procedure(write_dataarray1_rank3_int64_interface), deferred, pass(self) :: write_dataarray1_rank3_int64
            !! Data 1, rank 3, int64.
        procedure(write_dataarray1_rank3_int32_interface), deferred, pass(self) :: write_dataarray1_rank3_int32
            !! Data 1, rank 3, int32.
        procedure(write_dataarray1_rank3_int16_interface), deferred, pass(self) :: write_dataarray1_rank3_int16
            !! Data 1, rank 3, int16.
        procedure(write_dataarray1_rank3_int8_interface), deferred, pass(self) :: write_dataarray1_rank3_int8
            !! Data 1, rank 3, int8.
        procedure(write_dataarray1_rank4_real64_interface), deferred, pass(self) :: write_dataarray1_rank4_real64
            !! Data 1, rank 4, real64.
        procedure(write_dataarray1_rank4_real32_interface), deferred, pass(self) :: write_dataarray1_rank4_real32
            !! Data 1, rank 4, real32.
        procedure(write_dataarray1_rank4_int64_interface), deferred, pass(self) :: write_dataarray1_rank4_int64
            !! Data 1, rank 4, int64.
        procedure(write_dataarray1_rank4_int32_interface), deferred, pass(self) :: write_dataarray1_rank4_int32
            !! Data 1, rank 4, int32.
        procedure(write_dataarray1_rank4_int16_interface), deferred, pass(self) :: write_dataarray1_rank4_int16
            !! Data 1, rank 4, int16.
        procedure(write_dataarray1_rank4_int8_interface), deferred, pass(self) :: write_dataarray1_rank4_int8
            !! Data 1, rank 4, int8.
        procedure(write_dataarray3_rank1_real64_interface), deferred, pass(self) :: write_dataarray3_rank1_real64
            !! Data 3, rank 1, real64.
        procedure(write_dataarray3_rank1_real32_interface), deferred, pass(self) :: write_dataarray3_rank1_real32
            !! Data 3, rank 1, real32.
        procedure(write_dataarray3_rank1_int64_interface), deferred, pass(self) :: write_dataarray3_rank1_int64
            !! Data 3, rank 1, int64.
        procedure(write_dataarray3_rank1_int32_interface), deferred, pass(self) :: write_dataarray3_rank1_int32
            !! Data 3, rank 1, int32.
        procedure(write_dataarray3_rank1_int16_interface), deferred, pass(self) :: write_dataarray3_rank1_int16
            !! Data 3, rank 1, int16.
        procedure(write_dataarray3_rank1_int8_interface), deferred, pass(self) :: write_dataarray3_rank1_int8
            !! Data 3, rank 1, int8.
        procedure(write_dataarray3_rank3_real64_interface), deferred, pass(self) :: write_dataarray3_rank3_real64
            !! Data 3, rank 3, real64.
        procedure(write_dataarray3_rank3_real32_interface), deferred, pass(self) :: write_dataarray3_rank3_real32
            !! Data 3, rank 3, real32.
        procedure(write_dataarray3_rank3_int64_interface), deferred, pass(self) :: write_dataarray3_rank3_int64
            !! Data 3, rank 3, int64.
        procedure(write_dataarray3_rank3_int32_interface), deferred, pass(self) :: write_dataarray3_rank3_int32
            !! Data 3, rank 3, int32.
        procedure(write_dataarray3_rank3_int16_interface), deferred, pass(self) :: write_dataarray3_rank3_int16
            !! Data 3, rank 3, int16.
        procedure(write_dataarray3_rank3_int8_interface), deferred, pass(self) :: write_dataarray3_rank3_int8
            !! Data 3, rank 3, int8.
        procedure(write_dataarray_appended_interface), deferred, pass(self) :: write_dataarray_appended
            !! Write appended.

        ! private methods
        procedure, pass(self), private :: write_fielddata1_rank0
            !! Write FieldData tag (data 1, rank 0, real64).
        procedure, pass(self), private :: write_fielddata_tag
            !! Write FieldData tag.
        procedure, pass(self), private :: write_geo_strg_data1_rank2_real64
            !! Write **StructuredGrid** mesh (data 1, rank 2, real64).
        procedure, pass(self), private :: write_geo_strg_data1_rank2_real32
            !! Write **StructuredGrid** mesh (data 1, rank 2, real32).
        procedure, pass(self), private :: write_geo_strg_data1_rank4_real64
            !! Write **StructuredGrid** mesh (data 1, rank 4, real64).
        procedure, pass(self), private :: write_geo_strg_data1_rank4_real32
            !! Write **StructuredGrid** mesh (data 1, rank 4, real32).
        procedure, pass(self), private :: write_geo_strg_data3_rank1_real64
            !! Write **StructuredGrid** mesh (data 3, rank 1, real64).
        procedure, pass(self), private :: write_geo_strg_data3_rank1_real32
            !! Write **StructuredGrid** mesh (data 3, rank 1, real32).
        procedure, pass(self), private :: write_geo_strg_data3_rank3_real64
            !! Write **StructuredGrid** mesh (data 3, rank 3, real64).
        procedure, pass(self), private :: write_geo_strg_data3_rank3_real32
            !! Write **StructuredGrid** mesh (data 3, rank 3, real32).
        procedure, pass(self), private :: write_geo_rect_data3_rank1_real64
            !! Write **RectilinearGrid** mesh (data 3, rank 1, real64).
        procedure, pass(self), private :: write_geo_rect_data3_rank1_real32
            !! Write **RectilinearGrid** mesh (data 3, rank 1, real32).
        procedure, pass(self), private :: write_geo_unst_data1_rank2_real64
            !! Write **UnstructuredGrid** mesh (data 1, rank 2, real64).
        procedure, pass(self), private :: write_geo_unst_data1_rank2_real32
            !! Write **UnstructuredGrid** mesh (data 1, rank 2, real32).
        procedure, pass(self), private :: write_geo_unst_data3_rank1_real64
            !! Write **UnstructuredGrid** mesh (data 3, rank 1, real64).
        procedure, pass(self), private :: write_geo_unst_data3_rank1_real32
            !! Write **UnstructuredGrid** mesh (data 3, rank 1, real32).
        procedure, pass(self), private :: write_piece_start_tag
        !| Write `<Piece ...>` start tag.
        procedure, pass(self), private :: write_piece_start_tag_unst
        !| Write `<Piece ...>` start tag for unstructured topology.
        procedure, pass(self), private :: write_piece_end_tag
        !| Write `</Piece>` end tag.
        procedure, pass(self), private :: write_parallel_block_file
            !! Write single file that belong to the current block.
        procedure, pass(self), private :: write_parallel_block_files_array
            !! Write block list of files (array input).
        procedure, pass(self), private :: write_parallel_block_files_string
            !! Write block list of files (string input).
    end type xml_writer_abstract

    abstract interface
        !| Initialize writer.
        function initialize_interface(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, &
                                      is_volatile, mesh_kind) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function initialize_interface

        !| Finalize writer.
        function finalize_interface(self) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
            integer(int32) :: error
            !! Error status.
        end function finalize_interface

        !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (real64).
        function write_dataarray1_rank1_real64_interface(self, data_name, x, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real64
            class(xml_writer_abstract), intent(inout) :: self
                !! Writer.
            character(*), intent(in) :: data_name
                !! Data name.
            real(real64), intent(in) :: x(1:)
                !! Data variable.
            logical, intent(in), optional :: is_tuples
                !! Use "NumberOfTuples" instead of "NumberOfComponents".
            integer(int32) :: error
                !! Error status.
        end function write_dataarray1_rank1_real64_interface

        !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (real32).
        function write_dataarray1_rank1_real32_interface(self, data_name, x, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real32
            class(xml_writer_abstract), intent(inout) :: self
                !! Writer.
            character(*), intent(in) :: data_name
                !! Data name.
            real(real32), intent(in) :: x(1:)
                !! Data variable.
            logical, intent(in), optional :: is_tuples
                !! Use "NumberOfTuples" instead of "NumberOfComponents".
            integer(int32) :: error
                !! Error status.
        end function write_dataarray1_rank1_real32_interface

        !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int64).
        function write_dataarray1_rank1_int64_interface(self, data_name, x, is_tuples) result(error)
            import :: xml_writer_abstract, int32, int64
            class(xml_writer_abstract), intent(inout) :: self
                !! Writer.
            character(*), intent(in) :: data_name
                !! Data name.
            integer(int64), intent(in) :: x(1:)
                !! Data variable.
            logical, intent(in), optional :: is_tuples
                !! Use "NumberOfTuples" instead of "NumberOfComponents".
            integer(int32) :: error
                !! Error status.
        end function write_dataarray1_rank1_int64_interface

        !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int32).
        function write_dataarray1_rank1_int32_interface(self, data_name, x, is_tuples) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
                !! Writer.
            character(*), intent(in) :: data_name
                !! Data name.
            integer(int32), intent(in) :: x(1:)
                !! Data variable.
            logical, intent(in), optional :: is_tuples
                !! Use "NumberOfTuples" instead of "NumberOfComponents".
            integer(int32) :: error
                !! Error status.
        end function write_dataarray1_rank1_int32_interface

        !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int16).
        function write_dataarray1_rank1_int16_interface(self, data_name, x, is_tuples) result(error)
            import :: xml_writer_abstract, int16, int32
            class(xml_writer_abstract), intent(inout) :: self
                !! Writer.
            character(*), intent(in) :: data_name
                !! Data name.
            integer(int16), intent(in) :: x(1:)
                !! Data variable.
            logical, intent(in), optional :: is_tuples
                !! Use "NumberOfTuples" instead of "NumberOfComponents".
            integer(int32) :: error
                !! Error status.
        end function write_dataarray1_rank1_int16_interface

        !| Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (int8).
        function write_dataarray1_rank1_int8_interface(self, data_name, x, is_tuples) result(error)
            import :: xml_writer_abstract, int8, int32
            class(xml_writer_abstract), intent(inout) :: self
                !! Writer.
            character(*), intent(in) :: data_name
                !! Data name.
            integer(int8), intent(in) :: x(1:)
                !! Data variable.
            logical, intent(in), optional :: is_tuples
                !! Use "NumberOfTuples" instead of "NumberOfComponents".
            integer(int32) :: error
                !! Error status.
        end function write_dataarray1_rank1_int8_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
        function write_dataarray1_rank2_real64_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank2_real64_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
        function write_dataarray1_rank2_real32_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank2_real32_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
        function write_dataarray1_rank2_int64_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, int64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank2_int64_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
        function write_dataarray1_rank2_int32_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank2_int32_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
        function write_dataarray1_rank2_int16_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int16, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank2_int16_interface

        function write_dataarray1_rank2_int8_interface(self, data_name, x, one_component, is_tuples) result(error)

            !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
            import :: xml_writer_abstract, int8, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank2_int8_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
        function write_dataarray1_rank3_real64_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank3_real64_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
        function write_dataarray1_rank3_real32_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank3_real32_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
        function write_dataarray1_rank3_int64_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, int64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank3_int64_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
        function write_dataarray1_rank3_int32_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank3_int32_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
        function write_dataarray1_rank3_int16_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int16, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank3_int16_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
        function write_dataarray1_rank3_int8_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int8, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank3_int8_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real64).
        function write_dataarray1_rank4_real64_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank4_real64_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (real32).
        function write_dataarray1_rank4_real32_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank4_real32_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int64).
        function write_dataarray1_rank4_int64_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32, int64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank4_int64_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int32).
        function write_dataarray1_rank4_int32_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank4_int32_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int16).
        function write_dataarray1_rank4_int16_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int16, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank4_int16_interface

        !| Write `<DataArray... NumberOfComponents="n"...>...</DataArray>` tag (int8).
        function write_dataarray1_rank4_int8_interface(self, data_name, x, one_component, is_tuples) result(error)
            import :: xml_writer_abstract, int8, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray1_rank4_int8_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real64).
        function write_dataarray3_rank1_real64_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank1_real64_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real32).
        function write_dataarray3_rank1_real32_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank1_real32_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int64).
        function write_dataarray3_rank1_int64_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32, int64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank1_int64_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int32).
        function write_dataarray3_rank1_int32_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank1_int32_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int16).
        function write_dataarray3_rank1_int16_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int16, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank1_int16_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int8).
        function write_dataarray3_rank1_int8_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int8, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank1_int8_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real64).
        function write_dataarray3_rank3_real64_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank3_real64_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (real32).
        function write_dataarray3_rank3_real32_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32, real32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank3_real32_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int64).
        function write_dataarray3_rank3_int64_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32, int64
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank3_int64_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int32).
        function write_dataarray3_rank3_int32_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank3_int32_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int16).
        function write_dataarray3_rank3_int16_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int16, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank3_int16_interface

        !| Write `<DataArray... NumberOfComponents="3"...>...</DataArray>` tag (int8).
        function write_dataarray3_rank3_int8_interface(self, data_name, x, y, z, is_tuples) result(error)
            import :: xml_writer_abstract, int8, int32
            class(xml_writer_abstract), intent(inout) :: self
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
        end function write_dataarray3_rank3_int8_interface

        !| Write `<AppendedData...>...</AppendedData>` tag.
        subroutine write_dataarray_appended_interface(self)
            import :: xml_writer_abstract
            class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        end subroutine write_dataarray_appended_interface
    end interface
contains
    ! files methods

    !| Close XML file.
    subroutine close_xml_file(self)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.

        if (.not. self%is_volatile) close (unit=self%xml, iostat=self%error)
    end subroutine close_xml_file

    !| Open XML file.
    subroutine open_xml_file(self, filename)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: filename
            !! File name.

        if (.not. self%is_volatile) then
            open (newunit=self%xml, &
                  file=trim(adjustl(filename)), &
                  form='UNFORMATTED', &
                  access='STREAM', &
                  action='WRITE', &
                  status='REPLACE', &
                  iostat=self%error)
        else
            self%xml_volatile = ''
        end if
    end subroutine open_xml_file

    !| Free allocated memory.
    elemental subroutine free(self, error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(out), optional :: error
            !! Error status.

        call self%format_ch%free
        call self%topology%free
        self%indent = 0_int32
        self%ioffset = 0_int64
        self%xml = 0_int32
        self%vtm_block(1:2) = [-1_int32, -1_int32]
        self%error = 0_int32
        call self%tag%free
        self%is_volatile = .false.
        call self%xml_volatile%free
    end subroutine free

    !| Return the eventual XML volatile string file.
    pure subroutine get_xml_volatile(self, xml_volatile, error)
        class(xml_writer_abstract), intent(in) :: self
            !! Writer.
        character(len=:), intent(out), allocatable :: xml_volatile
            !! XML volatile file.
        integer(int32), intent(out), optional :: error
            !! Error status.

        if (self%is_volatile) then
            xml_volatile = self%xml_volatile%raw
        end if
    end subroutine get_xml_volatile

    ! tag methods
    !| Write `</tag_name>` end tag.
    subroutine write_end_tag(self, name)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: name
            !! Tag name.

        self%indent = self%indent - 2
        self%tag = xml_tag(name=name, indent=self%indent)
        if (.not. self%is_volatile) then
            call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., end_record=end_rec, only_end=.true.)
        else
            self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true., only_end=.true.)//end_rec
        end if
    end subroutine write_end_tag

    !! Write header tag.
    subroutine write_header_tag(self)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        type(string) :: buffer
            !! Buffer string.

        buffer = '<?xml version="1.0"?>'//end_rec
        if (endian == endianL) then
            buffer = buffer//'<VTKFile type="'//self%topology//'" version="1.0" byte_order="LittleEndian">'
        else
            buffer = buffer//'<VTKFile type="'//self%topology//'" version="1.0" byte_order="BigEndian">'
        end if
        if (.not. self%is_volatile) then
            write (unit=self%xml, iostat=self%error) buffer//end_rec
        else
            self%xml_volatile = self%xml_volatile//buffer//end_rec
        end if
        self%indent = 2
    end subroutine write_header_tag

    !| Write `<tag_name.../>` self closing tag.
    subroutine write_self_closing_tag(self, name, attributes)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: name
            !! Tag name.
        character(*), intent(in), optional :: attributes
            !! Tag attributes.

        self%tag = xml_tag(name=name, attributes_stream=attributes, sanitize_attributes_value=.true., indent=self%indent, &
                           is_self_closing=.true.)
        if (.not. self%is_volatile) then
            call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., end_record=end_rec)
        else
            self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true.)//end_rec
        end if
    end subroutine write_self_closing_tag

    !| Write `<tag_name...>` start tag.
    subroutine write_start_tag(self, name, attributes)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: name
            !! Tag name.
        character(*), intent(in), optional :: attributes
            !! Tag attributes.

        self%tag = xml_tag(name=name, attributes_stream=attributes, sanitize_attributes_value=.true., indent=self%indent)
        if (.not. self%is_volatile) then
            call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., end_record=end_rec, only_start=.true.)
        else
            self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true., only_start=.true.)//end_rec
        end if
        self%indent = self%indent + 2
    end subroutine write_start_tag

    !| Write `<tag_name...>...</tag_name>` tag.
    subroutine write_tag(self, name, attributes, content)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: name
            !! Tag name.
        character(*), intent(in), optional :: attributes
            !! Tag attributes.
        character(*), intent(in), optional :: content
            !! Tag content.

        self%tag = xml_tag(name=name, attributes_stream=attributes, sanitize_attributes_value=.true., content=content, &
                           indent=self%indent)
        if (.not. self%is_volatile) then
            call self%tag%write(unit=self%xml, iostat=self%error, is_indented=.true., &
                                is_content_indented=.true., end_record=end_rec)
        else
            self%xml_volatile = self%xml_volatile//self%tag%stringify(is_indented=.true., &
                                                                      is_content_indented=.true.)//end_rec
        end if
    end subroutine write_tag

    !! Write XML topology tag.
    subroutine write_topology_tag(self, nx1, nx2, ny1, ny2, nz1, nz2, mesh_kind)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
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
        type(string) :: buffer
            !! Buffer string.

        buffer = ''
        select case (self%topology%chars())
        case ('RectilinearGrid', 'StructuredGrid')
            buffer = 'WholeExtent="'// &
                     trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                     trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                     trim(str(n=nz1))//' '//trim(str(n=nz2))//'"'
        case ('PRectilinearGrid', 'PStructuredGrid')
            buffer = 'WholeExtent="'// &
                     trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                     trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                     trim(str(n=nz1))//' '//trim(str(n=nz2))//'" GhostLevel="#"'
        case ('PUnstructuredGrid')
            buffer = 'GhostLevel="0"'
        end select
        call self%write_start_tag(name=self%topology%chars(), attributes=buffer%chars())
        ! parallel topologies peculiars
        select case (self%topology%chars())
        case ('PRectilinearGrid')
            if (.not. present(mesh_kind)) then
                self%error = 1
                return
            end if
            call self%write_start_tag(name='PCoordinates')
            call self%write_self_closing_tag(name='PDataArray', attributes='type="'//trim(mesh_kind)//'"')
            call self%write_self_closing_tag(name='PDataArray', attributes='type="'//trim(mesh_kind)//'"')
            call self%write_self_closing_tag(name='PDataArray', attributes='type="'//trim(mesh_kind)//'"')
            call self%write_end_tag(name='PCoordinates')
        case ('PStructuredGrid', 'PUnstructuredGrid')
            if (.not. present(mesh_kind)) then
                self%error = 1
                return
            end if
            call self%write_start_tag(name='PPoints')
            call self%write_self_closing_tag(name='PDataArray', &
                                             attributes='type="'//trim(mesh_kind)//'" NumberOfComponents="3" Name="Points"')
            call self%write_end_tag(name='PPoints')
        end select
    end subroutine write_topology_tag

    ! write_dataarray
    !| Write `<DataArray...>...</DataArray>` tag.
    subroutine write_dataarray_tag(self, data_type, number_of_components, data_name, data_content, is_tuples)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_type
            !! Type of dataarray.
        integer(int32), intent(in) :: number_of_components
            !! Number of dataarray components.
        character(*), intent(in) :: data_name
            !! Data name.
        character(*), intent(in), optional :: data_content
            !! Data content.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        type(string) :: tag_attributes
            !! Tag attributes.
        logical :: is_tuples_
            !! Use "NumberOfTuples".

        is_tuples_ = .false.
        if (present(is_tuples)) is_tuples_ = is_tuples
        if (is_tuples_) then
            tag_attributes = 'type="'//trim(adjustl(data_type))// &
                             '" NumberOfTuples="'//trim(str(number_of_components, .true.))// &
                             '" Name="'//trim(adjustl(data_name))// &
                             '" format="'//self%format_ch//'"'
        else
            tag_attributes = 'type="'//trim(adjustl(data_type))// &
                             '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
                             '" Name="'//trim(adjustl(data_name))// &
                             '" format="'//self%format_ch//'"'
        end if
        call self%write_tag(name='DataArray', attributes=tag_attributes%chars(), content=data_content)
    end subroutine write_dataarray_tag

    !| Write `<DataArray.../>` tag.
    subroutine write_dataarray_tag_appended(self, data_type, number_of_components, data_name, is_tuples)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_type
            !! Type of dataarray.
        integer(int32), intent(in) :: number_of_components
            !! Number of dataarray components.
        character(*), intent(in) :: data_name
            !! Data name.
        logical, intent(in), optional :: is_tuples
            !! Use "NumberOfTuples".
        type(string) :: tag_attributes
            !! Tag attributes.
        logical :: is_tuples_
            !! Use "NumberOfTuples".

        is_tuples_ = .false.
        if (present(is_tuples)) is_tuples_ = is_tuples
        if (is_tuples_) then
            tag_attributes = 'type="'//trim(adjustl(data_type))// &
                             '" NumberOfTuples="'//trim(str(number_of_components, .true.))// &
                             '" Name="'//trim(adjustl(data_name))// &
                             '" format="'//self%format_ch// &
                             '" offset="'//trim(str(self%ioffset, .true.))//'"'
        else
            tag_attributes = 'type="'//trim(adjustl(data_type))// &
                             '" NumberOfComponents="'//trim(str(number_of_components, .true.))// &
                             '" Name="'//trim(adjustl(data_name))// &
                             '" format="'//self%format_ch// &
                             '" offset="'//trim(str(self%ioffset, .true.))//'"'
        end if
        call self%write_self_closing_tag(name='DataArray', attributes=tag_attributes%chars())
    end subroutine write_dataarray_tag_appended

    !| Write `<[/]PointData>` or `<[/]CellData>` open/close tag.
    function write_dataarray_location_tag(self, location, action) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: location
            !! Location of variables: **cell** or **node** centered.
        character(*), intent(in) :: action
            !! Action: **open** or **close** tag.
        integer(int32) :: error
            !! Error status.
        type(string) :: location_
            !! Location string.
        type(string) :: action_
            !! Action string.

        !|
        ! @note **must** be called before saving the data related to geometric mesh, this function initializes the
        ! saving of data variables indicating the *location* (node or cell centered) of variables that will be saved.
        ! @note A single file can contain both cell and node centered variables. In this case the VTK_DAT_XML function must be
        ! called two times, before saving cell-centered variables and before saving node-centered variables.
        !
        !### Examples of usage
        !
        !#### Opening node piece
        !```fortran
        ! error = vtk%write_dataarray('node','OPeN')
        !```
        !
        !#### Closing node piece
        !```fortran
        ! error = vtk%write_dataarray('node','Close')
        !```
        !
        !#### Opening cell piece
        !```fortran
        ! error = vtk%write_dataarray('cell','OPEN')
        !```
        !
        !#### Closing cell piece
        !```fortran
        !error = vtk%write_dataarray('cell','close')
        !```

        location_ = trim(adjustl(location)); location_ = location_%upper()
        action_ = trim(adjustl(action)); action_ = action_%upper()
        select case (location_%chars())
        case ('CELL')
            location_ = 'CellData'
        case ('NODE')
            location_ = 'PointData'
        end select
        select case (self%topology%chars())
        case ('PRectilinearGrid', 'PStructuredGrid', 'PUnstructuredGrid')
            location_ = 'P'//location_
        end select
        select case (action_%chars())
        case ('OPEN')
            call self%write_start_tag(name=location_%chars())
        case ('CLOSE')
            call self%write_end_tag(name=location_%chars())
        end select
        error = self%error
    end function write_dataarray_location_tag

    ! write_fielddata methods
    !| Write `<DataArray... NumberOfTuples="..."...>...</DataArray>` tag (real64).
    function write_fielddata1_rank0(self, data_name, x) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        class(*), intent(in) :: x
            !! Data variable.
        integer(int32) :: error
            !! Error status.

        select type (x)
        type is (real(real64))
            self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
        type is (real(real32))
            self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
        type is (integer(int64))
            self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
        type is (integer(int32))
            self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
        type is (integer(int16))
            self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
        type is (integer(int8))
            self%error = self%write_dataarray(data_name=data_name, x=[x], is_tuples=.true.)
        end select
        error = self%error
    end function write_fielddata1_rank0

    !| Write `<FieldData>`/`</FieldData>` start/end tag.
    function write_fielddata_tag(self, action) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: action
            !! Action: **open** or **close** tag.
        integer(int32) :: error
            !! Error status.
        type(string) :: action_
            !! Action string.

        action_ = trim(adjustl(action)); action_ = action_%upper()
        select case (action_%chars())
        case ('OPEN')
            call self%write_start_tag(name='FieldData')
        case ('CLOSE')
            call self%write_end_tag(name='FieldData')
        end select
        error = self%error
    end function write_fielddata_tag

    ! write_piece methods
    !| Write `<Piece ...>` start tag.
    function write_piece_start_tag(self, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: nx1
            !! Initial node of x axis.
        integer(int32), intent(in) :: nx2
            !! Final node of x axis.
        integer(int32), intent(in) :: ny1
            !! Initial node of y axis.
        integer(int32), intent(in) :: ny2
            !! Final node of y axis.
        integer(int32), intent(in) :: nz1
            !! Initial node of z axis.
        integer(int32), intent(in) :: nz2
            !! Final node of z axis.
        integer(int32) :: error
            !! Error status.
        type(string) :: tag_attributes
            !! Tag attributes.

        tag_attributes = 'Extent="'//trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                         trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                         trim(str(n=nz1))//' '//trim(str(n=nz2))//'"'
        call self%write_start_tag(name='Piece', attributes=tag_attributes%chars())
        error = self%error
    end function write_piece_start_tag

    !| Write `<Piece ...>` start tag for unstructured topology.
    function write_piece_start_tag_unst(self, np, nc) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: np
            !! Number of points.
        integer(int32), intent(in) :: nc
            !! Number of cells.
        integer(int32) :: error
            !! Error status.
        type(string) :: tag_attributes
            !! Tag attributes.

        tag_attributes = 'NumberOfPoints="'//trim(str(n=np))//'" NumberOfCells="'//trim(str(n=nc))//'"'
        call self%write_start_tag(name='Piece', attributes=tag_attributes%chars())
        error = self%error
    end function write_piece_start_tag_unst

    !| Write `</Piece>` end tag.
    function write_piece_end_tag(self) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32) :: error
            !! Error status.

        call self%write_end_tag(name='Piece')
        error = self%error
    end function write_piece_end_tag

    ! write_geo_rect methods
    !| Write mesh with **RectilinearGrid** topology (data 3, rank 1, real64).
    function write_geo_rect_data3_rank1_real64(self, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        real(real64), intent(in) :: x(1:)
            !! X coordinates.
        real(real64), intent(in) :: y(1:)
            !! Y coordinates.
        real(real64), intent(in) :: z(1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        call self%write_start_tag(name='Coordinates')
        error = self%write_dataarray(data_name='X', x=x)
        error = self%write_dataarray(data_name='Y', x=y)
        error = self%write_dataarray(data_name='Z', x=z)
        call self%write_end_tag(name='Coordinates')
        error = self%error
    end function write_geo_rect_data3_rank1_real64

    !| Write mesh with **RectilinearGrid** topology (data 3, rank 1, real32).
    function write_geo_rect_data3_rank1_real32(self, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        real(real32), intent(in) :: x(1:)
            !! X coordinates.
        real(real32), intent(in) :: y(1:)
            !! Y coordinates.
        real(real32), intent(in) :: z(1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        call self%write_start_tag(name='Coordinates')
        error = self%write_dataarray(data_name='X', x=x)
        error = self%write_dataarray(data_name='Y', x=y)
        error = self%write_dataarray(data_name='Z', x=z)
        call self%write_end_tag(name='Coordinates')
        error = self%error
    end function write_geo_rect_data3_rank1_real32

    ! write_geo_strg methods
    !| Write mesh with **StructuredGrid** topology (data 1, rank 2, real64).
    function write_geo_strg_data1_rank2_real64(self, xyz) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        real(real64), intent(in) :: xyz(1:, 1:)
            !! X, y, z coordinates [1:3,1:n].
        integer(int32) :: error
            !! Error status.

        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=xyz)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data1_rank2_real64

    !| Write mesh with **StructuredGrid** topology (data 1, rank 2, real32).
    function write_geo_strg_data1_rank2_real32(self, xyz) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        real(real32), intent(in) :: xyz(1:, 1:)
            !! X, y, z coordinates [1:3,:].
        integer(int32) :: error
            !! Error status.

        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=xyz)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data1_rank2_real32

    !| Write mesh with **StructuredGrid** topology (data 1, rank 4, real64).
    function write_geo_strg_data1_rank4_real64(self, xyz) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        real(real64), intent(in) :: xyz(1:, 1:, 1:, 1:)
            !! X, y, z coordinates [1:3,:,:,:].
        integer(int32) :: error
            !! Error status.

        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=xyz)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data1_rank4_real64

    !| Write mesh with **StructuredGrid** topology (data 1, rank 4, real32).
    function write_geo_strg_data1_rank4_real32(self, xyz) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        real(real32), intent(in) :: xyz(1:, 1:, 1:, 1:)
            !! X, y, z coordinates [1:3,:,:,:].
        integer(int32) :: error
            !! Error status.

        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=xyz)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data1_rank4_real32

    !| Write mesh with **StructuredGrid** topology (data 3, rank 1, real64).
    function write_geo_strg_data3_rank1_real64(self, n, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: n
            !! Number of nodes.
        real(real64), intent(in) :: x(1:)
            !! X coordinates.
        real(real64), intent(in) :: y(1:)
            !! Y coordinates.
        real(real64), intent(in) :: z(1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        if ((n /= size(x, dim=1)) .or. (n /= size(y, dim=1)) .or. (n /= size(z, dim=1))) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data3_rank1_real64

    !| Write mesh with **StructuredGrid** topology (data 3, rank 1, real32).
    function write_geo_strg_data3_rank1_real32(self, n, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: n
            !! Number of nodes.
        real(real32), intent(in) :: x(1:)
            !! X coordinates.
        real(real32), intent(in) :: y(1:)
            !! Y coordinates.
        real(real32), intent(in) :: z(1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        if ((n /= size(x, dim=1)) .or. (n /= size(y, dim=1)) .or. (n /= size(z, dim=1))) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data3_rank1_real32

    !| Write mesh with **StructuredGrid** topology (data 3, rank 3, real64).
    function write_geo_strg_data3_rank3_real64(self, n, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: n
            !! Number of nodes.
        real(real64), intent(in) :: x(1:, 1:, 1:)
            !! X coordinates.
        real(real64), intent(in) :: y(1:, 1:, 1:)
            !! Y coordinates.
        real(real64), intent(in) :: z(1:, 1:, 1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        if ((n /= size(x, dim=1)*size(x, dim=2)*size(x, dim=3)) .or. &
            (n /= size(y, dim=1)*size(y, dim=2)*size(y, dim=3)) .or. &
            (n /= size(z, dim=1)*size(z, dim=2)*size(z, dim=3))) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data3_rank3_real64

    !| Write mesh with **StructuredGrid** topology (data 3, rank 3, real32).
    function write_geo_strg_data3_rank3_real32(self, n, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: n
            !! Number of nodes.
        real(real32), intent(in) :: x(1:, 1:, 1:)
            !! X coordinates.
        real(real32), intent(in) :: y(1:, 1:, 1:)
            !! Y coordinates.
        real(real32), intent(in) :: z(1:, 1:, 1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        if ((n /= size(x, dim=1)*size(x, dim=2)*size(x, dim=3)) .or. &
            (n /= size(y, dim=1)*size(y, dim=2)*size(y, dim=3)) .or. &
            (n /= size(z, dim=1)*size(z, dim=2)*size(z, dim=3))) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_strg_data3_rank3_real32

    ! write_geo_unst methods
    !| Write mesh with **UnstructuredGrid** topology (data 1, rank 2, real64).
    function write_geo_unst_data1_rank2_real64(self, np, nc, xyz) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: np
            !! Number of points.
        integer(int32), intent(in) :: nc
            !! Number of cells.
        real(real64), intent(in) :: xyz(1:, 1:)
            !! X, y, z coordinates [1:3,:].
        integer(int32) :: error
            !! Error status.

        if (np /= size(xyz, dim=2)) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=xyz)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_unst_data1_rank2_real64

    !| Write mesh with **UnstructuredGrid** topology (data 1, rank 2, real32).
    function write_geo_unst_data1_rank2_real32(self, np, nc, xyz) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: np
            !! Number of points.
        integer(int32), intent(in) :: nc
            !! Number of cells.
        real(real32), intent(in) :: xyz(1:, 1:)
            !! X, y, z coordinates [1:3,:].
        integer(int32) :: error
            !! Error status.

        if (np /= size(xyz, dim=2)) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=xyz)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_unst_data1_rank2_real32

    !| Write mesh with **UnstructuredGrid** topology (data 3, rank 1, real64).
    function write_geo_unst_data3_rank1_real64(self, np, nc, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: np
            !! Number of points.
        integer(int32), intent(in) :: nc
            !! Number of cells.
        real(real64), intent(in) :: x(1:)
            !! X coordinates.
        real(real64), intent(in) :: y(1:)
            !! Y coordinates.
        real(real64), intent(in) :: z(1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        if ((np /= size(x, dim=1)) .or. (np /= size(y, dim=1)) .or. (np /= size(z, dim=1))) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_unst_data3_rank1_real64

    !| Write mesh with **UnstructuredGrid** topology (data 3, rank 1, real32).
    function write_geo_unst_data3_rank1_real32(self, np, nc, x, y, z) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: np
            !! Number of points.
        integer(int32), intent(in) :: nc
            !! Number of cells.
        real(real32), intent(in) :: x(1:)
            !! X coordinates.
        real(real32), intent(in) :: y(1:)
            !! Y coordinates.
        real(real32), intent(in) :: z(1:)
            !! Z coordinates.
        integer(int32) :: error
            !! Error status.

        if ((np /= size(x, dim=1)) .or. (np /= size(y, dim=1)) .or. (np /= size(z, dim=1))) then
            self%error = 1
            return
        end if
        call self%write_start_tag(name='Points')
        error = self%write_dataarray(data_name='Points', x=x, y=y, z=z)
        call self%write_end_tag(name='Points')
        error = self%error
    end function write_geo_unst_data3_rank1_real32

    !| Write mesh connectivity.
    function write_connectivity(self, nc, connectivity, offset, cell_type) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: nc
            !! Number of cells.
        integer(int32), intent(in) :: connectivity(1:)
            !! Mesh connectivity.
        integer(int32), intent(in) :: offset(1:)
            !! Cell offset.
        integer(int8), intent(in) :: cell_type(1:)
            !! VTK cell type.
        integer(int32) :: error
            !! Error status.

        !|
        ! **Must** be used when unstructured grid is used, it saves the connectivity of the unstructured gird.
        ! @note The vector **connect** must follow the VTK-XML standard. It is passed as *assumed-shape array*
        ! because its dimensions is related to the mesh dimensions in a complex way. Its dimensions can be calculated by the following
        ! equation: \(dc = \sum\limits_{i = 1}^{NC} {nvertex_i }\).
        ! Note that this equation is different from the legacy one. The XML connectivity convention is quite different from the
        ! legacy standard.
        ! As an example suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with
        ! square basis (5 vertices) and suppose that the basis of pyramid is constitute by a face of the hexahedron and so the two cells
        ! share 4 vertices. The above equation gives \(dc=8+5=13\). The connectivity vector for this mesh can be:
        !
        !##### first cell
        !- connect(1)  = 0 identification flag of \(1^\circ\) vertex of first cell
        !- connect(2)  = 1 identification flag of \(2^\circ\) vertex of first cell
        !- connect(3)  = 2 identification flag of \(3^\circ\) vertex of first cell
        !- connect(4)  = 3 identification flag of \(4^\circ\) vertex of first cell
        !- connect(5)  = 4 identification flag of \(5^\circ\) vertex of first cell
        !- connect(6)  = 5 identification flag of \(6^\circ\) vertex of first cell
        !- connect(7)  = 6 identification flag of \(7^\circ\) vertex of first cell
        !- connect(8)  = 7 identification flag of \(8^\circ\) vertex of first cell
        !
        !##### second cell
        !- connect(9 ) = 0 identification flag of \(1^\circ\) vertex of second cell
        !- connect(10) = 1 identification flag of \(2^\circ\) vertex of second cell
        !- connect(11) = 2 identification flag of \(3^\circ\) vertex of second cell
        !- connect(12) = 3 identification flag of \(4^\circ\) vertex of second cell
        !- connect(13) = 8 identification flag of \(5^\circ\) vertex of second cell
        !
        ! Therefore this connectivity vector convention is more simple than the legacy convention, now we must create also the
        ! *offset* vector that contains the data now missing in the *connect* vector. The offset
        ! vector for this mesh can be:
        !
        !##### first cell
        !- offset(1) = 8  => summ of nodes of \(1^\circ\) cell
        !
        !##### second cell
        !- offset(2) = 13 => summ of nodes of \(1^\circ\) and \(2^\circ\) cells
        !
        ! The value of every cell-offset can be calculated by the following equation: \(offset_c=\sum\limits_{i=1}^{c}{nvertex_i}\)
        ! where \(offset_c\) is the value of \(c^{th}\) cell and \(nvertex_i\) is the number of vertices of \(i^{th}\) cell.
        ! The function VTK_CON_XML does not calculate the connectivity and offset vectors: it writes the connectivity and offset
        ! vectors conforming the VTK-XML standard, but does not calculate them.
        ! The vector variable *cell\_type* must conform the VTK-XML standard (see the file VTK-Standard at the
        ! Kitware homepage) that is the same of the legacy standard. It contains the
        ! *type* of each cells. For the above example this vector is:
        !
        !##### first cell
        !- cell\_type(1) = 12 hexahedron type of first cell
        !
        !##### second cell
        !- cell\_type(2) = 14 pyramid type of second cell

        call self%write_start_tag(name='Cells')
        error = self%write_dataarray(data_name='connectivity', x=connectivity)
        error = self%write_dataarray(data_name='offsets', x=offset)
        error = self%write_dataarray(data_name='types', x=cell_type)
        call self%write_end_tag(name='Cells')
    end function write_connectivity

    ! write_parallel methods
    !| Write a block (open) container.
    function write_parallel_open_block(self, name) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in), optional :: name
            !! Block name.
        integer(int32) :: error
            !! Error status.
        type(string) :: buffer
            !! Buffer string.

        self%vtm_block = self%vtm_block + 1
        if (present(name)) then
            buffer = 'index="'//trim(str((self%vtm_block(1) + self%vtm_block(2)), .true.))//'" name="'//trim(adjustl(name))//'"'
        else
            buffer = 'index="'//trim(str((self%vtm_block(1) + self%vtm_block(2)), .true.))//'"'
        end if
        call self%write_start_tag(name='Block', attributes=buffer%chars())
        error = self%error
    end function write_parallel_open_block

    !| Close a block container.
    function write_parallel_close_block(self) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32) :: error
            !! Error status.

        self%vtm_block(2) = -1
        call self%write_end_tag(name='Block')
        error = self%error
    end function write_parallel_close_block

    !| Write parallel (partitioned) VTK-XML dataarray info.
    function write_parallel_dataarray(self, data_name, data_type, number_of_components) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: data_name
            !! Data name.
        character(*), intent(in) :: data_type
            !! Type of dataarray.
        integer(int32), intent(in), optional :: number_of_components
            !! Number of dataarray components.
        integer(int32) :: error
            !! Error status.
        type(string) :: buffer
            !! Buffer string.

        if (present(number_of_components)) then
            buffer = 'type="'//trim(adjustl(data_type))//'" Name="'//trim(adjustl(data_name))// &
                     '" NumberOfComponents="'//trim(str(number_of_components, .true.))//'"'
        else
            buffer = 'type="'//trim(adjustl(data_type))//'" Name="'//trim(adjustl(data_name))//'"'
        end if
        call self%write_self_closing_tag(name='PDataArray', attributes=buffer%chars())
        error = self%error
    end function write_parallel_dataarray

    !| Write parallel (partitioned) VTK-XML geo source file.
    function write_parallel_geo(self, source, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: source
            !! Source file name containing the piece data.
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
        integer(int32) :: error
            !! Error status.
        type(string) :: buffer
            !! Buffer string.

        select case (self%topology%chars())
        case ('PRectilinearGrid', 'PStructuredGrid')
            buffer = 'Extent="'// &
                     trim(str(n=nx1))//' '//trim(str(n=nx2))//' '// &
                     trim(str(n=ny1))//' '//trim(str(n=ny2))//' '// &
                     trim(str(n=nz1))//' '//trim(str(n=nz2))//'" Source="'//trim(adjustl(source))//'"'
        case ('PUnstructuredGrid')
            buffer = 'Source="'//trim(adjustl(source))//'"'
        end select
        call self%write_self_closing_tag(name='Piece', attributes=buffer%chars())
        error = self%error
    end function write_parallel_geo

    !| Write single file that belong to the current block.
    function write_parallel_block_file(self, file_index, filename, name) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        integer(int32), intent(in) :: file_index
            !! Index of file in the list.
        character(*), intent(in) :: filename
            !! Wrapped file names.
        character(*), intent(in), optional :: name
            !! Names attributed to wrapped file.
        integer(int32) :: error
            !! Error status.

        if (present(name)) then
            call self%write_self_closing_tag(name='DataSet', &
                                             attributes='index="'//trim(str(file_index, .true.))// &
                                             '" file="'//trim(adjustl(filename))// &
                                             '" name="'//trim(adjustl(name))//'"')
        else
            call self%write_self_closing_tag(name='DataSet', &
                                             attributes='index="'//trim(str(file_index, .true.))// &
                                             '" file="'//trim(adjustl(filename))//'"')
        end if
        error = self%error
    end function write_parallel_block_file

    !| Write list of files that belong to the current block (list passed as rank 1 array).
    function write_parallel_block_files_array(self, filenames, names) result(error)
        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: filenames(:)
            !! List of VTK-XML wrapped file names.
        character(*), intent(in), optional :: names(:)
            !! List names attributed to wrapped files.
        integer(int32) :: error
            !! Error status.
        integer(int32) :: f
            !! File counter.

        !|
        !#### Example of usage: 3 files blocks
        !```fortran
        ! error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'])
        !```
        !
        !#### Example of usage: 3 files blocks with custom name
        !```fortran
        ! error = vtm%write_files_list_of_block(filenames=['file_1.vts','file_2.vts','file_3.vtu'],&
        !                                       names=['block-bar','block-foo','block-baz'])
        !```

        if (present(names)) then
            if (size(names, dim=1) == size(filenames, dim=1)) then
                do f = 1, size(filenames, dim=1)
                    call self%write_self_closing_tag(name='DataSet', &
                                                     attributes='index="'//trim(str(f - 1, .true.))// &
                                                     '" file="'//trim(adjustl(filenames(f)))// &
                                                     '" name="'//trim(adjustl(names(f)))//'"')
                end do
            end if
        else
            do f = 1, size(filenames, dim=1)
                call self%write_self_closing_tag(name='DataSet', &
                                                 attributes='index="'//trim(str(f - 1, .true.))// &
                                                 '" file="'//trim(adjustl(filenames(f)))//'"')
            end do
        end if
        error = self%error
    end function write_parallel_block_files_array

    !| Write list of files that belong to the current block (list passed as single string).
    function write_parallel_block_files_string(self, filenames, names, delimiter) result(error)

        class(xml_writer_abstract), intent(inout) :: self
            !! Writer.
        character(*), intent(in) :: filenames
            !! List of VTK-XML wrapped file names.
        character(*), intent(in), optional :: names
            !! List names attributed to wrapped files.
        character(*), intent(in), optional :: delimiter
            !! Delimiter character.
        integer(int32) :: error
            !! Error status.
        type(string), allocatable :: filenames_(:)
            !! List of VTK-XML wrapped file names.
        type(string), allocatable :: names_(:)
            !! List names attributed to wrapped files.
        type(string) :: delimiter_
            !! Delimiter character.
        type(string) :: buffer
            !! A string buffer.
        integer(int32) :: f
            !! File counter.

        !|
        !#### Example of usage: 3 files blocks
        !```fortran
        ! error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu')
        !```
        !
        !#### Example of usage: 3 files blocks with custom name
        !```fortran
        ! error = vtm%write_files_list_of_block(filenames='file_1.vts file_2.vts file_3.vtu',&
        !                                       names='block-bar block-foo block-baz')
        !```

        delimiter_ = ' '; if (present(delimiter)) delimiter_ = delimiter
        buffer = filenames
        call buffer%split(tokens=filenames_, sep=delimiter_%chars())
        if (present(names)) then
            buffer = names
            call buffer%split(tokens=names_, sep=delimiter_%chars())
            if (size(names_, dim=1) == size(filenames_, dim=1)) then
                do f = 1, size(filenames_, dim=1)
                    call self%write_self_closing_tag(name='DataSet', &
                                                     attributes='index="'//trim(str(f - 1, .true.))// &
                                                     '" file="'//trim(adjustl(filenames_(f)))// &
                                                     '" name="'//trim(adjustl(names_(f)))//'"')
                end do
            end if
        else
            do f = 1, size(filenames_, dim=1)
                call self%write_self_closing_tag(name='DataSet', &
                                                 attributes='index="'//trim(str(f - 1, .true.))// &
                                                 '" file="'//trim(adjustl(filenames_(f)))//'"')
            end do
        end if
        error = self%error
    end function write_parallel_block_files_string
end module vtk_fortran_vtk_file_xml_writer_abstract
