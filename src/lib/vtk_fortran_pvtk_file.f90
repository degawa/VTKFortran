!| Parallel (partioned) VTK file class.
module vtk_fortran_pvtk_file
    use, intrinsic :: iso_fortran_env
    use befor64
    use penf, only: penf_init, is_initialized
    use vtk_fortran_vtk_file_xml_writer_abstract
    use vtk_fortran_vtk_file_xml_writer_ascii_local

    implicit none
    private
    public :: pvtk_file

    type :: pvtk_file
        !! VTK parallel (partioned) file class.
        private
        class(xml_writer_abstract), allocatable, public :: xml_writer
            !! XML writer.
    contains
        procedure, pass(self) :: initialize
            !! Initialize file.
        procedure, pass(self) :: finalize
            !! Finalize file.
    end type pvtk_file
contains
    !| Initialize file (writer).
    function initialize(self, filename, mesh_topology, mesh_kind, nx1, nx2, ny1, ny2, nz1, nz2) result(error)
        class(pvtk_file), intent(inout) :: self
            !! VTK file.
        character(*), intent(in) :: filename
            !! File name.
        character(*), intent(in) :: mesh_topology
            !! Mesh topology.
        character(*), intent(in) :: mesh_kind
            !! Kind of mesh data: Float64, Float32, ecc.
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

        !|
        ! @note This function must be the first to be called.
        !
        !### Supported topologies are:
        !
        !- PRectilinearGrid;
        !- PStructuredGrid;
        !- PUnstructuredGrid.
        !
        !### Example of usage
        !
        !```fortran
        ! type(pvtk_file) :: pvtk
        ! integer(int32)    :: nx1, nx2, ny1, ny2, nz1, nz2
        ! ...
        ! error = pvtk%initialize('XML_RECT_BINARY.pvtr','PRectilinearGrid',nx1=nx1,nx2=nx2,ny1=ny1,ny2=ny2,nz1=nz1,nz2=nz2)
        ! ...
        !```
        ! @note The file extension is necessary in the file name. The XML standard has different extensions for each
        ! different topologies (e.g. *pvtr* for rectilinear topology). See the VTK-standard file for more information.

        if (.not. is_initialized) call penf_init
        if (.not. is_b64_initialized) call b64_init
        if (allocated(self%xml_writer)) deallocate (self%xml_writer)
        allocate (xml_writer_ascii_local :: self%xml_writer)
        error = self%xml_writer%initialize(format='ascii', filename=filename, mesh_topology=mesh_topology, &
                                           nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
    end function initialize

    !| Finalize file (writer).
    function finalize(self) result(error)
        class(pvtk_file), intent(inout) :: self
            !! VTK file.
        integer(int32) :: error
            !! Error status.

        error = 1
        if (allocated(self%xml_writer)) error = self%xml_writer%finalize()
    end function finalize
end module vtk_fortran_pvtk_file
