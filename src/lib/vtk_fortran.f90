!| VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
module vtk_fortran
    use, intrinsic :: iso_fortran_env
    use vtk_fortran_pvtk_file, only: pvtk_file
    use vtk_fortran_vtk_file, only: vtk_file
    use vtk_fortran_vtm_file, only: vtm_file

    implicit none
    private
    public :: pvtk_file
    public :: vtk_file
    public :: vtm_file
    public :: write_xml_volatile

contains
    function write_xml_volatile(xml_volatile, filename) result(error)
        use newunit
        !! Write the volatile file into a real file.
        !! This is what a master process should do into a parallel scenario where it being the only process allowed to access to
        !! filesystem: slave processes create XML volatile file econded into a characters string and master process collects and writes
        !! them by means of `write_xml_volatile`.
        character(*), intent(in) :: xml_volatile
            !! XML volatile file.
        character(*), intent(in) :: filename
            !! XML file name.
        integer(int32) :: error
            !! Status error.
        integer(int32) :: xml_unit
            !! XML file unit.

        xml_unit = get_newunit_number()
        open (unit=xml_unit, &
              file=trim(adjustl(filename)), &
              form='UNFORMATTED', &
              access='STREAM', &
              action='WRITE', &
              status='REPLACE', &
              iostat=error)
        write (unit=xml_unit, iostat=error) xml_volatile
    end function write_xml_volatile
end module vtk_fortran
