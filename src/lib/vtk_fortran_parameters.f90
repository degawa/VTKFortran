!| VTK_Fortran parameters.
module vtk_fortran_parameters
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, int32

    implicit none
    private
    save
    public :: stderr
    public :: stdout
    public :: end_rec

    integer(int32), parameter :: stderr = error_unit
        !! Standard error unit.
    integer(int32), parameter :: stdout = output_unit
        !! Standard output unit.
    character(1), parameter :: end_rec = char(10)
        !! End-character for binary-record finalize.
end module vtk_fortran_parameters
