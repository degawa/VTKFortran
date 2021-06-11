macro(ADDTEST name)
    add_executable(test_vtk_fortran_${name} vtk_fortran_${name}.f90)
    target_link_libraries(test_vtk_fortran_${name} ${PROJECT_NAME})

    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_vtk_fortran_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTEST)

ADDTEST(use_module_basic)
ADDTEST(write_pvts)
ADDTEST(write_volatile)
ADDTEST(write_vtm)
ADDTEST(write_vtr)
ADDTEST(write_vts)
ADDTEST(write_vtu)
