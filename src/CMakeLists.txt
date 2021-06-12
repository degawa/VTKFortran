### build penf library
add_library(penf STATIC)
target_sources(penf
    PRIVATE
    third_party/PENF/src/lib/penf_b_size.F90
    third_party/PENF/src/lib/penf_global_parameters_variables.F90
    third_party/PENF/src/lib/penf_stringify.F90
    third_party/PENF/src/lib/penf.F90
)

### build face library
add_library(face STATIC)
target_sources(face
    PRIVATE
    third_party/FACE/src/lib/face.F90
)

### build befor64 library
add_library(befor64 STATIC)
target_sources(befor64
    PRIVATE
    third_party/BeFoR64/src/lib/befor64_pack_data_m.F90
    third_party/BeFoR64/src/lib/befor64.F90
)
target_link_libraries(befor64 penf)

### build stringifor library
add_library(stringifor STATIC)
target_sources(stringifor
    PRIVATE
    third_party/StringiFor/src/lib/stringifor.F90
    third_party/StringiFor/src/lib/stringifor_string_t.F90
)
target_link_libraries(stringifor
    PUBLIC
    penf
    face
    befor64
)

### build foxy library
add_library(foxy STATIC)
target_sources(foxy
    PRIVATE
    third_party/FoXy/src/lib/foxy.f90
    third_party/FoXy/src/lib/foxy_xml_tag.F90
    third_party/FoXy/src/lib/foxy_xml_file.f90
)
target_link_libraries(foxy
    PUBLIC
    penf
    befor64
    stringifor
)

### build VTK_IO
add_library(${PROJECT_NAME} STATIC)
target_sources(${PROJECT_NAME}
    PRIVATE
    lib/vtk_fortran.f90
    lib/vtk_fortran_dataarray_encoder.f90
    lib/vtk_fortran_parameters.f90
    lib/vtk_fortran_pvtk_file.f90
    lib/vtk_fortran_vtk_file.f90
    lib/vtk_fortran_vtk_file_xml_writer_abstract.f90
    lib/vtk_fortran_vtk_file_xml_writer_appended.f90
    lib/vtk_fortran_vtk_file_xml_writer_ascii_local.f90
    lib/vtk_fortran_vtk_file_xml_writer_binary_local.f90
    lib/vtk_fortran_vtm_file.F90
)
target_link_libraries(${PROJECT_NAME}
    PUBLIC
    befor64
    stringifor
    foxy
)

add_subdirectory(tests)

#################################################################
# INSTALL VTK_IO library, penf.mod and vtk_fortran.mod
#################################################################

if(${${PROJECT_NAME}_ENABLE_INSTALL_SUBDIR})
    set(LIB_DIST lib/${COMPILER_ID})
    set(INC_DIST include/${COMPILER_ID})
    message("**A subdirectory ${COMPILER_ID} is added to the install prefix**")
else()
    set(LIB_DIST lib)
    set(INC_DIST include)
endif()

### install library
install(TARGETS ${PROJECT_NAME}
    EXPORT ${PROJECT_NAME}
    ARCHIVE DESTINATION ${LIB_DIST}
    LIBRARY DESTINATION ${INC_DIST}
)

### install modules
IF (${CMAKE_GENERATOR} MATCHES "Visual Studio*")
    install(FILES
        ${PROJECT_BINARY_DIR}/modules/$<CONFIG>/vtk_fortran.mod
        ${PROJECT_BINARY_DIR}/modules/$<CONFIG>/penf.mod
        DESTINATION ${INC_DIST}
    )
ELSE ()
    install(FILES
        ${PROJECT_BINARY_DIR}/modules/vtk_fortran.mod
        ${PROJECT_BINARY_DIR}/modules/penf.mod
        DESTINATION ${INC_DIST}
    )
ENDIF ()
