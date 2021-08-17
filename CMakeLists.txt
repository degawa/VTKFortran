#################################################################
# HEADER
#################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 3.15)

PROJECT(vtkfortran Fortran)

enable_testing()

# Note the agreement between the first number of the triplet
# VERSION and the SOVERSION
SET(${PROJECT_NAME}_VERSION 0.0.1)
SET(${PROJECT_NAME}_SOVERSION 0)

SET(LIB ${PROJECT_NAME})

#################################################################
# DEFINE PATHS
#################################################################

SET(SRC_PATH ${CMAKE_SOURCE_DIR}/src)
SET(LIB_SRC_PATH ${SRC_PATH}/lib)

SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMake/Modules/")


#################################################################
# BUILD PATHS
#################################################################

SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
INCLUDE_DIRECTORIES(${CMAKE_Fortran_MODULE_DIRECTORY})

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU OR Fortran_COMPILER_NAME MATCHES "gfortran*")
    set(COMPILER_ID gnu)
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel OR Fortran_COMPILER_NAME MATCHES "ifort*")
    set(COMPILER_ID intel)
endif()
OPTION(${PROJECT_NAME}_ENABLE_INSTALL_SUBDIR "Add compiler ID as a subdirectory to the install prefix" OFF)

#################################################################
# CONFIGURATION TYPES & BUILD MODE
#################################################################

SET(CMAKE_CONFIGURATION_TYPES DEBUG RELEASE)
IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: NONE DEBUG RELEASE"
      FORCE)

  SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS NONE DEBUG RELEASE)
ENDIF(NOT CMAKE_BUILD_TYPE)


#################################################################
# Setting up options
#################################################################

GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

OPTION(${PROJECT_NAME}_ENABLE_F03STD "To force strict conformance to the Fortran 2003 standard" OFF)
OPTION(${PROJECT_NAME}_ENABLE_F08STD "To force strict conformance to the Fortran 2008 standard" OFF)
OPTION(${PROJECT_NAME}_ENABLE_F18STD "To force strict conformance to the Fortran 2018 standard" OFF)
OPTION(${PROJECT_NAME}_ENABLE_OPTIMIZE "To activate optimization flags" OFF)
OPTION(${PROJECT_NAME}_ENABLE_OPENMP "To activate the OpenMP extensions for Fortran" OFF)
OPTION(${PROJECT_NAME}_ENABLE_MPI "To compile MPI tests and link against MPI library" OFF)
OPTION(${PROJECT_NAME}_ENABLE_R16P "To define a real precission type with 128 bits" OFF)
#OPTION(${PROJECT_NAME}_ENABLE_HUGE "To use 64 bit instead 32 bit integer as a bit counter" OFF)

message("Conformance to the Fortran 2003 standard ${${PROJECT_NAME}_ENABLE_F03STD}")
message("Conformance to the Fortran 2008 standard ${${PROJECT_NAME}_ENABLE_F08STD}")
message("Conformance to the Fortran 2018 standard ${${PROJECT_NAME}_ENABLE_F18STD}")

IF (${${PROJECT_NAME}_ENABLE_F03STD})
    IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
        SET(STD_FLAGS " -std=f2003 -fall-intrinsics")
    ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
        IF (CMAKE_SYSTEM_NAME STREQUAL "Windows")
            SET(STD_FLAGS " /stand:f03")
        ELSEIF (CMAKE_SYSTEM_NAME STREQUAL "Linux")
            SET(STD_FLAGS " -std03")
        ENDIF ()
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_F08STD})
    IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
        SET(STD_FLAGS " -std=f2008 -fall-intrinsics")
    ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
        IF (CMAKE_SYSTEM_NAME STREQUAL "Windows")
            SET(STD_FLAGS " /stand:f08")
        ELSEIF (CMAKE_SYSTEM_NAME STREQUAL "Linux")
            SET(STD_FLAGS " -stand f08")
        ENDIF ()
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_F18STD})
    IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
        SET(STD_FLAGS " -std=f2018 -fall-intrinsics")
    ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
        if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 19.0)
            set(STD_FLAG_KEY "f15")
        else()
            set(STD_FLAG_KEY "f18")
        endif()
        IF (CMAKE_SYSTEM_NAME STREQUAL "Windows")
            SET(STD_FLAGS " /stand:${STD_FLAG_KEY}")
        ELSEIF (CMAKE_SYSTEM_NAME STREQUAL "Linux")
            SET(STD_FLAGS " -stand ${STD_FLAG_KEY}")
        ENDIF ()
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_OPTIMIZE})
    IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
        SET(EXTRA_FLAGS "${EXTRA_FLAGS}  -O3")
    ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
        IF (CMAKE_SYSTEM_NAME STREQUAL "Windows")
            SET(EXTRA_FLAGS "${EXTRA_FLAGS}  /O3 /Qipo /inline:all /Qipo-jobs:4 /Qopt-report-phase:vec")
        ELSEIF (CMAKE_SYSTEM_NAME STREQUAL "Linux")
            SET(EXTRA_FLAGS "${EXTRA_FLAGS}  -O3 -ipo -inline all -ipo-jobs4 -vec-report1")
        ENDIF ()
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_OPENMP})
    FIND_PACKAGE( OpenMP )
    IF (${OPENMP_FOUND})
        SET(EXTRA_FLAGS ${OpenMP_Fortran_FLAGS})
        ADD_DEFINITIONS("-DOPENMP")
    ELSE ()
        OPTION(${PROJECT_NAME}_ENABLE_OPENMP "To activate the OpenMP extensions for Fortran" OFF)
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_MPI})
    FIND_PACKAGE(MPI)
    IF(${MPI_FOUND})
        INCLUDE(CMakeForceCompiler)
        CMAKE_FORCE_Fortran_COMPILER(${MPI_Fortran_COMPILER} ${CMAKE_Fortran_COMPILER_ID})
        ADD_DEFINITIONS(${MPI_Fortran_COMPILE_FLAGS})
        INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATH})
        LINK_DIRECTORIES(${MPI_Fortran_LIBRARIES})
        ADD_DEFINITIONS("-DMPI2")
    ELSE ()
        OPTION(${PROJECT_NAME}_ENABLE_MPI "To compile MPI tests and link against MPI library" OFF)
    ENDIF ()
ENDIF ()

IF (${${PROJECT_NAME}_ENABLE_R16P})
    ADD_DEFINITIONS("-Dr16p")
ENDIF ()

#IF (${${PROJECT_NAME}_ENABLE_HUGE})
#    ADD_DEFINITIONS("-DHUGE")
#ENDIF ()


#################################################################
# FFLAGS depend on the compiler and the build type
#################################################################


IF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES "gfortran*")
  # gfortran
  set(FORTRAN_FLAGS "-cpp -frealloc-lhs ")
  set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} ${STD_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_RELEASE "${EXTRA_FLAGS} " CACHE STRING "" FORCE)
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g3 -Warray-bounds -Wcharacter-truncation -Wline-truncation -Wimplicit-interface -Wimplicit-procedure -Wunderflow -Wuninitialized -fcheck=all -fmodule-private -ffree-line-length-132 -fimplicit-none -fbacktrace -fdump-core -finit-real=nan " CACHE STRING "" FORCE)
ELSEIF (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME MATCHES "ifort*")
  # ifort
  IF (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(FORTRAN_FLAGS "/fpp /assume=realloc_lhs /nologo")
    set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} ${STD_FLAGS} " CACHE STRING "" FORCE)
    set (CMAKE_Fortran_FLAGS_RELEASE "${EXTRA_FLAGS} " CACHE STRING "" FORCE)
    set (CMAKE_Fortran_FLAGS_DEBUG   "/Od /debug:all /check:all /warn:all /Qdiag-disable:5462 /extend-source:132 /traceback /gen-interfaces" CACHE STRING "" FORCE)
  ELSE ()
    set(FORTRAN_FLAGS "-cpp -assume realloc_lhs ")
    set (CMAKE_Fortran_FLAGS "${FORTRAN_FLAGS} ${STD_FLAGS} " CACHE STRING "" FORCE)
    set (CMAKE_Fortran_FLAGS_RELEASE "${EXTRA_FLAGS} " CACHE STRING "" FORCE)
    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -debug all -check all -warn all -extend-source 132 -traceback -gen-interfaces " CACHE STRING "" FORCE)
  ENDIF ()
#-fpe-all=0 -fp-stack-check -fstack-protector-all -ftrapuv -no-ftz -std03
ELSE ()
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
ENDIF ()

message("Fortra_FLAGS: ${FORTRAN_FLAGS}")
message("CMAKE_Fortran_FLAGS: ${CMAKE_Fortran_FLAGS}")
message("CMAKE_Fortran_FLAGS_RELEASE: ${CMAKE_Fortran_FLAGS_RELEASE}")
message("CMAKE_Fortran_FLAGS_DEBUG: ${CMAKE_Fortran_FLAGS_DEBUG}")


#################################################################
# ADD SOURCES
#################################################################

add_subdirectory(src)
