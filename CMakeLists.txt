cmake_minimum_required(VERSION 3.18)

project(domain-tools VERSION 0.1.0 LANGUAGES Fortran)


# Find NetCDF
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(NetCDF COMPONENTS Fortran REQUIRED)


# Library targets
add_library(M_CLI2
  external/M_CLI2/src/M_CLI2.F90
)

add_library(topography
  src/utils.f90
  src/topography.f90
  src/gen_topo.f90
  src/kdtree2.f90
)
target_link_libraries(topography PUBLIC NetCDF::NetCDF_Fortran
                                 PRIVATE M_CLI2)

# Executables
add_executable(topogtools src/topogtools.f90)
target_link_libraries(topogtools PRIVATE topography M_CLI2)

add_executable(float_vgrid src/float_vgrid.f90)
target_link_libraries(float_vgrid PRIVATE topography M_CLI2)

# Installation
include(GNUInstallDirs)

install(TARGETS topogtools float_vgrid
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
