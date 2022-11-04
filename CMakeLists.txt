cmake_minimum_required(VERSION 3.18)

project(domain-tools VERSION 0.1.0 LANGUAGES Fortran)


# Find NetCDF
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(NetCDF COMPONENTS Fortran REQUIRED)

# Library targets
add_library(utils
  src/utils.f90
)
target_link_libraries(utils PRIVATE NetCDF::NetCDF_Fortran)

add_library(M_CLI2
  external/M_CLI2/src/M_CLI2.F90
)

# Executables
add_executable(deseas src/deseas.f90)
target_link_libraries(deseas PRIVATE NetCDF::NetCDF_Fortran utils M_CLI2)

add_executable(check_nonadvective src/check_nonadvective.f90)
target_link_libraries(check_nonadvective PRIVATE NetCDF::NetCDF_Fortran utils)

add_executable(fix_nonadvective src/fix_nonadvective.f90)
target_link_libraries(fix_nonadvective PRIVATE NetCDF::NetCDF_Fortran utils)

add_executable(gen_topo src/kdtree2.f90 src/gen_topo.f90)
target_link_libraries(gen_topo PRIVATE NetCDF::NetCDF_Fortran utils M_CLI2)

add_executable(min_max_depth src/min_max_depth.f90)
target_link_libraries(min_max_depth PRIVATE NetCDF::NetCDF_Fortran utils)


# Installation
include(GNUInstallDirs)

install(TARGETS deseas check_nonadvective fix_nonadvective gen_topo min_max_depth
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
