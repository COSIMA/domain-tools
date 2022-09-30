cmake_minimum_required(VERSION 3.18)

project(domain-tools VERSION 0.1.0 LANGUAGES Fortran)


# Find NetCDF
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(NetCDF COMPONENTS Fortran REQUIRED)


# Executables
add_executable(deseas src/deseas.f90)
target_link_libraries(deseas PRIVATE NetCDF::NetCDF_Fortran)

add_executable(check_nonadvective_mosaic src/check_nonadvective_mosaic.f90)
target_link_libraries(check_nonadvective_mosaic PRIVATE NetCDF::NetCDF_Fortran)

add_executable(fix_nonadvective_mosaic src/fix_nonadvective_mosaic.f90)
target_link_libraries(fix_nonadvective_mosaic PRIVATE NetCDF::NetCDF_Fortran)

add_executable(gen_topo src/kdtree2.f90 src/gen_topo.f90)
target_link_libraries(gen_topo PRIVATE NetCDF::NetCDF_Fortran)

add_executable(min_max_depth src/min_max_depth.f90)
target_link_libraries(min_max_depth PRIVATE NetCDF::NetCDF_Fortran)


# Installation
include(GNUInstallDirs)

install(TARGETS deseas check_nonadvective_mosaic fix_nonadvective_mosaic gen_topo min_max_depth
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
