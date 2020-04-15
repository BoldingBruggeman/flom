cmake_minimum_required(VERSION 3.12)

# Create main project.
project(flom VERSION 0.1.0 LANGUAGES Fortran)

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")

# Use use position-independent code (-fPIC) everywhere if building shared libraries
if(BUILD_SHARED_LIBS)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

#add_compile_options(-pg -g -check all)
#add_compile_options(-pg -g -fcheck=all)

# FLOM has the following external submodule dependencies

add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/extern/datetime-fortran" extern/datetime)

if (NOT EXISTS "${CMAKE_CURRENT_LIST_DIR}/extern/flexout/src/CMakeLists.txt")
   message(FATAL_ERROR "Flexible output manager not found at extern/flexout. Please retrieve this submodule first by running \"git submodule update --init\" within your FLOM source directory - followed by \"git pull --recurse-submodules\.")
else()
   add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/extern/flexout" extern/flexout)
endif()

if (NOT EXISTS "${CMAKE_CURRENT_LIST_DIR}/extern/GSW-Fortran/CMakeLists.txt")
	message(FATAL_ERROR "TEOS-10 GSW-Fortran toolbox at extern/GSW-Fortran. Please retrieve this submodule first by running \"git submodule update --init\" within your FLOM source directory.")
else()
   set(GSW_BUILD_STATIC_LIBS ON)
   add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/extern/GSW-Fortran" extern/gsw)
endif()

# Various compilation options
if( NOT FLOM_USE_STATIC )
   option(FLOM_USE_STATIC "Static compilation (grid sizes are set at compile time)" OFF)
endif()

include_directories("${CMAKE_CURRENT_LIST_DIR}/include")

add_library(flom "")
target_include_directories(flom PUBLIC
	$<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules> )
target_link_libraries(flom PUBLIC output_manager field_manager netcdf gsw_static datetime)

include(src/CMakeLists.txt)
include(tests/CMakeLists.txt)

