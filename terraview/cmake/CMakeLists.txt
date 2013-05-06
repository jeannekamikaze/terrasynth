cmake_minimum_required (VERSION 2.6)

set (CMAKE_BUILD_TYPE Debug CACHE STRING "Build type")

project (terraview)

file (GLOB_RECURSE SOURCES ../src/*.cc)
add_executable (terraview ${SOURCES})
target_link_libraries (terraview OGDTpd GL GLU GLEW glfw)

set (CMAKE_CXX_FLAGS "--std=c++0x")
set (EXECUTABLE_OUTPUT_PATH "..")
