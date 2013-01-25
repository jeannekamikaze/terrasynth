cmake_minimum_required (VERSION 2.6)

set (CMAKE_BUILD_TYPE Debug CACHE STRING "Build type")

project (terrasynth)

file (GLOB_RECURSE SOURCES ../src/*.cc)
add_executable (terrasynth ${SOURCES})
target_link_libraries (terrasynth OGDTd GL GLU GLEW glfw AntTweakBar)

set (CMAKE_CXX_FLAGS "--std=c++0x")
set (EXECUTABLE_OUTPUT_PATH "..")
