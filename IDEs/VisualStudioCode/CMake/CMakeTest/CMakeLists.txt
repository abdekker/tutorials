# Ensure CMake 3.10 or higher is installed
cmake_minimum_required(VERSION 3.10)
message("CMake example application for Visual Studio Code")

# Some diagnostics messages about the current CMake version
message(STATUS "  Diag: CMake version ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}.${CMAKE_PATCH_VERSION} is installed (1)")
message(STATUS "  Diag: CMake version ${CMAKE_VERSION} is installed (2)")
set(CMAKE_VERSION_REQUIRED "3.13.2")
if (${CMAKE_VERSION} VERSION_LESS ${CMAKE_VERSION_REQUIRED})
    message(STATUS "  Diag: Consider to switch to CMake ${CMAKE_VERSION_REQUIRED}")
else ()
    message(STATUS "  Diag: Your CMake version is >= ${CMAKE_VERSION_REQUIRED}")
endif()

# The project will be named "CMakeTest" (regardless of IDE)
project(CMakeTest)

# Set the build type (Debug or Release). This can be passed as a parameter with "-DCMAKE_BUILD_TYPE=XXX".
if (CMAKE_BUILD_TYPE STREQUAL "")
    message(STATUS "  Diag: Build type unspecified (setting to 'Release')")
    set(CMAKE_BUILD_TYPE Release)
else ()
    message(STATUS "  Diag: Build type specified as '${CMAKE_BUILD_TYPE}'")
endif ()

# When the project is built, place binaries in the "build" sub-directory, either Debug or Release
# Note: The $CMAKE_BUILD_TYPE variable is defined by passing a parameter to cmake
set(EXECUTABLE_OUTPUT_PATH ./${CMAKE_BUILD_TYPE})

# Define symbols based on the build type, using the "-DCMAKE_BUILD_TYPE=XXX" command-line parameter
# The symbols can then be used in C++ code with "#ifdef _DEBUG" and so on
if (${CMAKE_BUILD_TYPE} STREQUAL Debug)
    set_directory_properties(PROPERTIES COMPILE_DEFINITIONS "_DEBUG")
else ()
    set_directory_properties(PROPERTIES COMPILE_DEFINITIONS "NDEBUG")
endif ()

# Diagnostic messages
message(STATUS "  Diag: CMAKE_SYSTEM_PROCESSOR: ${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "  Diag: CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
message(STATUS "  Diag: EXECUTABLE_OUTPUT_PATH: ${EXECUTABLE_OUTPUT_PATH}")

# Define the files to be compiled into the final binary
set(SOURCE
   src/main.cpp
   src/testClass.cpp src/testClass.hpp)

# Create an executable compiled using the specified source files
add_executable(${PROJECT_NAME}.o ${SOURCE})

# Note: Below is an alternate method for including multiple files (eg. all files in the "src" folder)
    # file(
    #     GLOB_RECURSE
    #     MY_SOURCE_FILES
    #     src/*
    # )
    # add_executable(${PROJECT_NAME} ${MY_SOURCE_FILES})
