cmake_minimum_required(VERSION 3.10)
project(gprofEx1)
message(STATUS "Tutorial on gprof (Linux GNU profiler)")
message(STATUS "")

# Build type (modify these for RELEASE builds)
#set(CMAKE_BUILD_TYPE Debug)
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG")

# In theory the following commands should compile the project as follows:
#   gcc -Wall -pg gprofExample.cpp gprofExample.hpp gprofExampleNew.cpp gprofExampleNew.hpp -o gprofEx1.o
# However when executed, the output binary does not generate the "gmon.out" profile output file
# So use this CMakeLists.txt file for development, but for profiling, execute the "gcc" command directly
add_executable(gprofEx1.o
    gprofExample.cpp gprofExample.hpp
    gprofExampleNew.cpp gprofExampleNew.hpp)
target_compile_options(gprofEx1.o PUBLIC -std=c++1z -Werror -Wall -Wextra -pg)
