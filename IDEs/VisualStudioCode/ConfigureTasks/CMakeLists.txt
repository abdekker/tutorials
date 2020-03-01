cmake_minimum_required(VERSION 3.7)
message(STATUS "Hello from CMake: 1st tutorial")

message(STATUS "CMAKE_SYSTEM_PROCESSOR: ${CMAKE_SYSTEM_PROCESSOR}")
project(ConfigureTasks VERSION 0.1.0)
add_executable(ConfigureTasks configureTasks.cpp)

set(VAR1 1)
message(STATUS "VAR1 = ${VAR1}")
MATH(EXPR VAR1 "${VAR1}+1")
message(STATUS "VAR1 = ${VAR1}")
