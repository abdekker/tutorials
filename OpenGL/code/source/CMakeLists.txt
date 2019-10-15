cmake_minimum_required(VERSION 3.5)

# Change the following line for C++14 or C++17
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(RUNTIME_OUTPUT_DIRECTORY ../build)

find_package(OpenGL REQUIRED)
message("OPENGL_INCLUDE_DIRS=${OPENGL_INCLUDE_DIRS}")
message("OPENGL_LIBRARIES=${OPENGL_LIBRARIES}")

find_package(GLUT REQUIRED)
message("GLUT_INCLUDE_DIRS=${GLUT_INCLUDE_DIRS}")
message("GLUT_LIBRARIES=${GLUT_LIBRARIES}")

set(thisEXE demoOpenGL)
add_executable(${thisEXE} testOpenGL.cpp)

# include not needed as somehow done automatically
# include_directories(${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIRS})
# but the shared libraries of both need to be added
target_link_libraries(${thisEXE} ${OPENGL_LIBRARIES} ${GLUT_LIBRARY})
