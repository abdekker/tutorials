cmake_minimum_required (VERSION 3.10)

# Tutorial for SFML
project (SpaceInvaders)

# Build type
set(CMAKE_BUILD_TYPE Debug)

# SFML is the graphics library
# When you locate the SFML libraries, specify which modules you want from { system, window, graphics, network, audio, main }
set(CMAKE_MODULE_PATH /usr/share/SFML/cmake/Modules)
find_package(SFML COMPONENTS system window graphics network audio REQUIRED)

add_executable(SpaceInvaders
	main.cpp
	catch.hpp)

target_compile_options(SpaceInvaders PUBLIC -std=c++1z -Werror -Wall -Wextra)
target_link_libraries(SpaceInvaders PUBLIC -lm ${SFML_LIBRARIES})
