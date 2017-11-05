cmake_minimum_required(VERSION 3.6)

##
## PROJECT
## name and version
##
project(parsing VERSION 1.0.0 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)
add_definitions(-Wall -pedantic -fno-rtti -fno-exceptions)

include_directories(include/)

add_subdirectory(lib)
add_subdirectory(bin)
add_subdirectory(unittest)
