cmake_minimum_required(VERSION 3.6)

##
## PROJECT
## name and version
##
project(parsing VERSION 1.0.0 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

set(SOURCES
  lib/main.cc
  lib/lexer.cc
  lib/ast.cc
  lib/parser.cc
  include/parsing/lexer.h
  include/parsing/source_location.h
  include/parsing/string_table.h
)

include_directories(include/)

add_executable(parsing
  ${SOURCES}
)

add_subdirectory(unittest)