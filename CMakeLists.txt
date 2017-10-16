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
  lib/ast_ops.cc
  lib/parser.cc
  include/parsing/ast.def
  include/parsing/ast.h
  include/parsing/ast_ops.h
  include/parsing/ast_visitor.h
  include/parsing/keywords.def
  include/parsing/lexer.h
  include/parsing/parser.h
  include/parsing/source_location.h
  include/parsing/tokens.def
)

include_directories(include/)

add_executable(parsing
  ${SOURCES}
)

add_subdirectory(unittest)