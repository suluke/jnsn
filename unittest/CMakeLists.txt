configure_file(
  ${PROJECT_SOURCE_DIR}/cmake/gtest-download.cmake.in
  ${PROJECT_BINARY_DIR}/googletest/CMakeLists.txt
  COPYONLY
  )
execute_process(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/googletest")
execute_process(COMMAND "${CMAKE_COMMAND}" --build .
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/googletest")

# Prevent GoogleTest from overriding our compiler/linker options
# when building with Visual Studio
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# Add googletest directly to our build. This adds
# the following targets: gtest, gtest_main, gmock
# and gmock_main
add_subdirectory("${PROJECT_BINARY_DIR}/googletest/src"
                 "${PROJECT_BINARY_DIR}/googletest/build")

set(UNITTEST_RUNS)
macro(add_unittest test_name)
  add_executable(${test_name} EXCLUDE_FROM_ALL ${ARGN})
  target_link_libraries(${test_name} gtest_main gtest)
  add_custom_target(run_${test_name} DEPENDS ${test_name} COMMAND ${test_name} --gtest_color=true)
  list(APPEND UNITTEST_RUNS run_${test_name})
endmacro()

add_unittest(lexer_test
  lexer_test.cc
  ../lib/lexer.cc
  ../include/parsing/lexer.h
  ../include/parsing/tokens.def
  ../include/parsing/keywords.def
)
add_unittest(ast_test
  ast_test.cc
  ../lib/ast.cc
  ../lib/ast_ops.cc
  ../include/parsing/ast.h
  ../include/parsing/ast.def
  ../include/parsing/ast_visitor.h
)
add_unittest(ast_ops_test
  ast_ops_test.cc
  ../lib/ast.cc
  ../lib/ast_ops.cc
  ../include/parsing/ast.h
  ../include/parsing/ast.def
  ../include/parsing/ast_visitor.h
  ../include/parsing/ast_ops.h
)
add_unittest(parser_test
  parser_test.cc
  gtest_utils.h
  ../lib/parser.cc
  ../lib/ast.cc
  ../lib/ast_ops.cc
  ../lib/lexer.cc
  ../include/parsing/parser.h
)

add_custom_target(unittests
  DEPENDS ${UNITTEST_RUNS}
)
message(STATUS "Unittests: ${UNITTEST_RUNS}")