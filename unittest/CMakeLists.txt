configure_file(
  ${PROJECT_SOURCE_DIR}/cmake/gtest-download.cmake.in
  ${PROJECT_BINARY_DIR}/googletest/CMakeLists.txt
  COPYONLY
  )

set(gtest_configure_cmd "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
                        -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}")
if (BUILD_SHARED_LIBS)
  list(APPEND gtest_configure_cmd -DBUILD_SHARED_LIBS=ON)
endif()
list(APPEND gtest_configure_cmd WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/googletest")

execute_process(COMMAND ${gtest_configure_cmd})
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
  target_link_libraries(${test_name} gtest_main gtest jnsn)
  add_custom_target(run_${test_name} DEPENDS ${test_name} COMMAND ${test_name} --gtest_color=true)
  list(APPEND UNITTEST_RUNS run_${test_name})
endmacro()

add_unittest(lexer_test
  lexer_test.cc
  ../include/jnsn/lexer.h
  ../include/jnsn/string_table.h
  ../include/jnsn/tokens.def
  ../include/jnsn/keywords.def
)
add_unittest(ast_test
  ast_test.cc
  ../include/jnsn/ast.h
  ../include/jnsn/ast.def
  ../include/jnsn/ast_visitor.h
)
add_unittest(ast_ops_test
  ast_ops_test.cc
  ../include/jnsn/ast.h
  ../include/jnsn/ast.def
  ../include/jnsn/ast_visitor.h
  ../include/jnsn/ast_ops.h
)
add_unittest(parser_test
  parser_test.cc
  gtest_utils.h
  ../include/jnsn/parser.h
)
add_unittest(ir_test
  ir_test.cc
  ../include/jnsn/ir/instructions.def
  ../include/jnsn/ir/instructions.h
  ../include/jnsn/ir/intrinsics.def
  ../include/jnsn/ir/ir.h
  ../include/jnsn/ir/ir_builder.h
  ../include/jnsn/ir/ir_context.h
  ../include/jnsn/ir/module.h
  ../include/jnsn/ir/types.def
  ../include/jnsn/ir/types.h
  ../include/jnsn/ir/value.h
)
add_unittest(exec_test
  exec_test.cc
  ../include/jnsn/ast_exec.h
)

add_custom_target(unittests
  DEPENDS ${UNITTEST_RUNS}
)
message(STATUS "Unittests: ${UNITTEST_RUNS}")
