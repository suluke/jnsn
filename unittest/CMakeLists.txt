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

add_custom_target(unittests)

function(add_unittest test_name)
  add_executable(${test_name} EXCLUDE_FROM_ALL ${ARGN})
  target_link_libraries(lexer_test gtest_main gtest)
  add_dependencies(unittests ${test_name})
endfunction()

add_unittest(lexer_test
  lexer_test.cc
  ../lib/lexer.cc
)