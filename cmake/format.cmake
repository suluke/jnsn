# Check and update source code formatting
function(gen_fmt_targets PROJECT_NAME)
  file(GLOB_RECURSE CHECK_FORMAT_SOURCES
    ${PROJECT_SOURCE_DIR}/bin/*.h ${PROJECT_SOURCE_DIR}/bin/*.cc
    ${PROJECT_SOURCE_DIR}/include/*.h
    ${PROJECT_SOURCE_DIR}/lib/*.h ${PROJECT_SOURCE_DIR}/lib/*.cc
    ${PROJECT_SOURCE_DIR}/unittest/*.h ${PROJECT_SOURCE_DIR}/unittest/*.cc
    )

  set(check_format_depends)
  set(update_format_depends)
  set(i 0)
  foreach (file IN LISTS CHECK_FORMAT_SOURCES)
    add_custom_command(OUTPUT ${PROJECT_NAME}-check-format${i}
      COMMAND clang-format -sort-includes -style=llvm ${file} | diff -u ${file} -
      VERBATIM
      COMMENT "Checking format of ${file}..."
    )
    list(APPEND check_format_depends "${PROJECT_NAME}-check-format${i}")

    add_custom_command(OUTPUT ${PROJECT_NAME}-update-format${i}
      COMMAND clang-format -sort-includes -i -style=llvm ${file}
      VERBATIM
      COMMENT "Updating format of ${file}..."
    )
    list(APPEND update_format_depends "${PROJECT_NAME}-update-format${i}")

    math(EXPR i ${i}+1)
  endforeach ()

  add_custom_target(${PROJECT_NAME}-check-format DEPENDS ${check_format_depends})
  set_target_properties(${PROJECT_NAME}-check-format PROPERTIES FOLDER "${PROJECT_NAME}")

  add_custom_target(${PROJECT_NAME}-update-format DEPENDS ${update_format_depends})
  set_target_properties(${PROJECT_NAME}-update-format PROPERTIES FOLDER "${PROJECT_NAME}")
endfunction()
