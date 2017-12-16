set(SOURCES
  ast.cc
  ast_exec.cc
  ast_ops.cc
  ir_construction.cc
  lexer.cc
  parser.cc
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/ast.def
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/ast.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/ast_ops.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/ast_visitor.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/ir_construction.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/keywords.def
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/lexer.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/operators.def
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/parser.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/js/tokens.def
  ${PROJECT_SOURCE_DIR}/include/jnsn/source_location.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/string_table.h
  ${PROJECT_SOURCE_DIR}/include/jnsn/util.h)

add_library(jnsn_js
  ${SOURCES})

target_link_libraries(jnsn_js PUBLIC jnsn_ir)
