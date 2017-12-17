add_executable(lexer_cli
  lexer_cli.cc
  ../include/jnsn/js/keywords.def
  ../include/jnsn/js/lexer.h
  ../include/jnsn/js/tokens.def
  ../include/jnsn/string_table.h
  ../include/jnsn/source_location.h
  ../include/jnsn/util.h)
target_link_libraries(lexer_cli jnsn_js)

add_executable(parser_cli
  parser_cli.cc
  ../include/jnsn/js/ast.def
  ../include/jnsn/js/ast.h
  ../include/jnsn/js/ast_ops.h
  ../include/jnsn/js/ast_visitor.h
  ../include/jnsn/js/keywords.def
  ../include/jnsn/js/lexer.h
  ../include/jnsn/js/operators.def
  ../include/jnsn/js/parser.h
  ../include/jnsn/js/tokens.def
  ../include/jnsn/string_table.h
  ../include/jnsn/source_location.h
  ../include/jnsn/util.h)
target_link_libraries(parser_cli jnsn_js)

add_executable(ir_cli
  ir_cli.cc
  ../include/jnsn/js/ir_construction.h
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
  ../include/jnsn/string_table.h
  ../include/jnsn/source_location.h
  ../include/jnsn/util.h)
target_link_libraries(ir_cli jnsn_js)
