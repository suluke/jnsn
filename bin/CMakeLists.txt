add_executable(lexer_cli
  lexer_cli.cc
  ../include/jnsn/keywords.def
  ../include/jnsn/lexer.h
  ../include/jnsn/string_table.h
  ../include/jnsn/source_location.h
  ../include/jnsn/tokens.def
  ../include/jnsn/util.h)
target_link_libraries(lexer_cli jnsn)

add_executable(parser_cli
  parser_cli.cc
  ../include/jnsn/ast.def
  ../include/jnsn/ast.h
  ../include/jnsn/ast_ops.h
  ../include/jnsn/ast_visitor.h
  ../include/jnsn/keywords.def
  ../include/jnsn/lexer.h
  ../include/jnsn/operators.def
  ../include/jnsn/parser.h
  ../include/jnsn/string_table.h
  ../include/jnsn/source_location.h
  ../include/jnsn/tokens.def
  ../include/jnsn/util.h)
target_link_libraries(parser_cli jnsn)

add_executable(exec_cli
  exec_cli.cc
  ../include/jnsn/ast.def
  ../include/jnsn/ast.h
  ../include/jnsn/ast_exec.h
  ../include/jnsn/ast_ops.h
  ../include/jnsn/ast_visitor.h
  ../include/jnsn/keywords.def
  ../include/jnsn/lexer.h
  ../include/jnsn/operators.def
  ../include/jnsn/parser.h
  ../include/jnsn/string_table.h
  ../include/jnsn/source_location.h
  ../include/jnsn/tokens.def
  ../include/jnsn/util.h)
target_link_libraries(exec_cli jnsn)

add_executable(ir_cli
  ir_cli.cc
  ../include/jnsn/ast.def
  ../include/jnsn/ast.h
  ../include/jnsn/ast_exec.h
  ../include/jnsn/ast_ops.h
  ../include/jnsn/ast_visitor.h
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
  ../include/jnsn/keywords.def
  ../include/jnsn/lexer.h
  ../include/jnsn/operators.def
  ../include/jnsn/parser.h
  ../include/jnsn/string_table.h
  ../include/jnsn/source_location.h
  ../include/jnsn/tokens.def
  ../include/jnsn/util.h)
target_link_libraries(ir_cli jnsn)
