add_executable(lexer_cli
  lexer_cli.cc
  ../include/parsing/keywords.def
  ../include/parsing/lexer.h
  ../include/parsing/string_table.h
  ../include/parsing/source_location.h
  ../include/parsing/tokens.def
  ../include/parsing/util.h)
target_link_libraries(lexer_cli parsing)

add_executable(parser_cli
  parser_cli.cc
  ../include/parsing/ast.def
  ../include/parsing/ast.h
  ../include/parsing/ast_ops.h
  ../include/parsing/ast_visitor.h
  ../include/parsing/keywords.def
  ../include/parsing/lexer.h
  ../include/parsing/operators.def
  ../include/parsing/parser.h
  ../include/parsing/string_table.h
  ../include/parsing/source_location.h
  ../include/parsing/tokens.def
  ../include/parsing/util.h)
target_link_libraries(parser_cli parsing)

add_executable(exec_cli
  exec_cli.cc
  ../include/parsing/ast.def
  ../include/parsing/ast.h
  ../include/parsing/ast_exec.h
  ../include/parsing/ast_ops.h
  ../include/parsing/ast_visitor.h
  ../include/parsing/keywords.def
  ../include/parsing/lexer.h
  ../include/parsing/operators.def
  ../include/parsing/parser.h
  ../include/parsing/string_table.h
  ../include/parsing/source_location.h
  ../include/parsing/tokens.def
  ../include/parsing/util.h)
target_link_libraries(exec_cli parsing)

add_executable(ir_cli
  ir_cli.cc
  ../include/parsing/ast.def
  ../include/parsing/ast.h
  ../include/parsing/ast_exec.h
  ../include/parsing/ast_ops.h
  ../include/parsing/ast_visitor.h
  ../include/parsing/ir/instructions.def
  ../include/parsing/ir/instructions.h
  ../include/parsing/ir/intrinsics.def
  ../include/parsing/ir/ir.h
  ../include/parsing/ir/ir_builder.h
  ../include/parsing/ir/ir_context.h
  ../include/parsing/ir/module.h
  ../include/parsing/ir/types.def
  ../include/parsing/ir/types.h
  ../include/parsing/ir/value.h
  ../include/parsing/keywords.def
  ../include/parsing/lexer.h
  ../include/parsing/operators.def
  ../include/parsing/parser.h
  ../include/parsing/string_table.h
  ../include/parsing/source_location.h
  ../include/parsing/tokens.def
  ../include/parsing/util.h)
target_link_libraries(ir_cli parsing)