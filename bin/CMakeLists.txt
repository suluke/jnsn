add_executable(lexer_cli
  lexer_cli.cc
  ../include/parsing/keywords.def
  ../include/parsing/lexer.h
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
  ../include/parsing/source_location.h
  ../include/parsing/tokens.def
  ../include/parsing/util.h)
target_link_libraries(parser_cli parsing)

add_executable(exec_cli
  exec_cli.cc
  ../include/parsing/ast.def
  ../include/parsing/ast.h
  ../include/parsing/ast_executor.h
  ../include/parsing/ast_ops.h
  ../include/parsing/ast_visitor.h
  ../include/parsing/keywords.def
  ../include/parsing/lexer.h
  ../include/parsing/operators.def
  ../include/parsing/parser.h
  ../include/parsing/source_location.h
  ../include/parsing/tokens.def
  ../include/parsing/util.h)
target_link_libraries(exec_cli parsing)