#ifndef PARSING_PARSER_H
#define PARSING_PARSER_H

#include "parsing/ast.h"
#include "parsing/lexer.h"
#include <stack>

namespace parsing {

struct parser_error {
  std::string msg;
  source_location loc;
  friend std::ostream &operator<<(std::ostream &, const parser_error &);
};

class parser_base {
public:
  using ast_root = module_node *;
  using result = std::variant<ast_root, parser_error>;

private:
  virtual lexer_base &get_lexer() = 0;

  ast_node_store nodes;
  ast_root module;
  std::optional<parser_error> error;
  token current_token;
  std::stack<token> rewind_stack;

  lexer_base::result next_token();
  bool advance();
  void rewind(token t);
  void reset();

  statement_node *parse_statement();
  statement_node *parse_block_or_obj();
  statement_node *parse_keyword_stmt();
  function_stmt_node *parse_function_stmt();

  if_stmt_node *parse_if_stmt();
  do_while_node *parse_do_while();
  while_stmt_node *parse_while_stmt();
  for_stmt_node *parse_for_stmt();
  switch_stmt_node *parse_switch_stmt();
  return_stmt_node *parse_return_stmt();
  throw_stmt_node *parse_throw_stmt();
  try_stmt_node *parse_try_stmt();

  expression_node *parse_expression(bool comma_is_operator);
  expression_node *parse_atomic_expr();
  expression_node *parse_keyword_expr();
  string_literal_node *parse_string_literal();
  number_literal_node *parse_number_literal();
  function_expr_node *parse_function_expr();
  param_list_node *parse_param_list();
  block_node *parse_block();
  var_decl_node *parse_var_decl();
  bin_op_expr_node *parse_bin_op(expression_node *lhs, bool comma_is_operator);
  array_literal_node *parse_array_literal();
  object_literal_node *parse_object_literal();
  computed_member_access_node *parse_computed_access(expression_node *base);
  member_access_node *parse_member_access(expression_node *base);
  call_expr_node *parse_call(expression_node *callee);

  void set_error(std::string msg, source_location loc);

public:
  result parse();
};

class cin_line_parser : public parser_base {
  cin_line_lexer lexer;
  lexer_base &get_lexer() { return lexer; }
};

} // namespace parsing

#endif // PARSING_PARSER_H
