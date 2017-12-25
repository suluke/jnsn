#ifndef JNSN_JS_PARSER_H
#define JNSN_JS_PARSER_H

#include "jnsn/js/ast.h"
#include "jnsn/js/lexer.h"
#include <stack>

namespace jnsn {

struct parser_error {
  std::string msg;
  source_location loc;
  friend std::ostream &operator<<(std::ostream &, const parser_error &);
};

class parser_base {
public:
  using ast_root = module_node;
  template <class nodety> using res = std::variant<nodety *, parser_error>;
  using result = res<ast_root>;

private:
  virtual lexer_base &get_lexer() = 0;

  ast_node_store nodes;
  module_node *module;
  token current_token;
  std::stack<token> rewind_stack;

  lexer_base::result next_token();
  std::variant<bool, parser_error> advance();
  void rewind(token t);
  void reset();

  res<statement_node> parse_statement();
  res<statement_node> parse_keyword_stmt();
  res<function_stmt_node> parse_function_stmt();

  res<if_stmt_node> parse_if_stmt();
  res<do_while_node> parse_do_while();
  res<while_stmt_node> parse_while_stmt();
  res<statement_node> parse_for_stmt();
  res<switch_stmt_node> parse_switch_stmt();
  res<return_stmt_node> parse_return_stmt();
  res<throw_stmt_node> parse_throw_stmt();
  res<try_stmt_node> parse_try_stmt();
  res<expression_node> parse_new_keyword();
  res<statement_node> parse_import();
  res<statement_node> parse_export();
  res<class_stmt_node> parse_class_stmt();

  res<expression_node> parse_expression(bool comma_is_operator);
  res<expression_node> parse_unary_or_atomic_expr();
  res<expression_node> parse_atomic_expr();
  res<expression_node> parse_atomic_keyword_expr();
  res<expression_node> parse_keyword_expr();
  res<expression_node> parse_parens_expr();
  res<string_literal_node> parse_string_literal();
  res<template_literal_node> parse_template_literal();
  res<number_literal_node> parse_number_literal();
  res<class_expr_node> parse_class_expr();
  res<function_expr_node> parse_function_expr();
  res<param_list_node> parse_param_list();
  res<block_node> parse_block();
  res<var_decl_node> parse_var_decl();
  res<bin_op_expr_node> parse_bin_op(expression_node *lhs,
                                     bool comma_is_operator);
  res<array_literal_node> parse_array_literal();
  res<object_literal_node> parse_object_literal();
  res<computed_member_access_node> parse_computed_access(expression_node *base);
  res<member_access_node> parse_member_access(expression_node *base);
  res<call_expr_node> parse_call(expression_node *callee);

public:
  result parse(bool verify = true);
};

class cin_line_parser : public parser_base {
  cin_line_lexer lexer;
  lexer_base &get_lexer() { return lexer; }
};

} // namespace jnsn
#endif // JNSN_JS_PARSER_H
