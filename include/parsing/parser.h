#ifndef PARSING_PARSER_H
#define PARSING_PARSER_H

#include "parsing/ast.h"
#include "parsing/lexer.h"

namespace parsing {

struct parser_error {
  std::string msg;
  source_location loc;
};

class parser_base {
  virtual lexer_base &get_lexer() = 0;

  ast_node_store nodes;
  typed_ast_node_ref<module_node> module;
  std::optional<parser_error> error;
  token current_token;

  lexer_base::result next_token();
  bool advance();

  typed_ast_node_ref<expression_node> parse_expression();
  typed_ast_node_ref<expression_node> parse_keyword_expr();
  typed_ast_node_ref<function_node> parse_function();
  typed_ast_node_ref<param_list_node> parse_param_list();
  typed_ast_node_ref<block_node> parse_block();
  typed_ast_node_ref<var_decl_node> parse_var_decl();
public:
  using result = std::variant<module_node *, parser_error>;

  result parse();
};

class cin_line_parser : public parser_base {
  cin_line_lexer lexer;
  lexer_base &get_lexer() {
    return lexer;
  }
};

} // namespace parsing

#endif // PARSING_PARSER_H
