#ifndef PARSING_PARSER_H
#define PARSING_PARSER_H

#include "parsing/ast.h"
#include "parsing/lexer.h"

namespace parsing {

struct parser_error {
  const char *msg;
  source_location loc;
};

class parser_base {
  virtual lexer_base::result next_token() = 0;

  ast_node_store nodes;
  ast_node_ref module;
public:
  using result = std::variant<module_node *, parser_error>;

  parser_base() : nodes(), module(nodes.make_module()) {}
  
  result parse();
};

class cin_line_parser : public parser_base {
  cin_line_lexer lexer;
  lexer_base::result next_token() {
    return lexer.next();
  }
};

} // namespace parsing

#endif // PARSING_PARSER_H