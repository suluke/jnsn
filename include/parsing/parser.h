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
public:
  using result = std::variant<module_node, parser_error>;
  result parse();
};

} // namespace parsing

#endif // PARSING_PARSER_H