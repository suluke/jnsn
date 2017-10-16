#ifndef PARSING_AST_OPS_H
#define PARSING_AST_OPS_H
#include "parsing/parser.h"

namespace parsing {

class ast_to_json {
private:
  const ast_node_ref ast;

public:
  ast_to_json(ast_node_ref ast) : ast(ast) {}
  friend std::ostream &operator<<(std::ostream &, const ast_to_json &);
};

} // namespace parsing
#endif // PARSING_AST_OPS_H