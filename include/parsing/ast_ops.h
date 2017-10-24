#ifndef PARSING_AST_OPS_H
#define PARSING_AST_OPS_H
#include "parsing/parser.h"

namespace parsing {

class ast_to_json {
private:
  const ast_node *ast;

public:
  ast_to_json(const ast_node *ast) : ast(ast) {}
  friend std::ostream &operator<<(std::ostream &, const ast_to_json &);
};

} // namespace parsing
#endif // PARSING_AST_OPS_H
