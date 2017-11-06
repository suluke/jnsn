#ifndef PARSING_AST_OPS_H
#define PARSING_AST_OPS_H
#include "parsing/ast.h"

namespace parsing {

class ast_to_json {
private:
  const ast_node *ast;

public:
  ast_to_json(const ast_node *ast) : ast(ast) {}
  friend std::ostream &operator<<(std::ostream &, const ast_to_json &);
};

template<class nodety> bool isa(const ast_node *);
#define NODE(NAME, CHILDREN) template<> bool isa<NAME##_node>(const ast_node *);
#define DERIVED(NAME, EXTENDS, CHILDREN) NODE(NAME, CHILDREN)
#include "parsing/ast.def"

template<class nodety>
bool isa(const ast_node &node) {
  return isa<nodety>(&node);
}

} // namespace parsing
#endif // PARSING_AST_OPS_H
