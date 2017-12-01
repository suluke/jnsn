#ifndef JNSN_PARSING_AST_OPS_H
#define JNSN_PARSING_AST_OPS_H
#include "jnsn/ast.h"

namespace jnsn {

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
#include "jnsn/ast.def"

template<class nodety>
bool isa(const ast_node &node) {
  return isa<nodety>(&node);
}

} // namespace jnsn
#endif // JNSN_PARSING_AST_OPS_H
