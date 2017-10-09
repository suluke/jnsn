#ifndef PARSING_AST_VISITOR_H
#define PARSING_AST_VISITOR_H

namespace parsing {

class ast_node_visitor_base;
/// Base class of all ast nodes (for visitor)
struct ast_node {
  virtual void accept(ast_node_visitor_base &v);
};
/// forward declaration of all ast nodes
#define NODE(NAME, CHILD_NODES) class NAME ## _node;
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) class NAME ## _node;
#include "parsing/ast.def"

class ast_node_visitor_base {
#define NODE(NAME, CHILD_NODES) friend class NAME ## _node;
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) friend class NAME ## _node;
#include "parsing/ast.def"

#define NODE(NAME, CHILD_NODES) virtual void gen_result(NAME ## _node &node) = 0;
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) virtual void gen_result(NAME ## _node &node) = 0;
#include "parsing/ast.def"
protected:
  void dispatch(ast_node &node) {
    node.accept(*this);
  }
};
template<typename RET_TY>
class ast_node_visitor : public ast_node_visitor_base {
  RET_TY result;
#define NODE(NAME, CHILD_NODES) void gen_result(NAME ## _node &node) override { result = accept(node); }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) void gen_result(NAME ## _node &node) override { result = accept(node); }
#include "parsing/ast.def"
#define NODE(NAME, CHILD_NODES) virtual RET_TY accept(NAME ## _node &) = 0;
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) virtual RET_TY accept(NAME ## _node &) = 0;
#include "parsing/ast.def"
public:
  RET_TY visit(ast_node &n) final {
    dispatch(n);
    return result;
  }
};

} // namespace parsing

#endif // PARSING_AST_VISITOR_H