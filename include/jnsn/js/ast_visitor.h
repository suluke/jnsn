#ifndef JNSN_JS_AST_VISITOR_H
#define JNSN_JS_AST_VISITOR_H
#include "jnsn/source_location.h"

namespace jnsn {

class const_ast_node_visitor_base;
/// Base class of all ast nodes (for visitor)
struct ast_node {
  source_location loc;
  ast_node(source_location loc) : loc(loc) {}
  ast_node(const ast_node &) = default;
  ast_node(ast_node &&) = default;
  ast_node &operator=(const ast_node &) = default;
  ast_node &operator=(ast_node &&) = default;
  virtual void accept(const_ast_node_visitor_base &v) const = 0;
  virtual ~ast_node() = default;
};
/// forward declaration of all ast nodes
#define NODE(NAME, CHILD_NODES) struct NAME##_node;
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"

class const_ast_node_visitor_base {
/// make ast nodes friend classes
#define NODE(NAME, CHILD_NODES) friend struct NAME##_node;
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"

#define NODE(NAME, CHILD_NODES)                                                \
  virtual void gen_result(const NAME##_node &node) = 0;
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  virtual void gen_result(const NAME##_node &node) = 0;
#include "jnsn/js/ast.def"
protected:
  void dispatch(const ast_node &node) { node.accept(*this); }
};

template <typename RET_TY>
class const_ast_node_visitor : public const_ast_node_visitor_base {
  RET_TY result;
#define NODE(NAME, CHILD_NODES)                                                \
  void gen_result(const NAME##_node &node) override { result = accept(node); }
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"
#define NODE(NAME, CHILD_NODES) virtual RET_TY accept(const NAME##_node &) = 0;
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"
public:
  RET_TY visit(const ast_node &n) {
    dispatch(n);
    return result;
  }
};

template <>
class const_ast_node_visitor<void> : public const_ast_node_visitor_base {
#define NODE(NAME, CHILD_NODES)                                                \
  void gen_result(const NAME##_node &node) override { accept(node); }
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  void gen_result(const NAME##_node &node) override { accept(node); }
#include "jnsn/js/ast.def"
#define NODE(NAME, CHILD_NODES) virtual void accept(const NAME##_node &) = 0;
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  virtual void accept(const NAME##_node &) = 0;
#include "jnsn/js/ast.def"
public:
  void visit(const ast_node &n) { dispatch(n); }
};

} // namespace jnsn
#endif // JNSN_JS_AST_VISITOR_H
