#ifndef JNSN_JS_AST_WALKER_H
#define JNSN_JS_AST_WALKER_H
#include "jnsn/js/ast.h"
namespace jnsn {

template <class impl> struct ast_walker : public const_ast_node_visitor<void> {
  ast_walker() { static_assert(std::is_base_of_v<ast_walker, impl>); }
// FIXME needs `virtual` because otherwise it won't be instantiated
// see https://stackoverflow.com/a/23679903/1468532
#define NODE(NAME, CHILD_NODES)                                                \
  virtual bool on_enter(const NAME##_node &) { return true; }
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"

#define NODE(NAME, CHILD_NODES)                                                \
  virtual void on_leave(const NAME##_node &) {}
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"

#define CHILDREN(...) __VA_ARGS__
#define ONE(OF, NAME) visit(*node.NAME);
#define MANY(OF, NAME)                                                         \
  for (const auto *child : node.NAME)                                          \
    visit(*child);
#define MAYBE(OF, NAME)                                                        \
  if (node.NAME)                                                               \
    visit(**node.NAME);
#define NODE(NAME, CHILD_NODES)                                                \
  void accept(const NAME##_node &node) override {                              \
    if (this->on_enter(node)) {                                                \
      CHILD_NODES                                                              \
    }                                                                          \
    this->on_leave(node);                                                      \
  }
#define EXTENDS(BASE) BASE##_node
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  void accept(const NAME##_node &node) override {                              \
    if (this->on_enter(node)) {                                                \
      accept(static_cast<const ANCESTOR &>(node));                             \
      CHILD_NODES                                                              \
    }                                                                          \
    this->on_leave(node);                                                      \
  }
#include "jnsn/js/ast.def"
}; // namespace jnsn

} // namespace jnsn
#endif // JNSN_JS_AST_WALKER_H
