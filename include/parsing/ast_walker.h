#ifndef PARSING_AST_WALKER_H
#define PARSING_AST_WALKER_H
#include "parsing/ast.h"
namespace parsing {

template <class impl> struct ast_walker : public const_ast_node_visitor<void> {
// FIXME needs `virtual` because otherwise it won't be instantiated
// see https://stackoverflow.com/a/23679903/1468532
#define NODE(NAME, CHILD_NODES)                                                \
  virtual void on_enter(const NAME##_node &) {}
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "parsing/ast.def"

#define NODE(NAME, CHILD_NODES)                                                \
  virtual void on_leave(const NAME##_node &) {}
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "parsing/ast.def"

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
    this->on_enter(node);                                                      \
    CHILD_NODES                                                                \
    this->on_leave(node);                                                      \
  }
#define EXTENDS(BASE) BASE##_node
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  void accept(const NAME##_node &node) override {                              \
    this->on_enter(node);                                                      \
    accept(static_cast<const ANCESTOR &>(node));                                     \
    CHILD_NODES                                                                \
    this->on_leave(node);                                                      \
  }
#include "parsing/ast.def"
};

} // namespace parsing
#endif // PARSING_AST_WALKER_H