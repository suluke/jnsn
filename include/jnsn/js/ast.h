#ifndef JNSN_JS_AST_H
#define JNSN_JS_AST_H

#include "jnsn/js/ast_visitor.h"
#include "jnsn/js/lexer.h"
#include <cassert>
#include <deque>
#include <memory>
#include <vector>

namespace jnsn {
class ast_node_store;

/// Setting up node structs
#define NODE(NAME, CHILD_NODES)                                                \
  struct NAME##_node : public ast_node {                                       \
    NAME##_node(source_location loc) : ast_node(loc) {}                        \
    void accept(const_ast_node_visitor_base &v) const override {               \
      v.gen_result(*this);                                                     \
    }                                                                          \
    CHILD_NODES                                                                \
  };
#define CHILDREN(...) __VA_ARGS__

#define EXTENDS(BASE) BASE##_node
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  struct NAME##_node : public ANCESTOR {                                       \
    NAME##_node(source_location loc) : ANCESTOR(loc) {}                        \
    void accept(const_ast_node_visitor_base &v) const override {               \
      v.gen_result(*this);                                                     \
    }                                                                          \
    CHILD_NODES                                                                \
  };
#define MANY(OF, NAME) std::vector<OF##_node *> NAME;
#define ONE(OF, NAME) OF##_node *NAME = nullptr;
#define STRING(NAME) string_table::entry NAME;
#define STRINGS(NAME) std::vector<string_table::entry> NAME;
#define MAYBE_STR(NAME) std::optional<string_table::entry> NAME;
#define MAYBE(OF, NAME) std::optional<OF##_node *> NAME;
#include "jnsn/js/ast.def"

/// The place where different nodes live
class ast_node_store {
  using ast_node_storage = std::variant<
#define NODE(NAME, CHILD_NODES) NAME##_node,
#define DERIVED(NAME, BASE, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"
      std::monostate>;
  std::deque<ast_node_storage> nodes;

public:
#define NODE(NAME, CHILD_NODES) NAME##_node *make_##NAME(source_location loc);
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"

  void clear() { nodes.clear(); }
};

std::ostream &operator<<(std::ostream &, const ast_node *);
} // namespace jnsn

#endif // JNSN_JS_AST_H
