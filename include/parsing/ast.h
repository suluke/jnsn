#ifndef PARSING_AST_H
#define PARSING_AST_H

#include "parsing/ast_visitor.h"
#include "parsing/lexer.h"
#include <cassert>
#include <vector>
#include <memory>

namespace parsing {
class ast_node_store;

/// Setting up node structs
#define NODE(NAME, CHILD_NODES)                                                \
  struct NAME##_node : public ast_node CHILD_NODES;
#define CHILDREN(...)                                                          \
  {                                                                            \
    __VA_ARGS__                                                                \
    void accept(ast_node_visitor_base &v) override { v.gen_result(*this); }    \
    void accept(const_ast_node_visitor_base &v) const override {               \
      v.gen_result(*this);                                                     \
    }                                                                          \
  }
#define EXTENDS(BASE)                                                          \
public                                                                         \
  BASE##_node
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  struct NAME##_node : ANCESTOR CHILD_NODES;
#define MANY(OF, NAME) std::vector<OF##_node *> NAME;
#define ONE(OF, NAME) OF##_node *NAME;
#define STRING(NAME) string_table::entry NAME;
#define STRINGS(NAME) std::vector<string_table::entry> NAME;
#define MAYBE_STR(NAME) std::optional<string_table::entry> NAME;
#define MAYBE(OF, NAME) std::optional<OF##_node *> NAME;
#include "parsing/ast.def"

/// The place where different nodes live
class ast_node_store {
  template <class NTy>
  using no_reloc_buf = std::vector<NTy>; // FIXME this would be an ideal case
                                         // for llvm::SmallVector
  template <class NTy>
  using no_reloc_buf_ptr = std::unique_ptr<no_reloc_buf<NTy>>;
#define NODE(NAME, CHILD_NODES)                                                \
  std::vector<no_reloc_buf_ptr<NAME##_node>> NAME##_vec;
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "parsing/ast.def"

public:
#define NODE(NAME, CHILD_NODES) NAME##_node *make_##NAME();
#define DERIVED(NAME, ANCESTOR, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "parsing/ast.def"

  void clear();
};

std::ostream &operator<<(std::ostream &, const ast_node *);
} // namespace parsing

#endif // PARSING_AST_H
