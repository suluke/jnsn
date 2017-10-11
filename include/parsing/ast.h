#ifndef PARSING_AST_H
#define PARSING_AST_H

#include <vector>
#include "parsing/ast_visitor.h"
#include "parsing/string_table.h"

namespace parsing {
class ast_node_store;

enum class ast_node_type {
#define NODE(NAME, CHILD_NODES) NAME ## _ty,
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) NAME ## _ty,
#include "parsing/ast.def"
};

/// A handle to ast nodes stored inside an ast_node_store
class ast_node_ref {
  ast_node_store *store;
  ast_node_type ty;
  size_t id;
public:
  ast_node_ref(ast_node_store &store, ast_node_type ty, size_t id) : store(&store), ty(ty), id(id) {}
  ast_node_ref() = default;
  ast_node_ref(const ast_node_ref &) = default;
  ast_node_ref(ast_node_ref &&) = default;
  ast_node_ref &operator=(ast_node_ref &&) = default;
  ast_node_ref &operator=(const ast_node_ref &) = default;
  ast_node &operator*();
  ast_node *operator->();
  operator bool() { return store; }
};

/// Setting up node structs
#define NODE(NAME, CHILD_NODES) struct NAME ## _node : public ast_node CHILD_NODES;
#define CHILDREN(...) {\
  __VA_ARGS__ \
  void accept(ast_node_visitor_base &v) override { v.gen_result(*this); }\
}
#define EXTENDS(BASE) public BASE ## _node
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) struct NAME ## _node : ANCESTORS CHILD_NODES;
#define MANY(OF, NAME) std::vector<ast_node_ref> NAME;
#define ONE(OF, NAME) ast_node_ref NAME;
using IDENTIFIER_node = string_table::entry;
#include "parsing/ast.def"

/// The place where different nodes live
class ast_node_store {
  friend class ast_node_ref;
  ast_node &get(ast_node_type ty, size_t id);

#define NODE(NAME, CHILD_NODES) std::vector<NAME ## _node> NAME ## _vec;
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) std::vector<NAME ## _node> NAME ## _vec;
#include "parsing/ast.def"

public:
#define NODE(NAME, CHILD_NODES) ast_node_ref make_ ## NAME() { NAME ## _vec.emplace_back(); return { *this, ast_node_type::NAME ## _ty, NAME ## _vec.size() - 1 }; }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) ast_node_ref make_ ## NAME() { NAME ## _vec.emplace_back(); return { *this, ast_node_type::NAME ## _ty, NAME ## _vec.size() - 1 }; }
#include "parsing/ast.def"
};

} // namespace parsing

#endif // PARSING_AST_H