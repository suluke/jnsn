#include "parsing/ast.h"
#include <cassert>
#include <iostream>

using namespace parsing;

ast_node &ast_node_ref::get() {
  return store->get(ty, id);
}

const ast_node &ast_node_ref::get() const {
  return store->get(ty, id);
}

ast_node &ast_node_ref::operator*() {
  assert(*this);
  return get();
}

ast_node *ast_node_ref::operator->() {
  assert(*this);
  return &get();
}

const ast_node &ast_node_ref::operator*() const {
  assert(*this);
  return get();
}

const ast_node *ast_node_ref::operator->() const {
  assert(*this);
  return &get();
}

/// has_type impl
#define NODE(NAME, CHILD_NODES)                                                \
  bool NAME##_node::has_type(ast_node_type ty) {                               \
    return ty == ast_node_type::NAME##_ty;                                     \
  }
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  bool NAME##_node::has_type(ast_node_type ty) {                               \
    return ty == ast_node_type::NAME##_ty;                                     \
  }
#include "parsing/ast.def"

/// ast_node_store impl
ast_node &ast_node_store::get(ast_node_type ty, size_t id) {
  switch(ty) {
#define NODE(NAME, CHILD_NODES) case ast_node_type::NAME ## _ty: return NAME ## _vec[id];
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) case ast_node_type::NAME ## _ty: return NAME ## _vec[id];
#include "parsing/ast.def"
  default:
    assert(false && "Node type does not exist");
  }
  assert(false && "Node type does not exist");
}

const ast_node &ast_node_store::get(ast_node_type ty, size_t id) const {
  switch(ty) {
#define NODE(NAME, CHILD_NODES) case ast_node_type::NAME ## _ty: return NAME ## _vec[id];
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) case ast_node_type::NAME ## _ty: return NAME ## _vec[id];
#include "parsing/ast.def"
  default:
    assert(false && "Node type does not exist");
  }
  assert(false && "Node type does not exist");
}

void ast_node_store::clear() {
#define NODE(NAME, CHILD_NODES) NAME ## _vec.clear();
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) NAME ## _vec.clear();
#include "parsing/ast.def"
}

struct node_printer : public const_ast_node_visitor<void> {
  std::ostream &stream;
  node_printer(std::ostream &stream) : stream(stream) {}
#define CHILDREN(...) do { __VA_ARGS__ } while(false)
#define MANY(OF, NAME) for (auto &child : node.NAME) { child->accept(*this); }
#define ONE(OF, NAME) node.NAME->accept(*this);
#define NODE(NAME, CHILD_NODES) \
virtual void accept(const NAME ## _node &node) {\
  stream << #NAME << " {\n"; \
  CHILD_NODES;\
  stream << "}\n";\
}
#define DERIVED(NAME, ANCESTORS, CHILD_NODES)  \
virtual void accept(const NAME ## _node &node) {\
  stream << #NAME << " {\n"; \
  CHILD_NODES;\
  stream << "}\n";\
}
#include "parsing/ast.def"
};

namespace parsing {
std::ostream &operator<<(std::ostream &stream, const ast_node_ref &ref) {
  stream << *ref;
}
std::ostream &operator<<(std::ostream &stream, const ast_node &node) {
  node_printer printer{stream};
  printer.visit(node);
}
} // namespace parsing
