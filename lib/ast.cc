#include "parsing/ast.h"
#include <cassert>

using namespace parsing;

ast_node &ast_node_ref::operator*() {
  return store->get(ty, id);
}

ast_node *ast_node_ref::operator->() {
  return &store->get(ty, id);
}

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