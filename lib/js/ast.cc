#include "jnsn/js/ast.h"
#include "jnsn/js/ast_ops.h"
#include <cassert>
#include <iostream>

using namespace jnsn;
/// ast_node_store impl
#define NODE(NAME, CHILD_NODES)                                                \
  NAME##_node *ast_node_store::make_##NAME(source_location loc) {              \
    return &std::get<NAME##_node>(nodes.emplace_back(NAME##_node{loc}));       \
  }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) NODE(NAME, CHILD_NODES)
#include "jnsn/js/ast.def"

namespace jnsn {
std::ostream &operator<<(std::ostream &stream, const ast_node *ref) {
  stream << ast_to_json(ref) << "\n";
  return stream;
}
} // namespace jnsn
