// This definition disables the "extern template isa" declarations so that we
// can safely provide them here
#define PARSING_AST_OPS_CC
#include "parsing/ast_ops.h"
#undef PARSING_AST_OPS_CC
#include <sstream>

using namespace parsing;
using namespace std;

/// Returns the number of children that were printed for the top-level
/// node
struct parent_json_printer : public const_ast_node_visitor<unsigned> {
  std::ostream &stream;
  const_ast_node_visitor<void> &printer;
  parent_json_printer(std::ostream &stream,
                      const_ast_node_visitor<void> &printer)
      : stream(stream), printer(printer) {}
#define SEP_MEMBERS                                                            \
  if (child_idx++) {                                                           \
    stream << ", ";                                                            \
  }

#define CHILDREN(...)                                                          \
  do {                                                                         \
    int child_idx = children_printed;                                          \
    __VA_ARGS__                                                                \
    children_printed = child_idx;                                              \
  } while (false)

#define MANY(OF, NAME)                                                         \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": [";                                                \
  for (size_t i = 0; i < node.NAME.size(); ++i) {                              \
    if (i != 0) {                                                              \
      stream << ", ";                                                          \
    }                                                                          \
    node.NAME[i]->accept(printer);                                             \
  }                                                                            \
  stream << "]";

#define ONE(OF, NAME)                                                          \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": ";                                                 \
  node.NAME->accept(printer);

#define MAYBE(OF, NAME)                                                        \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": ";                                                 \
  if (node.NAME) {                                                             \
    (*node.NAME)->accept(printer);                                             \
  } else {                                                                     \
    stream << "null";                                                          \
  }

#define MAYBE_STR(NAME)                                                        \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME << "\": ";                                              \
  if (node.NAME) {                                                             \
    stream << "\"" << *node.NAME << "\"";                                      \
  } else {                                                                     \
    stream << "null";                                                          \
  }

#define STRINGS(NAME)                                                          \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": [";                                                \
  for (size_t i = 0; i < node.NAME.size(); ++i) {                              \
    if (i != 0) {                                                              \
      stream << ", ";                                                          \
    }                                                                          \
    stream << "\"" << node.NAME[i] << "\"";                                    \
  }                                                                            \
  stream << "]";

#define STRING(NAME)                                                           \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": \"" << node.NAME << "\"";

#define NODE(NAME, CHILD_NODES)                                                \
  unsigned accept(const NAME##_node &node) override {                          \
    unsigned children_printed = 0;                                             \
    CHILD_NODES;                                                               \
    return children_printed;                                                   \
  }
#define EXTENDS(NAME) NAME##_node
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  unsigned accept(const NAME##_node &node) override {                          \
    unsigned children_printed = accept(static_cast<const ANCESTOR &>(node));   \
    CHILD_NODES;                                                               \
    return children_printed;                                                   \
  }

#include "parsing/ast.def"
};

struct json_printer : public const_ast_node_visitor<void> {
  std::ostream &stream;
  parent_json_printer parent_printer;
  json_printer(std::ostream &stream)
      : stream(stream), parent_printer(stream, *this) {}

#define CHILDREN(...)                                                          \
  do {                                                                         \
    [[maybe_unused]] unsigned child_idx = parent_children;                     \
    __VA_ARGS__                                                                \
  } while (false)

#define MANY(OF, NAME)                                                         \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": [";                                                \
  for (size_t i = 0; i < node.NAME.size(); ++i) {                              \
    if (i != 0) {                                                              \
      stream << ", ";                                                          \
    }                                                                          \
    node.NAME[i]->accept(*this);                                               \
  }                                                                            \
  stream << "]";

#define ONE(OF, NAME)                                                          \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": ";                                                 \
  node.NAME->accept(*this);

#define MAYBE(OF, NAME)                                                        \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": ";                                                 \
  if (node.NAME) {                                                             \
    visit(**node.NAME);                                                        \
  } else {                                                                     \
    stream << "null";                                                          \
  }

#define MAYBE_STR(NAME)                                                        \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME << "\": ";                                              \
  if (node.NAME) {                                                             \
    stream << "\"" << *node.NAME << "\"";                                      \
  } else {                                                                     \
    stream << "null";                                                          \
  }

#define STRINGS(NAME)                                                          \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": [";                                                \
  for (size_t i = 0; i < node.NAME.size(); ++i) {                              \
    if (i != 0) {                                                              \
      stream << ", ";                                                          \
    }                                                                          \
    stream << "\"" << node.NAME[i] << "\"";                                    \
  }                                                                            \
  stream << "]";

#define STRING(NAME)                                                           \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": \"" << node.NAME << "\"";

#define NODE(NAME, CHILD_NODES)                                                \
  void accept(const NAME##_node &node) override {                              \
    stream << "{";                                                             \
    stream << "\"type\": "                                                     \
           << "\"" #NAME "\", ";                                               \
    unsigned parent_children = 0;                                              \
    CHILD_NODES;                                                               \
    stream << "}";                                                             \
  }
#define EXTENDS(NAME) NAME##_node
#define DERIVED(NAME, ANCESTOR, CHILD_NODES)                                   \
  void accept(const NAME##_node &node) override {                              \
    stream << "{";                                                             \
    stream << "\"type\": "                                                     \
           << "\"" #NAME "\", ";                                               \
    auto parent_children =                                                     \
        parent_printer.accept(static_cast<const ANCESTOR &>(node));            \
    CHILD_NODES;                                                               \
    stream << "}";                                                             \
  }
#include "parsing/ast.def"
};

namespace parsing {
std::ostream &operator<<(std::ostream &stream, const ast_to_json &wrapper) {
  json_printer printer{stream};
  printer.visit(*wrapper.ast);
  return stream;
}
} // namespace parsing

/// isa<> impl
template <class nodety>
struct isa_checker : public const_ast_node_visitor<bool> {
#define NODE(NAME, CHILDREN)                                                   \
  bool accept(const NAME##_node &) override {                                  \
    return std::is_same_v<NAME##_node, nodety>;                                \
  }
#define EXTENDS(BASE) BASE##_node
#define DERIVED(NAME, BASE, CHILDREN)                                          \
  bool accept(const NAME##_node &node) {                                       \
    return std::is_same_v<NAME##_node, nodety> ||                              \
           accept(static_cast<const BASE &>(node));                            \
  }
#include "parsing/ast.def"
};

namespace parsing {
#define NODE(NAME, CHILDREN)                                                   \
  template <> bool isa<NAME##_node>(const ast_node *node) {                    \
    assert(node);                                                              \
    return isa_checker<NAME##_node>().visit(*node);                            \
  }
#define DERIVED(NAME, EXTENDS, CHILDREN) NODE(NAME, CHILDREN)
#include "parsing/ast.def"
} // namespace parsing
