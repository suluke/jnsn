#include "parsing/ast_ops.h"

using namespace parsing;

struct json_printer : public const_ast_node_visitor<void> {
  std::ostream &stream;
  json_printer(std::ostream &stream) : stream(stream) {}
#define CHILDREN(...)                                                          \
  do {                                                                         \
    __VA_ARGS__                                                                \
  } while (false)
#define MANY(OF, NAME)                                                         \
  for (auto &child : node.NAME) {                                              \
    child->accept(*this);                                                      \
  }
#define ONE(OF, NAME) node.NAME->accept(*this);
#define NODE(NAME, CHILD_NODES)                                                \
  virtual void accept(const NAME##_node &node) {                               \
    stream << #NAME << " {\n";                                                 \
    CHILD_NODES;                                                               \
    stream << "}\n";                                                           \
  }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES)                                  \
  virtual void accept(const NAME##_node &node) {                               \
    stream << #NAME << " {\n";                                                 \
    CHILD_NODES;                                                               \
    stream << "}\n";                                                           \
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