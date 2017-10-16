#include "parsing/ast_ops.h"
#include <sstream>

using namespace parsing;
using namespace std;

struct json_printer : public const_ast_node_visitor<void> {
  std::ostream &stream;
  json_printer(std::ostream &stream) : stream(stream) {}
#define SEP_MEMBERS stream << ", "

#define CHILDREN(...)                                                          \
  do {                                                                         \
    __VA_ARGS__                                                                \
  } while (false)
#define MANY(OF, NAME)                                                         \
  stream << #NAME ": [";                                                       \
  for (auto It = node.NAME.begin(), end = node.NAME.end(); It != end; ++It) {  \
    if (It != node.NAME.begin())                                               \
      stream << ", ";                                                          \
    (*It)->accept(*this);                                                      \
  }                                                                            \
  stream << "]";                                                               \
  SEP_MEMBERS;
#define ONE(OF, NAME)                                                          \
  stream << #NAME << ": ";                                                     \
  node.NAME->accept(*this);                                                    \
  SEP_MEMBERS;
#define MAYBE(OF, NAME)                                                        \
  stream << #NAME ": ";                                                        \
  if (node.NAME) {                                                             \
    visit(**node.NAME);                                                         \
  } else {                                                                     \
    stream << "null";                                                          \
  }                                                                            \
  SEP_MEMBERS;
#define MAYBE_STR(NAME)                                                        \
  stream << #NAME << ": ";                                                     \
  if (node.NAME) {                                                             \
    stream << "\"" << *node.NAME << "\"";                                      \
  } else {                                                                     \
    stream << "null";                                                          \
  }                                                                            \
  SEP_MEMBERS;
#define STRINGS(NAME)                                                          \
  stream << #NAME ": [";                                                       \
  for (auto &s : node.NAME) {                                                  \
    stream << s << ", ";                                                       \
  }                                                                            \
  stream << "]";                                                               \
  SEP_MEMBERS;
#define STRING(NAME)                                                           \
  stream << #NAME ": \"" << node.NAME << "\"";                                 \
  SEP_MEMBERS;

#define NODE(NAME, CHILD_NODES)                                                \
  virtual void accept(const NAME##_node &node) {                               \
    stream << "{";                                                             \
    stream << "type: "                                                         \
           << "\"" #NAME "\", ";                                               \
    CHILD_NODES;                                                               \
    stream << "}";                                                             \
  }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES)                                  \
  virtual void accept(const NAME##_node &node) {                               \
    stream << "{";                                                             \
    stream << "type: "                                                         \
           << "\"" #NAME "\", ";                                               \
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