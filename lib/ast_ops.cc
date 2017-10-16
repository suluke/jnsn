#include "parsing/ast_ops.h"
#include <sstream>

using namespace parsing;
using namespace std;

struct json_printer : public const_ast_node_visitor<void> {
  std::ostream &stream;
  json_printer(std::ostream &stream) : stream(stream) {}

#define SEP_MEMBERS                                                            \
  if (child_idx++) {                                                           \
    stream << ", ";                                                            \
  }

#define CHILDREN(...)                                                          \
  do {                                                                         \
    int child_idx = 0;                                                         \
    __VA_ARGS__                                                                \
  } while (false)

#define MANY(OF, NAME)                                                         \
  SEP_MEMBERS;                                                                 \
  stream << "\"" #NAME "\": [";                                                \
  for (int i = 0; i < node.NAME.size(); ++i) {                                 \
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
  for (int i = 0; i < node.NAME.size(); ++i) {                                 \
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
  virtual void accept(const NAME##_node &node) {                               \
    stream << "{";                                                             \
    stream << "\"type\": "                                                     \
           << "\"" #NAME "\", ";                                               \
    CHILD_NODES;                                                               \
    stream << "}";                                                             \
  }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES)                                  \
  virtual void accept(const NAME##_node &node) {                               \
    stream << "{";                                                             \
    stream << "\"type\": "                                                     \
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