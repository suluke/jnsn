#ifndef PARSING_AST_H
#define PARSING_AST_H

#include <vector>
#include "parsing/ast_visitor.h"
#include "parsing/string_table.h"

/* Please don't hate me for this abomination of a header.
 * I'm simply trying to get this up and running as quickly as possible
 */
namespace parsing {

#define NODE(NAME, CHILD_NODES) struct NAME ## _node : public ast_node CHILD_NODES;
#define CHILDREN(...) {\
  __VA_ARGS__ \
  void accept(ast_node_visitor_base &v) override { v.gen_result(*this); }\
}
#define EXTENDS(BASE) public BASE ## _node
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) struct NAME ## _node : ANCESTORS CHILD_NODES;
#define MANY(OF, NAME) std::vector<OF ## _node *> NAME;
#define ONE(OF, NAME) OF ## _node *NAME;
using IDENTIFIER_node = string_table::entry;
#include "parsing/ast.def"

class ast_node_store {
#define NODE(NAME, CHILD_NODES) std::vector<NAME ## _node> NAME ## _vec;
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) std::vector<NAME ## _node> NAME ## _vec;
#include "parsing/ast.def"

public:
#define NODE(NAME, CHILD_NODES) NAME ## _node &make_ ## NAME() { NAME ## _vec.emplace_back(); return NAME ## _vec.back(); }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) NAME ## _node &make_ ## NAME() { NAME ## _vec.emplace_back(); return NAME ## _vec.back(); }
#include "parsing/ast.def"
};

} // namespace parsing

#endif // PARSING_AST_H