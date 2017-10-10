#include "parsing/ast.h"
#include "gtest/gtest.h"

using namespace parsing;

TEST(ast_test, visitor) {
  struct name_checker : public ast_node_visitor<const char *> {
#define NODE(NAME, CHILD_NODES) const char *accept(NAME ## _node &) override { return #NAME; }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) const char *accept(NAME ## _node &) override { return #NAME; }
#include "parsing/ast.def"
  } checker;
  #define NODE_CHECK(NAME) {\
    NAME ## _node node;\
    ast_node &as_base = node;\
    auto *res = checker.visit(as_base);\
    ASSERT_STREQ(res, #NAME);\
  }
  #define NODE(NAME, CHILD_NODES) NODE_CHECK(NAME)
  #define DERIVED(NAME, ANCESTORS, CHILD_NODES) NODE_CHECK(NAME)
  #include "parsing/ast.def"
  #undef NODE_CHECK
}