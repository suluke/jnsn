#include "parsing/ast_ops.h"
#include "gtest/gtest.h"

using namespace parsing;

TEST(ast_ops_test, isa) {
  ast_node *ptr;
#define NODE(NAME, CHILDREN)                                                   \
  {                                                                            \
    auto node = NAME##_node{};                                                 \
    ptr = &node;                                                               \
    ASSERT_TRUE(isa<NAME##_node>(ptr));                                        \
  }
#define DERIVED(NAME, EXTENDS, CHILDREN) NODE(NAME, CHILDREN)
#include "parsing/ast.def"
  {
    auto node = module_node{};
    ASSERT_FALSE(isa<statement_node>(node));
  }
  {
    auto node = function_expr_node{};
    ASSERT_TRUE(isa<expression_node>(node));
  }
  {
    auto node = statement_node{};
    ASSERT_FALSE(isa<expression_node>(node));
  }
}