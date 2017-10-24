#include "parsing/ast.h"
#include "gtest/gtest.h"
#include <sstream>

using namespace parsing;

struct name_checker : public ast_node_visitor<const char *> {
#define NODE(NAME, CHILD_NODES) const char *accept(NAME ## _node &) override { return #NAME; }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) const char *accept(NAME ## _node &) override { return #NAME; }
#include "parsing/ast.def"
};

TEST(ast_test, visitor) {
  name_checker checker;
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

TEST(ast_test, node_store) {
  ast_node_store store;
  auto node = store.make_module();
  name_checker checker;
  auto res = checker.visit(*node);
  ASSERT_STREQ(res, "module");
  // stress test container insertion
  constexpr int mod_count = 5000;
  constexpr int expr_count = 100;
  std::vector<module_node *> modules;
  modules.reserve(mod_count);
  for (int i = 0; i < mod_count; i++) {
    modules.emplace_back(store.make_module());
  }
  auto *expr = store.make_empty_expr();
  for (auto *mod : modules) {
    for (int i = 0; i < expr_count; i++) {
      mod->exprs.emplace_back(expr);
    }
  }
  for (int i = 0; i < mod_count; i++) {
    store.make_module();
  }
  for (auto *mod : modules) {
    ASSERT_EQ(mod->exprs.size(), (size_t) expr_count);
  }
}

TEST(ast_test, printing) {
  ast_node_store store;
  auto node = store.make_module();
  std::stringstream ss;
  ss << node;
  ASSERT_EQ(ss.str(), "{\"type\": \"module\", \"exprs\": []}\n");
}
