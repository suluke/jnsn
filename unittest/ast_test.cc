#include "jnsn/js/ast.h"
#include "jnsn/js/ast_analysis.h"
#include "jnsn/js/ast_walker.h"
#include "gtest/gtest.h"
#include <sstream>

using namespace jnsn;

struct name_checker : public const_ast_node_visitor<const char *> {
#define NODE(NAME, CHILD_NODES)                                                \
  const char *accept(const NAME##_node &) override { return #NAME; }
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) NODE(NAME, ANCESTOR)
#include "jnsn/js/ast.def"
};

TEST(ast_test, visitor) {
  name_checker checker;
#define NODE_CHECK(NAME)                                                       \
  {                                                                            \
    NAME##_node node({});                                                      \
    ast_node &as_base = node;                                                  \
    auto *res = checker.visit(as_base);                                        \
    ASSERT_STREQ(res, #NAME);                                                  \
  }
#define NODE(NAME, CHILD_NODES) NODE_CHECK(NAME)
#define DERIVED(NAME, ANCESTORS, CHILD_NODES) NODE_CHECK(NAME)
#include "jnsn/js/ast.def"
#undef NODE_CHECK
}

TEST(ast_test, walker) {
  std::stringstream ss;
  ast_node_store store;
  auto *node = store.make_module({});
  auto *block1 = store.make_block({});
  auto *block2 = store.make_block({});
  node->stmts.emplace_back(block1);
  node->stmts.emplace_back(block2);
  struct walker : public ast_walker<walker> {
    std::stringstream &ss;
    walker(std::stringstream &ss) : ss(ss) {}
    bool on_enter(const module_node &mod) override {
      ss << "enter mod;";
      return true;
    }
    void on_leave(const module_node &mod) override { ss << "leave mod;"; }
    bool on_enter(const block_node &mod) override {
      ss << "enter block;";
      return true;
    }
    void on_leave(const block_node &mod) override { ss << "leave block;"; }
  } wlk(ss);
  wlk.visit(*node);
  ASSERT_STREQ(
      ss.str().c_str(),
      "enter mod;enter block;leave block;enter block;leave block;leave mod;");

  // Test early exit
  ss.str("");
  struct walker2 : public ast_walker<walker2> {
    std::stringstream &ss;
    walker2(std::stringstream &ss) : ss(ss) {}
    bool on_enter(const module_node &mod) override {
      ss << "enter mod;";
      return false;
    }
    void on_leave(const module_node &mod) override { ss << "leave mod;"; }
    bool on_enter(const block_node &mod) override {
      ss << "enter block;";
      return true;
    }
    void on_leave(const block_node &mod) override { ss << "leave block;"; }
  } wlk2(ss);
  wlk2.visit(*node);
  ASSERT_STREQ(ss.str().c_str(), "enter mod;leave mod;");
}

TEST(ast_test, node_store) {
  ast_node_store store;
  auto node = store.make_module({});
  name_checker checker;
  auto res = checker.visit(*node);
  ASSERT_STREQ(res, "module");
  // stress test container insertion
  constexpr int mod_count = 5000;
  constexpr int stmt_count = 100;
  std::vector<module_node *> modules;
  modules.reserve(mod_count);
  for (int i = 0; i < mod_count; i++) {
    modules.emplace_back(store.make_module({}));
  }
  auto *stmt = store.make_empty_stmt({});
  for (auto *mod : modules) {
    for (int i = 0; i < stmt_count; i++) {
      mod->stmts.emplace_back(stmt);
    }
  }
  for (int i = 0; i < mod_count; i++) {
    store.make_module({});
  }
  for (auto *mod : modules) {
    ASSERT_EQ(mod->stmts.size(), (size_t)stmt_count);
  }
}

TEST(ast_test, printing) {
  ast_node_store store;
  auto *node = store.make_module({});
  std::stringstream ss;
  ss << node;
  ASSERT_EQ(ss.str(), "{\"type\": \"module\", \"stmts\": []}\n");
}

TEST(ast_test, analysis) {
  ast_node_store store;
  auto *node = store.make_function_expr({});
  auto report = analyze_js_ast(*node);
  ASSERT_TRUE(report);
}
