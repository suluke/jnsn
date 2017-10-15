#include "parse_utils.h"
#include "gtest/gtest.h"
#include <sstream>

using namespace parsing;
using namespace std;

class parser_test : public ::testing::Test {
protected:
  constant_string_parser parser;
  stringstream str;
};

TEST_F(parser_test, empty) {
  parser.lexer.set_text("");
  auto res = parser.parse();
  ASSERT_TRUE(holds_alternative<module_node *>(res));
  auto *mod = get<module_node *>(res);

  str << *mod;
  ASSERT_EQ(str.str(), "module {\n}\n");
}
TEST_F(parser_test, decl) {
  parser.lexer.set_text("let x;");
  auto res = parser.parse();
  ASSERT_TRUE(holds_alternative<module_node *>(res));
  auto *mod = get<module_node *>(res);

  str << *mod;
  ASSERT_EQ(str.str(), "module {\nvar_decl {\n}\n}\n");
}