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

using ast_root = parser_base::ast_root;

TEST_F(parser_test, empty) {
  parser.lexer.set_text("");
  auto res = parser.parse();
  ASSERT_TRUE(holds_alternative<ast_root>(res));
  auto mod = get<ast_root>(res);

  str << mod;
  ASSERT_EQ(str.str(), "{\"type\": \"module\", \"stmts\": []}\n");
}
TEST_F(parser_test, decl) {
  parser.lexer.set_text("let x;");
  auto res = parser.parse();
  ASSERT_TRUE(holds_alternative<ast_root>(res));
  auto mod = get<ast_root>(res);

  str << mod;
  ASSERT_EQ(str.str(), "{\"type\": \"module\", \"stmts\": [{\"type\": "
                       "\"var_decl\", \"keyword\": \"let\", \"name\": \"x\", "
                       "\"init\": null}]}\n");
}