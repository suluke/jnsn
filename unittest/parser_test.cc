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

#define ASSERT_PARSED_MATCHES_JSON(INPUT, JSON)                                \
  do {                                                                         \
    str.str("");                                                               \
    parser.lexer.set_text(INPUT);                                              \
    auto res = parser.parse();                                                 \
    ASSERT_TRUE(holds_alternative<ast_root>(res))                              \
        << std::get<parser_error>(res);                                        \
    auto mod = get<ast_root>(res);                                             \
                                                                               \
    str << mod;                                                                \
    ASSERT_EQ(str.str(), JSON "\n");                                           \
  } while (false)

#define PARSER_ERROR(INPUT)                                                    \
  do {                                                                         \
    str.str("");                                                               \
    parser.lexer.set_text(INPUT);                                              \
    auto res = parser.parse();                                                 \
    ASSERT_TRUE(holds_alternative<parser_error>(res));                         \
  } while (false)

TEST_F(parser_test, empty) {
  ASSERT_PARSED_MATCHES_JSON("", "{\"type\": \"module\", \"stmts\": []}");
}
TEST_F(parser_test, block) {
  ASSERT_PARSED_MATCHES_JSON("{}", "{\"type\": \"module\", \"stmts\": "
                                   "[{\"type\": \"object_literal\", "
                                   "\"entries\": []}]}");
}
TEST_F(parser_test, literals) {
  ASSERT_PARSED_MATCHES_JSON("1", "{\"type\": \"module\", \"stmts\": "
                                  "[{\"type\": \"int_literal\", \"val\": "
                                  "\"1\"}]}");
  PARSER_ERROR("1.window");
}
TEST_F(parser_test, parenthesis) {
  ASSERT_PARSED_MATCHES_JSON("(((1)))", "{\"type\": \"module\", \"stmts\": "
                                        "[{\"type\": \"int_literal\", \"val\": "
                                        "\"1\"}]}");
  PARSER_ERROR("(((1))");
}
TEST_F(parser_test, decl) {
  ASSERT_PARSED_MATCHES_JSON(
      "let x;", "{\"type\": \"module\", \"stmts\": [{\"type\": "
                "\"var_decl\", \"keyword\": \"let\", \"name\": \"x\", "
                "\"init\": null}]}");
  ASSERT_PARSED_MATCHES_JSON(
      "{let i = 0;}",
      "{\"type\": \"module\", \"stmts\": [{\"type\": "
      "\"block\", \"stmts\": [{\"type\": "
      "\"var_decl\", \"keyword\": \"let\", \"name\": \"i\", "
      "\"init\": {\"type\": \"int_literal\", \"val\": \"0\"}}]}]}");
}
TEST_F(parser_test, binary_ops) {
  ASSERT_PARSED_MATCHES_JSON(
      "1 + 1", "{\"type\": \"module\", \"stmts\": [{\"type\": \"add\", "
               "\"lhs\": {\"type\": \"int_literal\", \"val\": \"1\"}, \"rhs\": "
               "{\"type\": \"int_literal\", \"val\": \"1\"}}]}");
  ASSERT_PARSED_MATCHES_JSON(
      "1 + 4 / 2", "{\"type\": \"module\", \"stmts\": [{\"type\": \"add\", "
                   "\"lhs\": {\"type\": \"int_literal\", \"val\": \"1\"}, "
                   "\"rhs\": {\"type\": \"divide\", \"lhs\": {\"type\": "
                   "\"int_literal\", \"val\": \"4\"}, \"rhs\": {\"type\": "
                   "\"int_literal\", \"val\": \"2\"}}}]}");
  ASSERT_PARSED_MATCHES_JSON(
      "1 + 4 / 2; 6 + 7",
      "{\"type\": \"module\", \"stmts\": [{\"type\": \"add\", "
      "\"lhs\": {\"type\": \"int_literal\", \"val\": \"1\"}, "
      "\"rhs\": {\"type\": \"divide\", \"lhs\": {\"type\": "
      "\"int_literal\", \"val\": \"4\"}, \"rhs\": {\"type\": "
      "\"int_literal\", \"val\": \"2\"}}}, {\"type\": \"add\", \"lhs\": "
      "{\"type\": \"int_literal\", \"val\": \"6\"}, \"rhs\": {\"type\": "
      "\"int_literal\", \"val\": \"7\"}}]}");
}
TEST_F(parser_test, function) {
  ASSERT_PARSED_MATCHES_JSON(
      "function test(arg1, arg2) { return arg1 + arg2; }",
      "{\"type\": \"module\", \"stmts\": [{\"type\": \"function\", \"name\": "
      "\"test\", \"params\": {\"type\": \"param_list\", \"names\": [\"arg1\", "
      "\"arg2\"], \"rest\": null}, \"body\": {\"type\": \"block\", \"stmts\": "
      "[{\"type\": \"return_stmt\", \"value\": {\"type\": \"add\", \"lhs\": "
      "{\"type\": \"identifier_expr\", \"str\": \"arg1\"}, \"rhs\": {\"type\": "
      "\"identifier_expr\", \"str\": \"arg2\"}}}]}}]}");
}
TEST_F(parser_test, member_access) {
  ASSERT_PARSED_MATCHES_JSON(
      "a.b.c.d", "{\"type\": \"module\", \"stmts\": [{\"type\": "
                 "\"member_access\", \"base\": {\"type\": \"member_access\", "
                 "\"base\": {\"type\": \"member_access\", \"base\": {\"type\": "
                 "\"identifier_expr\", \"str\": \"a\"}, \"member\": \"b\"}, "
                 "\"member\": \"c\"}, \"member\": \"d\"}]}");
}
TEST_F(parser_test, call) {
  ASSERT_PARSED_MATCHES_JSON("console.log(1 + 2);",
                             "{\"type\": \"module\", \"stmts\": [{\"type\": "
                             "\"call_expr\", \"callee\": {\"type\": "
                             "\"member_access\", \"base\": {\"type\": "
                             "\"identifier_expr\", \"str\": \"console\"}, "
                             "\"member\": \"log\"}, \"args\": {\"type\": "
                             "\"argument_list\", \"values\": [{\"type\": "
                             "\"add\", \"lhs\": {\"type\": \"int_literal\", "
                             "\"val\": \"1\"}, \"rhs\": {\"type\": "
                             "\"int_literal\", \"val\": \"2\"}}]}}]}");
}