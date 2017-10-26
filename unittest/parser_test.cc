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
    ASSERT_EQ(str.str(), JSON);                                                \
  } while (false)

#define PARSER_ERROR(INPUT)                                                    \
  do {                                                                         \
    str.str("");                                                               \
    parser.lexer.set_text(INPUT);                                              \
    auto res = parser.parse();                                                 \
    ASSERT_TRUE(holds_alternative<parser_error>(res));                         \
  } while (false)

TEST_F(parser_test, empty) {
  ASSERT_PARSED_MATCHES_JSON("", "{\"type\": \"module\", \"stmts\": []}\n");
}
TEST_F(parser_test, block) {
  ASSERT_PARSED_MATCHES_JSON("{}", "{\"type\": \"module\", \"stmts\": "
                                   "[{\"type\": \"object_literal\", "
                                   "\"entries\": []}]}\n");
}
TEST_F(parser_test, literals) {
  ASSERT_PARSED_MATCHES_JSON("1", "{\"type\": \"module\", \"stmts\": "
                                  "[{\"type\": \"int_literal\", \"val\": "
                                  "\"1\"}]}\n");
  PARSER_ERROR("1.window");
}
TEST_F(parser_test, parenthesis) {
  ASSERT_PARSED_MATCHES_JSON("(((1)))", "{\"type\": \"module\", \"stmts\": "
                                        "[{\"type\": \"int_literal\", \"val\": "
                                        "\"1\"}]}\n");
  PARSER_ERROR("(((1))");
}
TEST_F(parser_test, decl) {
  ASSERT_PARSED_MATCHES_JSON(
      "let x;", "{\"type\": \"module\", \"stmts\": [{\"type\": "
                "\"var_decl\", \"keyword\": \"let\", \"name\": \"x\", "
                "\"init\": null}]}\n");
  ASSERT_PARSED_MATCHES_JSON(
      "{let i = 0;}",
      "{\"type\": \"module\", \"stmts\": [{\"type\": "
      "\"block\", \"stmts\": [{\"type\": "
      "\"var_decl\", \"keyword\": \"let\", \"name\": \"i\", "
      "\"init\": {\"type\": \"int_literal\", \"val\": \"0\"}}]}]}\n");
}
TEST_F(parser_test, binary_ops) {
  ASSERT_PARSED_MATCHES_JSON(
      "1 + 1", "{\"type\": \"module\", \"stmts\": [{\"type\": \"add\", "
               "\"lhs\": {\"type\": \"int_literal\", \"val\": \"1\"}, \"rhs\": "
               "{\"type\": \"int_literal\", \"val\": \"1\"}}]}\n");
  ASSERT_PARSED_MATCHES_JSON(
      "1 + 2 / 2", "{\"type\": \"module\", \"stmts\": [{\"type\": \"add\", "
                   "\"lhs\": {\"type\": \"int_literal\", \"val\": \"1\"}, "
                   "\"rhs\": {\"type\": \"divide\", \"lhs\": {\"type\": "
                   "\"int_literal\", \"val\": \"2\"}, \"rhs\": {\"type\": "
                   "\"int_literal\", \"val\": \"2\"}}}]}\n");
}
