#include "parse_utils.h"
#include "parsing/ast_exec.h"
#include "gtest/gtest.h"
#include <sstream>

using namespace parsing;
using namespace std;

class exec_test : public ::testing::Test {
protected:
  constant_string_parser parser;
  stringstream str;
};

#define ASSERT_EXEC_RES_EQUALS(INPUT, EXPECTED)                                \
  do {                                                                         \
    str.str("");                                                               \
    parser.lexer.set_text(INPUT);                                              \
    auto parse_res = parser.parse();                                           \
    ASSERT_TRUE(holds_alternative<module_node *>(parse_res))                   \
        << std::get<parser_error>(parse_res);                                  \
    auto mod = get<module_node *>(parse_res);                                  \
                                                                               \
    auto exec_res = ast_executor().execute(*mod);                              \
    ASSERT_TRUE(holds_alternative<exec_value>(exec_res))                       \
        << std::get<exec_error>(exec_res);                                     \
    auto val = std::get<exec_value>(exec_res);                                 \
    str << val;                                                                \
    ASSERT_EQ(str.str(), EXPECTED);                                            \
  } while (false)

TEST_F(exec_test, basic) {
  ASSERT_EXEC_RES_EQUALS("1", "1");
  ASSERT_EXEC_RES_EQUALS("true", "true");
  ASSERT_EXEC_RES_EQUALS("false", "false");
  ASSERT_EXEC_RES_EQUALS("null", "null");
  ASSERT_EXEC_RES_EQUALS("undefined", "undefined");
  ASSERT_EXEC_RES_EQUALS("0xf", "15");
  ASSERT_EXEC_RES_EQUALS("\"abc\"", "\"abc\"");
  ASSERT_EXEC_RES_EQUALS("'abc'", "\"abc\"");
  ASSERT_EXEC_RES_EQUALS("`abc`", "\"abc\"");
  ASSERT_EXEC_RES_EQUALS("[1,2,3]", "[1, 2, 3]");
  ASSERT_EXEC_RES_EQUALS("1,2", "2");
}