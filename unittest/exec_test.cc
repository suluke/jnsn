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
  ast_executor exec;
};

#define ASSERT_EXEC_RES_EQUALS(INPUT, EXPECTED)                                \
  do {                                                                         \
    str.str("");                                                               \
    parser.lexer.set_text(INPUT);                                              \
    auto parse_res = parser.parse();                                           \
    ASSERT_TRUE(holds_alternative<module_node *>(parse_res))                   \
        << std::get<parser_error>(parse_res);                                  \
    auto mod = get<module_node *>(parse_res);                                  \
    auto exec_res = exec.execute(*mod);                                        \
    ASSERT_TRUE(holds_alternative<exec_value>(exec_res))                       \
        << std::get<exec_error>(exec_res);                                     \
    auto val = std::get<exec_value>(exec_res);                                 \
    str << val;                                                                \
    ASSERT_EQ(str.str(), EXPECTED);                                            \
  } while (false)

TEST_F(exec_test, builtins) {
  ASSERT_EXEC_RES_EQUALS("true", "true");
  ASSERT_EXEC_RES_EQUALS("false", "false");
  ASSERT_EXEC_RES_EQUALS("null", "null");
  ASSERT_EXEC_RES_EQUALS("undefined", "undefined");
}
TEST_F(exec_test, literals) {
  ASSERT_EXEC_RES_EQUALS("1", "1");
  ASSERT_EXEC_RES_EQUALS("0xf", "15");
  ASSERT_EXEC_RES_EQUALS("\"abc\"", "\"abc\"");
  ASSERT_EXEC_RES_EQUALS("'abc'", "\"abc\"");
  ASSERT_EXEC_RES_EQUALS("`abc`", "\"abc\"");
  ASSERT_EXEC_RES_EQUALS("[1,2,3]", "[1, 2, 3]");
  ASSERT_EXEC_RES_EQUALS("1,2", "2");
  ASSERT_EXEC_RES_EQUALS("~1", "-2");
}
TEST_F(exec_test, binops) {
  ASSERT_EXEC_RES_EQUALS("1+1", "2");
  ASSERT_EXEC_RES_EQUALS("1-1", "0");
  ASSERT_EXEC_RES_EQUALS("3*2", "6");
  ASSERT_EXEC_RES_EQUALS("4/2", "2");
  ASSERT_EXEC_RES_EQUALS("5%3", "2");
  ASSERT_EXEC_RES_EQUALS("2**3", "8");
  ASSERT_EXEC_RES_EQUALS("1 + 2**3 / 4 * 7", "15");
}
TEST_F(exec_test, comparisons) {
  ASSERT_EXEC_RES_EQUALS("2<3", "true");
  ASSERT_EXEC_RES_EQUALS("2<2", "false");
  ASSERT_EXEC_RES_EQUALS("2<1", "false");

  ASSERT_EXEC_RES_EQUALS("2<=3", "true");
  ASSERT_EXEC_RES_EQUALS("2<=2", "true");
  ASSERT_EXEC_RES_EQUALS("2<=1", "false");

  ASSERT_EXEC_RES_EQUALS("2>3", "false");
  ASSERT_EXEC_RES_EQUALS("2>2", "false");
  ASSERT_EXEC_RES_EQUALS("2>1", "true");

  ASSERT_EXEC_RES_EQUALS("2>=3", "false");
  ASSERT_EXEC_RES_EQUALS("2>=2", "true");
  ASSERT_EXEC_RES_EQUALS("2>=1", "true");
}
TEST_F(exec_test, value_isa) {
  ASSERT_TRUE(isa<number_value>(exec_value(number_value{0.})));
  ASSERT_FALSE(isa<number_value>(exec_value(string_value{""})));
}