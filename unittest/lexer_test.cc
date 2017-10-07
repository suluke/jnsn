#include "parsing/lexer.h"
#include "gtest/gtest.h"

using namespace parsing;

///
///
class constant_string_lexer : public lexer_base {
  const char *it;
  read_t read_unit() override {
    read_t res;
    if (*it != '\0') {
      res = {*it};
      ++it;
    }
    return res;
  }

public:
  void set_text(const char *text) {
    reset();
    it = text;
  }
};

class lexer_test : public ::testing::Test {
protected:
  constant_string_lexer lexer;
};

TEST_F(lexer_test, numbers) {
#define SINGLE_TOKEN(TEXT, TYPE)                                               \
  do {                                                                         \
    lexer.set_text(TEXT);                                                      \
    auto res = lexer.next();                                                   \
    ASSERT_TRUE(std::holds_alternative<token>(res)) << "was: " << res;         \
    auto tok = std::get<token>(res);                                           \
    ASSERT_EQ(tok.type, token_type::TYPE);                                     \
    ASSERT_EQ(tok.text, TEXT);                                                 \
    res = lexer.next();                                                        \
    ASSERT_TRUE(std::holds_alternative<lexer_base::eof_t>(res))                \
        << "was: " << res;                                                     \
  } while (false)

  SINGLE_TOKEN("123", INT_LITERAL);
  SINGLE_TOKEN("123.", FLOAT_LITERAL);
  SINGLE_TOKEN("123.e1", FLOAT_LITERAL);
  SINGLE_TOKEN("123e1", INT_LITERAL);
  SINGLE_TOKEN("123e+1", INT_LITERAL);
  SINGLE_TOKEN("123e-1", FLOAT_LITERAL);
  SINGLE_TOKEN(".123", FLOAT_LITERAL);
  SINGLE_TOKEN("0x123", HEX_LITERAL);
  SINGLE_TOKEN("0o123", OCT_LITERAL);
  SINGLE_TOKEN("0b010", BIN_LITERAL);
}