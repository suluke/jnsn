#include "parsing/lexer.h"
#include "gtest/gtest.h"
#include <initializer_list>

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

#define TOKEN_SEQUENCE(INPUT, ...)                                             \
  do {                                                                         \
    std::initializer_list<token> expected_tokens = {__VA_ARGS__};              \
    lexer.set_text(INPUT);                                                     \
    for (auto &expected : expected_tokens) {                                   \
      auto res = lexer.next();                                                 \
      ASSERT_TRUE(std::holds_alternative<token>(res)) << "was: " << res;       \
      auto tok = std::get<token>(res);                                         \
      ASSERT_EQ(tok.type, expected.type);                                      \
      ASSERT_EQ(tok.text, expected.text);                                      \
    }                                                                          \
    auto res = lexer.next();                                                   \
    ASSERT_TRUE(std::holds_alternative<lexer_base::eof_t>(res));               \
  } while (false)
// end define TOKEN_SEQUENCE(INPUT, ...)

#define SINGLE_NOTEXT_TOKEN(INPUT, TYPE)                                       \
  TOKEN_SEQUENCE(INPUT, token{token_type::TYPE, "", {}})

#define INPUT_IS_TOKEN_TEXT(INPUT, TYPE)                                       \
  TOKEN_SEQUENCE(INPUT, token{token_type::TYPE, INPUT, {}})

#define LEXER_ERROR(INPUT)                                                     \
  do {                                                                         \
    lexer.set_text(INPUT);                                                     \
    auto res = lexer.next();                                                   \
    ASSERT_TRUE(std::holds_alternative<lexer_error>(res)) << "was: " << res;   \
  } while (false)

#define TOKEN(TYPE, TEXT)                                                      \
  token {                                                                      \
    token_type::TYPE, TEXT, {}                                                 \
  }

TEST_F(lexer_test, identifiers) {
  INPUT_IS_TOKEN_TEXT("abc", IDENTIFIER);
  INPUT_IS_TOKEN_TEXT("$abc", IDENTIFIER);
  INPUT_IS_TOKEN_TEXT("_abc", IDENTIFIER);
  INPUT_IS_TOKEN_TEXT("abc\\", IDENTIFIER); // FIXME
}

TEST_F(lexer_test, numbers) {
  INPUT_IS_TOKEN_TEXT("123", INT_LITERAL);
  INPUT_IS_TOKEN_TEXT("123.", FLOAT_LITERAL);
  INPUT_IS_TOKEN_TEXT("123.e1", FLOAT_LITERAL);
  INPUT_IS_TOKEN_TEXT("123e1", INT_LITERAL);
  INPUT_IS_TOKEN_TEXT("123e+1", INT_LITERAL);
  INPUT_IS_TOKEN_TEXT("123e-1", FLOAT_LITERAL);
  INPUT_IS_TOKEN_TEXT(".123", FLOAT_LITERAL);
  INPUT_IS_TOKEN_TEXT("0x123", HEX_LITERAL);
  INPUT_IS_TOKEN_TEXT("0o123", OCT_LITERAL);
  INPUT_IS_TOKEN_TEXT("0b010", BIN_LITERAL);

  LEXER_ERROR("00");
  LEXER_ERROR("01");
  LEXER_ERROR("01.2");
  LEXER_ERROR("1e");
  LEXER_ERROR("1e+");
  LEXER_ERROR("1e-");
  // Dots not preceded by an identifier are interpreted as the start of a
  // number
  LEXER_ERROR(".");
}

TEST_F(lexer_test, comments) {
  TOKEN_SEQUENCE("abc // test", TOKEN(IDENTIFIER, "abc"),
                 TOKEN(LINE_COMMENT, "// test"));
  TOKEN_SEQUENCE("abc /* test */", TOKEN(IDENTIFIER, "abc"),
                 TOKEN(BLOCK_COMMENT, "/* test */"));
}

TEST_F(lexer_test, operators) {
#define TOKEN_TYPE(NAME, STR)                                                  \
  if (STR != "" && STR != ".") {                                               \
    SINGLE_NOTEXT_TOKEN(STR, NAME);                                            \
  }
#include "parsing/tokens.def"
}