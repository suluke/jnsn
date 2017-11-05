#include "lex_utils.h"
#include "gtest/gtest.h"
#include <initializer_list>

using namespace parsing;
using namespace std;

class string_table_test : public ::testing::Test {
protected:
  string_table str_table;
};

TEST_F(string_table_test, keywords) {
#define KEYWORD(NAME) const auto NAME##_handle = str_table.get_handle(#NAME);
#include "parsing/keywords.def"
#define KEYWORD(NAME) const auto NAME##_handle2 = str_table.get_handle(#NAME);
#include "parsing/keywords.def"
#define KEYWORD(NAME) ASSERT_EQ(NAME##_handle.data(), NAME##_handle2.data());
#include "parsing/keywords.def"
}

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
      ASSERT_TRUE(std::holds_alternative<token>(res))                          \
          << "Details:\n  Actual: " << res << "\nExpected: " << expected;      \
      auto tok = std::get<token>(res);                                         \
      ASSERT_EQ(tok.type, expected.type);                                      \
      ASSERT_EQ(tok.text, expected.text);                                      \
    }                                                                          \
    auto res = lexer.next();                                                   \
    ASSERT_TRUE(std::holds_alternative<lexer_base::eof_t>(res));               \
  } while (false)
// end define TOKEN_SEQUENCE(INPUT, ...)

#define SINGLE_NOTEXT_TOKEN(INPUT, TYPE) TOKEN_SEQUENCE(INPUT, TOKEN(TYPE, ""))

#define INPUT_IS_TOKEN_TEXT(INPUT, TYPE)                                       \
  TOKEN_SEQUENCE(INPUT, TOKEN(TYPE, INPUT))

#define LEXER_ERROR_AFTER(INPUT, SKIPPED)                                      \
  do {                                                                         \
    lexer.set_text(INPUT);                                                     \
    for (int i = 0; i < SKIPPED; ++i) {                                        \
      auto skipped = lexer.next();                                             \
      ASSERT_FALSE(std::holds_alternative<lexer_error>(skipped));              \
      ASSERT_FALSE(std::holds_alternative<lexer_base::eof_t>(skipped));        \
    }                                                                          \
    auto res = lexer.next();                                                   \
    ASSERT_TRUE(std::holds_alternative<lexer_error>(res)) << "was: " << res;   \
  } while (false)
#define LEXER_ERROR(INPUT) LEXER_ERROR_AFTER(INPUT, 0)

#define TOKEN(TYPE, TEXT) lexer.make_token(token_type::TYPE, TEXT)

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

  TOKEN_SEQUENCE("0;", TOKEN(INT_LITERAL, "0"), TOKEN(SEMICOLON, ""));

  LEXER_ERROR("00");
  LEXER_ERROR("01");
  LEXER_ERROR("01.2");
  LEXER_ERROR("1e");
  LEXER_ERROR("1e+");
  LEXER_ERROR("1e-");
}

TEST_F(lexer_test, comments) {
  TOKEN_SEQUENCE("abc // test", TOKEN(IDENTIFIER, "abc"),
                 TOKEN(LINE_COMMENT, "// test"));
  TOKEN_SEQUENCE("abc /* test */", TOKEN(IDENTIFIER, "abc"),
                 TOKEN(BLOCK_COMMENT, "/* test */"));
}

TEST_F(lexer_test, operators) {
#define TOKEN_TYPE(NAME, STR)                                                  \
  if (string{STR} != "" && string{STR} != ".") {                               \
    SINGLE_NOTEXT_TOKEN(STR, NAME);                                            \
  }
#include "parsing/tokens.def"
}

TEST_F(lexer_test, strings) {
  INPUT_IS_TOKEN_TEXT("\"ABCDE\"", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'ABCDE'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'ABCDE\\'\\\\'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\"'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("\"\'\"", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\\xff'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\\xFF'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\\uabcd'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\\uABCD'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\\u{abcde}'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\\u{aBcDe}'", STRING_LITERAL);
  INPUT_IS_TOKEN_TEXT("'\\u{abc}\\uabcd\\xababc'", STRING_LITERAL);
  LEXER_ERROR("'\\xff");
  LEXER_ERROR("'\\x'");
  LEXER_ERROR("'\\xa'");
  LEXER_ERROR("'\\u'");
  LEXER_ERROR("'\\ua'");
  LEXER_ERROR("'\\uab'");
  LEXER_ERROR("'\\uabc'");
  LEXER_ERROR("'\\u{abc'");
  LEXER_ERROR("'\\u{abc\n}'");
  LEXER_ERROR("'\\xij'");
  LEXER_ERROR("'\\uhijk'");
  LEXER_ERROR("'\\u{hij}'");
}

TEST_F(lexer_test, templates) {
  TOKEN_SEQUENCE("`\\\n`", TOKEN(TEMPLATE_STRING, "``")); // This is a weird one
  INPUT_IS_TOKEN_TEXT("`\\u{abc}\\uabcd\\xababc`", TEMPLATE_STRING);
  LEXER_ERROR("`");
  LEXER_ERROR_AFTER("`${", 1);
  TOKEN_SEQUENCE("`${A}`", TOKEN(TEMPLATE_HEAD, "`${"), TOKEN(IDENTIFIER, "A"),
                 TOKEN(TEMPLATE_END, "}`"));
  TOKEN_SEQUENCE("`${A}${B}`", TOKEN(TEMPLATE_HEAD, "`${"),
                 TOKEN(IDENTIFIER, "A"), TOKEN(TEMPLATE_MIDDLE, "}${"),
                 TOKEN(IDENTIFIER, "B"), TOKEN(TEMPLATE_END, "}`"));
  TOKEN_SEQUENCE("`${'A' + `${B}`}`", TOKEN(TEMPLATE_HEAD, "`${"),
                 TOKEN(STRING_LITERAL, "'A'"), TOKEN(PLUS, ""),
                 TOKEN(TEMPLATE_HEAD, "`${"), TOKEN(IDENTIFIER, "B"),
                 TOKEN(TEMPLATE_END, "}`"), TOKEN(TEMPLATE_END, "}`"));
}

TEST_F(lexer_test, regex) {
  INPUT_IS_TOKEN_TEXT("/abc/", REGEX_LITERAL);
  TOKEN_SEQUENCE("1/23/4", TOKEN(INT_LITERAL, "1"), TOKEN(SLASH, ""),
                 TOKEN(INT_LITERAL, "23"), TOKEN(SLASH, ""),
                 TOKEN(INT_LITERAL, "4"));
}

TEST_F(lexer_test, big1) {
  const auto prog = R"delim(
    /* Test */
    function test() {
      for (let i = 0; i < 10; ++i) {
        console.log((i + 1) * 1e1);
      }
    }
    test();
    // END
  )delim";
  TOKEN_SEQUENCE(
      prog, TOKEN(BLOCK_COMMENT, "/* Test */"), TOKEN(KEYWORD, "function"),
      TOKEN(IDENTIFIER, "test"), TOKEN(PAREN_OPEN, ""), TOKEN(PAREN_CLOSE, ""),
      TOKEN(BRACE_OPEN, ""), TOKEN(KEYWORD, "for"), TOKEN(PAREN_OPEN, ""),
      TOKEN(KEYWORD, "let"), TOKEN(IDENTIFIER, "i"), TOKEN(EQ, ""),
      TOKEN(INT_LITERAL, "0"), TOKEN(SEMICOLON, ""), TOKEN(IDENTIFIER, "i"),
      TOKEN(LT, ""), TOKEN(INT_LITERAL, "10"), TOKEN(SEMICOLON, ""),
      TOKEN(INCR, ""), TOKEN(IDENTIFIER, "i"), TOKEN(PAREN_CLOSE, ""),
      TOKEN(BRACE_OPEN, ""), TOKEN(IDENTIFIER, "console"), TOKEN(DOT, ""),
      TOKEN(IDENTIFIER, "log"), TOKEN(PAREN_OPEN, ""), TOKEN(PAREN_OPEN, ""),
      TOKEN(IDENTIFIER, "i"), TOKEN(PLUS, ""), TOKEN(INT_LITERAL, "1"),
      TOKEN(PAREN_CLOSE, ""), TOKEN(ASTERISK, ""), TOKEN(INT_LITERAL, "1e1"),
      TOKEN(PAREN_CLOSE, ""), TOKEN(SEMICOLON, ""), TOKEN(BRACE_CLOSE, ""),
      TOKEN(BRACE_CLOSE, ""), TOKEN(IDENTIFIER, "test"), TOKEN(PAREN_OPEN, ""),
      TOKEN(PAREN_CLOSE, ""), TOKEN(SEMICOLON, ""),
      TOKEN(LINE_COMMENT, "// END"));
}

TEST_F(lexer_test, keyword_types) {
#define KEYWORD(NAME)                                                          \
  ASSERT_EQ(keyword_type::kw_##NAME,                                           \
            lexer_base::get_keyword_type(TOKEN(KEYWORD, #NAME)));
#include "parsing/keywords.def"
}
