#ifndef JNSN_JS_LEXER_H
#define JNSN_JS_LEXER_H

#include "jnsn/source_location.h"
#include "jnsn/string_table.h"
#include <cassert>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <variant>

namespace jnsn {

enum class token_type {
#define TOKEN_TYPE(NAME, STR) NAME,
#include "jnsn/js/tokens.def"
};
std::ostream &operator<<(std::ostream &stream, const token_type ty);

enum class keyword_type {
#define KEYWORD(NAME) kw_##NAME,
#include "jnsn/js/keywords.def"
};

///
///
struct token {
  token_type type;
  string_table::entry text;
  source_location loc;

  friend std::ostream &operator<<(std::ostream &stream, const token &tok);
  bool is_number_literal() {
    return this->type == token_type::INT_LITERAL ||
           this->type == token_type::HEX_LITERAL ||
           this->type == token_type::OCT_LITERAL ||
           this->type == token_type::BIN_LITERAL ||
           this->type == token_type::FLOAT_LITERAL;
  }
};

///
///
struct lexer_error {
  std::string msg;
  source_location loc;
  friend std::ostream &operator<<(std::ostream &stream, const lexer_error &e);
};

///
///
class lexer_base {
public:
  using unit = char;
  using eof_t = std::monostate;
  using read_t = std::optional<unit>;
  using result = std::variant<eof_t, lexer_error, token>;
  using window_t = std::array<read_t, 2>;

private:
  source_location loc;
  std::stringstream text;
  string_table str_table;
  window_t window;
  std::optional<token> prev;
  size_t template_depth = 0;

  unit current() { return *window[0]; }
  read_t peek() { return window[1]; }

  virtual read_t read_unit() = 0;

  bool eof();
  void advance();

  result lex_alnum();
  result lex_punct();
  result lex_number();
  result lex_id_keyword();
  result lex_dot();
  result lex_line_comment();
  result lex_block_comment();
  result lex_hex_int();
  result lex_bin_int();
  result lex_oct_int();
  result lex_eq();
  result lex_plus();
  result lex_minus();
  result lex_asterisk();
  result lex_slash();
  result lex_percent();
  result lex_exclamation();
  result lex_caret();
  result lex_lt();
  result lex_gt();
  result lex_ampersand();
  result lex_vert_bar();
  result lex_str();
  result lex_backtick();
  result lex_regex();
  result lex_closing_brace();

  std::optional<lexer_error> consume_escape_seq();

public:
  lexer_base() : window({' ', '\n'}) {}
  const result next();
  void reset() {
    window = {' ', '\n'};
    loc = {};
    template_depth = 0;
  }
  token make_token(token_type, const char *text);
  static keyword_type get_keyword_type(const token &);
};

std::ostream &operator<<(std::ostream &stream, const lexer_base::result &res);

///
///
class cin_line_lexer : public lexer_base {
  std::string line;
  std::string::iterator it;
  read_t read_unit() override {
    read_t res;
    if (it != line.end()) {
      res = {*it};
      ++it;
    }
    return res;
  }

public:
  cin_line_lexer() {
    std::getline(std::cin, line);
    it = line.begin();
  }
};
} // namespace jnsn

#endif // JNSN_JS_LEXER_H
