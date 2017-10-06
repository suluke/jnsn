#ifndef PARSING_LEXER_H
#define PARSING_LEXER_H

#include "parsing/source_location.h"
#include "parsing/string_table.h"
#include <cassert>
#include <deque>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <variant>

namespace parsing {

enum class token_type {
  DOT,
  KEYWORD,
  IDENTIFIER,
  INT_LITERAL,
  FLOAT_LITERAL,
  MINUS,
  PLUS,
  PAREN_OPEN,
  PAREN_CLOSE,
  BRACE_OPEN,
  BRACE_CLOSE,
  BRACKET_OPEN,
  BRACKET_CLOSE
};
///
///
struct token {
  token_type type;
  string_table::entry text;
  source_location loc;
};

///
///
struct lexer_error {
  std::string msg;
  source_location loc;
};

///
///
class lexer_base {
public:
  using unit = char;
  using read_t = std::optional<unit>;
  using result = std::variant<std::monostate, lexer_error, token>;

private:
  std::deque<unit> history;
  source_location loc;
  std::stringstream text;
  string_table str_table;

  virtual read_t read_unit() = 0;

  bool is_eof();
  unit consume_unit();
  void unconsume_unit(unit u);
  source_location prev_location() {
    auto prev = loc;
    prev.rewind(' ');
    return prev;
  }

  result lex_alnum(unit u);
  result lex_punct(unit u);
  result lex_number(unit u);
  result lex_id_keyword(unit u);

public:
  result next();
};

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
    std::cin >> line;
    it = line.begin();
  }
};
} // namespace parsing

#endif
