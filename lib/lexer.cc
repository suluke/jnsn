#include "parsing/lexer.h"
#include <algorithm>
#include <cctype>

namespace parsing {

using unit = lexer_base::unit;
using result = lexer_base::result;

std::ostream &operator<<(std::ostream &stream, const token_type ty) {
#define TOKEN_TYPE(NAME)                                                       \
  if (ty == token_type::NAME)                                                  \
    stream << #NAME;
#include "parsing/tokens.def"
  return stream;
}
std::ostream &operator<<(std::ostream &stream, const token &tok) {
  stream << tok.type;
  if (!tok.text.empty()) {
    stream << " (\"" << tok.text << "\" at " << tok.loc << ")";
  }
  return stream;
}

bool lexer_base::eof() { return !window[0]; }

void lexer_base::advance() {
  assert(!eof() && "Read after eof");
  loc.advance(current()); // works because not eof
  std::rotate(window.begin(), window.begin() + 1, window.end());
  window.back() = read_unit();
}

result lexer_base::next() {
  // Skip whitespace
  unit u;
  do {
    // we always have to advance because we expect that our predecessor
    // has forgotten
    advance();
    if (eof()) {
      return {};
    }
    u = current();
    if (std::iscntrl(u) && !std::isspace(u)) {
      auto loc = this->loc;
      return lexer_error{"Found junk", loc};
    }
  } while (!std::isgraph(u));
  auto start_loc = loc;
  text.str("");
  text.clear();
  result res;
  // Dispatch to more concrete lexing functions
  if (std::isalnum(u) || u == '_' || u == '$') {
    res = lex_alnum();
  } else if (std::ispunct(u)) {
    res = lex_punct();
  } else {
    // FIXME make this more sophisticated for non-ascii characters
    return lexer_error{"Cannot handle character", start_loc};
  }
  if (auto *T = std::get_if<token>(&res)) {
    T->loc = start_loc;
    prev = *T;
  }
  return res;
}

result lexer_base::lex_alnum() {
  if (std::isdigit(current())) {
    return lex_number();
  } else {
    return lex_id_keyword();
  }
}
result lexer_base::lex_punct() {
  if (current() == '.') {
    if (!prev || prev->type != token_type::IDENTIFIER) {
      return lex_float();
    } else {
      return token{token_type::DOT, {}, {}};
    }
  }
  return lexer_error{"Not implemented", loc};
}

result lexer_base::lex_float() { return lexer_error{"Not implemented", loc}; }

/// This macro helps implementing binary, octal and hex literals with
/// minimal code duplication
#define LEX_SPECIAL_BASE_INT(NAME, PREFIX, TYPE, IS_DIGIT)                     \
  text << PREFIX;                                                              \
  advance();                                                                   \
  auto next = window[1];                                                       \
  if (!next || !IS_DIGIT(*next)) {                                             \
    return lexer_error{NAME " literal must have digits after " PREFIX, loc};   \
  }                                                                            \
  do {                                                                         \
    advance();                                                                 \
    text << current();                                                         \
    next = window[1];                                                          \
  } while (next && IS_DIGIT(*next));                                           \
  return token{token_type::TYPE, str_table.get_handle(text.str()), {}};

result lexer_base::lex_hex_int() {
  LEX_SPECIAL_BASE_INT("Hex", "0x", HEX_LITERAL, std::isxdigit)
}
result lexer_base::lex_bin_int() {
  LEX_SPECIAL_BASE_INT("Binary", "0b", BIN_LITERAL, [](unit u) { return u == '0' || u == '1'; })
}
result lexer_base::lex_oct_int() {
  LEX_SPECIAL_BASE_INT("Octal", "0o", OCT_LITERAL, [](unit u) { return u >= '0' && u <= '7'; })
}

result lexer_base::lex_number() {
  if (!window[1]) {
    text << current();
    return token{token_type::INT_LITERAL, str_table.get_handle(text.str()), {}};
  }
  if (current() == '0') {
    auto next = *window[1];
    if (next == '.') {
      return lex_float();
    } else if (next == 'x' || next == 'X') {
      return lex_hex_int();
    } else if (next == 'b' || next == 'B') {
      return lex_bin_int();
    } else if (next == 'o' || next == 'O') {
      return lex_oct_int();
    }
    // token that starts with 0 and isn't one of the above must end here
    text << current();
    return token{token_type::INT_LITERAL, str_table.get_handle(text.str()), {}};
  }
  return lexer_error{"Not implemented", loc};
}

static bool is_keyword(string_table::entry word) {
#define KEYWORD(NAME)                                                          \
  if (word == #NAME)                                                           \
    return true;
#include "parsing/keywords.def"
  return false;
}

result lexer_base::lex_id_keyword() {
  text << current();
  auto next = window[1];
  while (next && (std::isalnum(*next) || *next == '_' || *next == '$' ||
                  *next == '\\')) {
    // TODO backslash may be used to start unicode id sequence, so we
    // should dispatch to some method that can handle that correctly
    advance();
    text << current();
    next = window[1];
  }
  auto str = str_table.get_handle(text.str());
  if (is_keyword(str)) {
    return token{token_type::KEYWORD, str, {}};
  } else {
    return token{token_type::IDENTIFIER, str, {}};
  }
}
} // namespace parsing
