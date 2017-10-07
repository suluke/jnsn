#include "parsing/lexer.h"
#include <algorithm>
#include <cctype>

namespace parsing {

using unit = lexer_base::unit;
using result = lexer_base::result;

std::ostream &operator<<(std::ostream &stream, const token_type ty) {
#define TOKEN_TYPE(NAME, STR)                                                  \
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
std::ostream &operator<<(std::ostream &stream, const lexer_error &e) {
  stream << "ERROR: " << e.msg << " (at " << e.loc << ")";
  return stream;
}
template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...)->overloaded<Ts...>;
std::ostream &operator<<(std::ostream &stream, const lexer_base::result &res) {
  std::visit(overloaded{[&stream](lexer_base::eof_t) { stream << "EOF"; },
                        [&stream](lexer_error e) { stream << e; },
                        [&stream](token t) { stream << t; }},
             res);
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
  if (!window[1]) {
    return eof_t{};
  }
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
    if (next_unit() && *next_unit() == '.') {
      advance();
      if (!next_unit() || *next_unit() != '.') {
        return lexer_error{"Unexpected char after '..'. Expected third dot.",
                           loc};
      }
      advance();
      return token{token_type::DOTDOTDOT, {}, {}};
    } else if (!prev || prev->type != token_type::IDENTIFIER) {
      return lex_number();
    } else {
      return token{token_type::DOT, {}, {}};
    }
  }
  return lexer_error{"Not implemented", loc};
}

/// This macro helps implementing binary, octal and hex literals with
/// minimal code duplication
#define LEX_SPECIAL_BASE_INT(NAME, PREFIX, TYPE, IS_DIGIT)                     \
  do { /* idiomatic do-while-false-wrapper */                                  \
    text << PREFIX;                                                            \
    advance(); /* now points to second char of prefix */                       \
    if (!next_unit() || !IS_DIGIT(*next_unit())) {                             \
      return lexer_error{NAME " literal must have digits after " PREFIX, loc}; \
    }                                                                          \
    do {                                                                       \
      advance();                                                               \
      text << current();                                                       \
    } while (next_unit() && IS_DIGIT(*next_unit()));                           \
    return token{token_type::TYPE, str_table.get_handle(text.str()), {}};      \
  } while (false)

result lexer_base::lex_hex_int() {
  LEX_SPECIAL_BASE_INT("Hex", "0x", HEX_LITERAL, std::isxdigit);
}
result lexer_base::lex_bin_int() {
  LEX_SPECIAL_BASE_INT("Binary", "0b", BIN_LITERAL,
                       [](unit u) { return u == '0' || u == '1'; });
}
result lexer_base::lex_oct_int() {
  LEX_SPECIAL_BASE_INT("Octal", "0o", OCT_LITERAL,
                       [](unit u) { return u >= '0' && u <= '7'; });
}

result lexer_base::lex_number() {
  // this method can be entered either by having a leading digit or a
  // leading dot
  token_type ty = token_type::INT_LITERAL;
  if (current() != '.') { // we got a digit
    if (!next_unit()) {
      text << current();
      return token{
          token_type::INT_LITERAL, str_table.get_handle(text.str()), {}};
    }
    if (current() == '0') {
      if (*next_unit() == '.') {
        // fall through
      } else if (*next_unit() == 'x' || *next_unit() == 'X') {
        return lex_hex_int();
      } else if (*next_unit() == 'b' || *next_unit() == 'B') {
        return lex_bin_int();
      } else if (*next_unit() == 'o' || *next_unit() == 'O') {
        return lex_oct_int();
      } else if (!std::isspace(*next_unit())) {
        return lexer_error{"Tokens beginning with '0' must be followed by '.', "
                           "'x', 'b', 'o' or spaces",
                           loc};
      }
    }
    text << current();
    // consume remaining leading digits
    while (next_unit() && std::isdigit(*next_unit())) {
      advance();
      text << current();
    }
    // a dot will also be part of the number
    if (next_unit() && *next_unit() == '.') {
      advance();
    }
  } else if (!next_unit() || !std::isdigit(*next_unit())) {
    return lexer_error{"Expected number, but no digits after leading dot ('.')",
                       loc};
  }
  // now we're looking at a leading dot (if it exists)
  if (current() == '.') {
    ty = token_type::FLOAT_LITERAL;
    text << current();
    // consume decimal places
    while (next_unit() && std::isdigit(*next_unit())) {
      advance();
      text << current();
    }
  }
  if (!next_unit()) {
    return token{ty, str_table.get_handle(text.str()), {}};
  }
  if (*next_unit() == 'e' || *next_unit() == 'E') {
    advance();
    text << current();
    if (!next_unit() || (!std::isdigit(*next_unit()) && *next_unit() != '+' &&
                         *next_unit() != '-')) {
      return lexer_error{"Missing digits after exponent part of number literal",
                         loc};
    }
    if (*next_unit() == '-') {
      ty = token_type::FLOAT_LITERAL;
    }
    if (*next_unit() == '-' || *next_unit() == '+') {
      advance(); // consume sign
      text << current();
    }
    if (!next_unit() || !std::isdigit(*next_unit())) {
      return lexer_error{
          "Missing digits after exponent part's sign of number literal", loc};
    }
    // consume exponent
    while (next_unit() && std::isdigit(*next_unit())) {
      advance();
      text << current();
    }
  }
  return token{ty, str_table.get_handle(text.str()), {}};
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
  while (next_unit() && (std::isalnum(*next_unit()) || *next_unit() == '_' ||
                         *next_unit() == '$' || *next_unit() == '\\')) {
    // TODO backslash may be used to start unicode id sequence, so we
    // should dispatch to some method that can handle that correctly
    advance();
    text << current();
  }
  auto str = str_table.get_handle(text.str());
  if (is_keyword(str)) {
    return token{token_type::KEYWORD, str, {}};
  } else {
    return token{token_type::IDENTIFIER, str, {}};
  }
}
} // namespace parsing
