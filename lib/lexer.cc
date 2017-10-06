#include "parsing/lexer.h"
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
  return stream;
}

bool lexer_base::is_eof() {
  if (!history.empty()) {
    return false;
  }
  auto res = read_unit();
  if (res) {
    history.push_back(*res);
    return false;
  }
  return true;
}

unit lexer_base::consume_unit() {
  assert(!is_eof() && "Read after eof");
  unit u;
  if (!history.empty()) {
    u = history.front();
    history.pop_front();
  } else {
    auto res = read_unit();
    assert(res && "Not eof but eof(?)");
    u = *res;
  }
  loc.advance(u);
  return u;
}

void lexer_base::unconsume_unit(unit u) {
  history.push_front(u);
  loc.rewind(u);
}

result lexer_base::next() {
  unit u;
  // Skip whitespace
  do {
    if (is_eof()) {
      return {};
    }
    u = consume_unit();
    if (std::iscntrl(u) && !std::isspace(u)) {
      auto loc = this->loc;
      loc.rewind(u);
      return lexer_error{"Found junk", loc};
    }
  } while (!std::isgraph(u));
  auto start_loc = prev_location();
  text.str("");
  text.clear();
  result res;
  // Dispatch to more concrete lexing functions
  if (std::isalnum(u) || u == '_') {
    res = lex_alnum(u);
  } else if (std::ispunct(u)) {
    res = lex_punct(u);
  } else {
    // FIXME make this more sophisticated for non-ascii characters
    return lexer_error{"Cannot handle character", start_loc};
  }
  if (auto *T = std::get_if<token>(&res)) {
    T->loc = start_loc;
  }
  return res;
}

result lexer_base::lex_alnum(unit u) {
  if (std::isdigit(u)) {
    return lex_number(u);
  } else {
    return lex_id_keyword(u);
  }
}
result lexer_base::lex_punct(unit u) {
  return lexer_error{"Not implemented", loc};
}
result lexer_base::lex_number(unit u) {
  text << u;
  if (is_eof()) {
    return token{token_type::INT_LITERAL, str_table.get_handle(text.str()),
                 prev_location()};
  }
  if (u == '0') {
  }
  return lexer_error{"Not implemented", loc};
}

static bool is_keyword(string_table::entry word) {
  return true; // FIXME
}

result lexer_base::lex_id_keyword(unit u) {
  do {
    text << u;
    if (is_eof()) {
      break;
    }
    u = consume_unit();
  } while (std::isalnum(u) || u == '_');
  if (!is_eof()) {
    unconsume_unit(u);
  }
  auto str = str_table.get_handle(text.str());
  if (is_keyword(str)) {
    return token{token_type::KEYWORD, str, {}};
  } else {
    return token{token_type::IDENTIFIER, str, {}};
  }
}
} // namespace parsing
