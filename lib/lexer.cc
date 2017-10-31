#include "parsing/lexer.h"
#include "parsing/util.h"
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

/// keywords impl
static bool is_keyword(string_table::entry word) {
#define KEYWORD(NAME)                                                          \
  if (word == #NAME)                                                           \
    return true;
#include "parsing/keywords.def"
  return false;
}

#define KEYWORD(NAME) static const char *kw_##NAME##_str = #NAME;
#include "parsing/keywords.def"

static const char *find_kw(std::string &s) {
#define KEYWORD(NAME)                                                          \
  if (s == #NAME)                                                              \
    return kw_##NAME##_str;
#include "parsing/keywords.def"
  return nullptr;
}

/// string_table impl
string_table::entry string_table::get_handle(std::string s) {
  if (const char *kw = find_kw(s)) {
    return {kw};
  }
  auto it_ins = table.insert(std::move(s));
  auto it = it_ins.first;
  return std::string_view{it->data(), it->size()};
}

static constexpr bool one_of(unit u, const unit *alternatives) {
  if (*alternatives == '\0') {
    return false;
  } else if (*alternatives == u) {
    return true;
  }
  return one_of(u, alternatives + 1);
}
static constexpr bool islineterminator(unit u) {
  return one_of(u, "\r\n"); // FIXME the spec has two more...
}

bool lexer_base::eof() { return !window[0]; }

void lexer_base::advance() {
  assert(!eof() && "Read after eof");
  loc.advance(current()); // works because not eof
  std::rotate(window.begin(), window.begin() + 1, window.end());
  window.back() = read_unit();
}

const result lexer_base::next() {
  if (!peek()) {
    if (template_depth != 0) {
      return lexer_error{"Unexpected EOF in template literal", loc};
    }
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
    return lex_dot();
  } else if (current() == '/') {
    return lex_slash();
  } else if (current() == ',') {
    return token{token_type::COMMA, {}, {}};
  } else if (current() == ';') {
    return token{token_type::SEMICOLON, {}, {}};
  } else if (current() == '=') {
    return lex_eq();
  } else if (current() == '+') {
    return lex_plus();
  } else if (current() == '-') {
    return lex_minus();
  } else if (current() == '*') {
    return lex_asterisk();
  } else if (current() == '/') {
    return lex_slash();
  } else if (current() == '%') {
    return lex_percent();
  } else if (current() == '!') {
    return lex_exclamation();
  } else if (current() == '?') {
    return token{token_type::QMARK, {}, {}};
  } else if (current() == ':') {
    return token{token_type::COLON, {}, {}};
  } else if (current() == '~') {
    return token{token_type::TILDE, {}, {}};
  } else if (current() == '^') {
    return lex_caret();
  } else if (current() == '<') {
    return lex_lt();
  } else if (current() == '>') {
    return lex_gt();
  } else if (current() == '&') {
    return lex_ampersand();
  } else if (current() == '|') {
    return lex_vert_bar();
  } else if (current() == '(') {
    return token{token_type::PAREN_OPEN, {}, {}};
  } else if (current() == ')') {
    return token{token_type::PAREN_CLOSE, {}, {}};
  } else if (current() == '{') {
    return token{token_type::BRACE_OPEN, {}, {}};
  } else if (current() == '}') {
    return lex_closing_brace();
  } else if (current() == '[') {
    return token{token_type::BRACKET_OPEN, {}, {}};
  } else if (current() == ']') {
    return token{token_type::BRACKET_CLOSE, {}, {}};
  } else if (current() == '\'' || current() == '"') {
    return lex_str();
  } else if (current() == '`') {
    return lex_backtick();
  }
  return lexer_error{"Unknown punctuation character", loc};
}

result lexer_base::lex_dot() {
  if (!peek()) {
    return token{token_type::DOT, {}, {}};
  }
  if (*peek() == '.') {
    advance();
    if (!peek() || *peek() != '.') {
      // Not sure if it makes sense to catch this here. We might also just
      // lex two DOT tokens instead, causing a syntax error later
      return lexer_error{"Unexpected char after '..'. Expected third dot.",
                         loc};
    }
    advance();
    return token{token_type::DOTDOTDOT, {}, {}};
  } else if (std::isdigit(*peek())) {
    return lex_number();
  }
  return token{token_type::DOT, {}, {}};
}

result lexer_base::lex_eq() {
  assert(current() == '=');
  if (!peek()) {
    return token{token_type::EQ, {}, {}};
  }
  if (peek() == '>') {
    advance();
    return token{token_type::ARROW, {}, {}};
  } else if (peek() == '=') {
    advance();
    if (!peek()) {
      return token{token_type::EQEQ, {}, {}};
    }
    if (*peek() != '=') {
      return token{token_type::EQEQ};
    }
    advance(); // move onto third '='
    return token{token_type::EQEQEQ, {}, {}};
  }
  return token{token_type::EQ, {}, {}};
}

result lexer_base::lex_plus() {
  assert(current() == '+');
  if (!peek()) {
    return token{token_type::PLUS, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    return token{token_type::PLUS_EQ, {}, {}};
  } else if (*peek() == '+') {
    advance();
    return token{token_type::INCR, {}, {}};
  }
  return token{token_type::PLUS, {}, {}};
}
result lexer_base::lex_minus() {
  assert(current() == '-');
  if (!peek()) {
    return token{token_type::MINUS, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    return token{token_type::MINUS_EQ, {}, {}};
  } else if (*peek() == '-') {
    advance();
    return token{token_type::DECR, {}, {}};
  }
  return token{token_type::MINUS, {}, {}};
}
result lexer_base::lex_asterisk() {
  assert(current() == '*');
  if (!peek()) {
    return token{token_type::ASTERISK, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    return token{token_type::MUL_EQ, {}, {}};
  } else if (*peek() == '*') {
    advance();
    if (!peek()) {
      return token{token_type::POW, {}, {}};
    }
    if (*peek() == '=') {
      advance();
      return token{token_type::POW_EQ, {}, {}};
    }
    return token{token_type::POW, {}, {}};
  }
  return token{token_type::ASTERISK, {}, {}};
}
result lexer_base::lex_slash() {
  assert(current() == '/');
  if (!peek()) {
    return token{token_type::SLASH, {}, {}};
  }
  if (*peek() == '/') {
    return lex_line_comment();
  } else if (*peek() == '*') {
    return lex_block_comment();
  } else if (!prev || (prev->type == token_type::KEYWORD &&
                       prev->type == token_type::IDENTIFIER &&
                       prev->type == token_type::INT_LITERAL &&
                       prev->type == token_type::FLOAT_LITERAL &&
                       prev->type == token_type::HEX_LITERAL &&
                       prev->type == token_type::OCT_LITERAL &&
                       prev->type == token_type::BIN_LITERAL)) {
    return lex_regex();
  } else if (*peek() == '=') {
    advance();
    return token{token_type::DIV_EQ, {}, {}};
  }
  return token{token_type::SLASH, {}, {}};
}

result lexer_base::lex_regex() {
  assert(current() == '/');
  auto start = loc;
  bool ended = false;
  while (peek()) {
    if (islineterminator(current())) {
      return lexer_error{"Unexpected line end in regex literal", loc};
    }
    text << current();
    if (current() == '\\') {
      advance();
      if (!peek()) {
        return lexer_error{"Unexpected EOF in regex literal", loc};
      }
      text << current();
    }
    if (peek() == '/') {
      advance();
      text << current();
      ended = true;
      break;
    }
    advance();
  }
  if (!ended) {
    return lexer_error{"Reached EOF while lexing regex literal", start};
  }
  return token{token_type::REGEX_LITERAL, str_table.get_handle(text.str()),
               start};
}

result lexer_base::lex_percent() {
  assert(current() == '%');
  if (!peek()) {
    return token{token_type::PERCENT, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    return token{token_type::MOD_EQ, {}, {}};
  }
  return token{token_type::PERCENT, {}, {}};
}

result lexer_base::lex_exclamation() {
  assert(current() == '!');
  if (!peek()) {
    return token{token_type::EXMARK, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    if (!peek() || *peek() != '=') {
      return token{token_type::NEQ, {}, {}};
    }
    advance();
    return token{token_type::NEQEQ, {}, {}};
  }
  return token{token_type::EXMARK, {}, {}};
}

result lexer_base::lex_caret() {
  assert(current() == '^');
  if (!peek()) {
    return token{token_type::CARET, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    return token{token_type::CARET_EQ, {}, {}};
  }
  return token{token_type::CARET, {}, {}};
}

result lexer_base::lex_lt() {
  assert(current() == '<');
  if (!peek()) {
    return token{token_type::LT, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    return token{token_type::LT_EQ, {}, {}};
  } else if (*peek() == '<') {
    advance();
    if (!peek()) {
      return token{token_type::LSHIFT, {}, {}};
    }
    if (*peek() == '=') {
      advance();
      return token{token_type::LSH_EQ, {}, {}};
    }
    return token{token_type::LSHIFT};
  }
  return token{token_type::LT, {}, {}};
}

result lexer_base::lex_gt() {
  assert(current() == '>');
  if (!peek()) {
    return token{token_type::GT, {}, {}};
  }
  if (*peek() == '=') {
    advance();
    return token{token_type::GT_EQ, {}, {}};
  } else if (*peek() == '>') {
    advance();
    if (!peek()) {
      return token{token_type::RSHIFT, {}, {}};
    }
    if (*peek() == '=') {
      advance();
      return token{token_type::RSH_EQ, {}, {}};
    } else if (*peek() == '>') {
      advance();
      if (!peek()) {
        return token{token_type::LOG_RSHIFT, {}, {}};
      }
      if (*peek() == '=') {
        advance();
        return token{token_type::LOG_RSH_EQ, {}, {}};
      }
      return token{token_type::LOG_RSHIFT, {}, {}};
    }
    return token{token_type::RSHIFT};
  }
  return token{token_type::LT, {}, {}};
}

result lexer_base::lex_ampersand() {
  assert(current() == '&');
  if (!peek()) {
    return token{token_type::AMPERSAND, {}, {}};
  }
  if (*peek() == '&') {
    advance();
    return token{token_type::LOG_AND, {}, {}};
  } else if (*peek() == '=') {
    advance();
    return token{token_type::AND_EQ, {}, {}};
  }
  return token{token_type::AMPERSAND, {}, {}};
}

result lexer_base::lex_vert_bar() {
  assert(current() == '|');
  if (!peek()) {
    return token{token_type::VERT_BAR, {}, {}};
  }
  if (*peek() == '|') {
    advance();
    return token{token_type::LOG_OR, {}, {}};
  } else if (*peek() == '=') {
    advance();
    return token{token_type::OR_EQ, {}, {}};
  }
  return token{token_type::VERT_BAR, {}, {}};
}

result lexer_base::lex_line_comment() {
  assert(current() == '/' && *peek() == '/');
  text << current();
  do {
    advance();
    text << current();
  } while (peek() && *peek() != '\n');
  return token{token_type::LINE_COMMENT, str_table.get_handle(text.str()), {}};
}

result lexer_base::lex_block_comment() {
  assert(current() == '/' && *peek() == '*');
  auto start = loc;
  text << "/*";
  advance(); // now pointing on *
  bool closed = false;
  while (peek()) {
    advance();
    text << current();
    if (current() == '*' && peek() && *peek() == '/') {
      advance();
      text << current();
      closed = true;
      break;
    }
  }
  if (!closed) {
    return lexer_error{"Reached end of file while lexing block comment", start};
  }
  return token{token_type::BLOCK_COMMENT, str_table.get_handle(text.str()), {}};
}

result lexer_base::lex_str() {
  assert(current() == '"' || current() == '\'');
  auto start = loc;
  auto first = current();
  bool ended = false;
  while (peek()) {
    if (current() == '\\') {
      if (auto maybe_error = consume_escape_seq()) {
        return *maybe_error;
      }
    } else if (islineterminator(current())) {
      return lexer_error{"Unexpected end of line in string literal", loc};
    } else {
      text << current();
    }
    advance();
    if (current() == first) {
      text << current();
      ended = true;
      break;
    }
  }
  if (!ended) {
    return lexer_error{"Reached end of file while lexing string literal",
                       start};
  }
  return token{
      token_type::STRING_LITERAL, str_table.get_handle(text.str()), {}};
}

result lexer_base::lex_backtick() {
  assert(current() == '`');
  text << '`';
  if (!peek()) {
    return lexer_error{"Unexpected EOF in template literal", loc};
  }
  advance();
  bool ended = false;
  while (peek()) {
    if (current() == '$' && *peek() == '{') {
      advance();
      text << "${";
      ++template_depth;
      return token{
          token_type::TEMPLATE_HEAD, str_table.get_handle(text.str()), {}};
    } else if (current() == '\\') {
      if (auto err = consume_escape_seq()) {
        return *err;
      }
    } else {
      text << current();
    }
    advance();
    if (current() == '`') {
      ended = true;
      text << '`';
      break;
    }
  }
  if (!ended) {
    return lexer_error{"Unexpected EOF in template literal", loc};
  }
  return token{
      token_type::STRING_LITERAL, str_table.get_handle(text.str()), {}};
}

result lexer_base::lex_closing_brace() {
  assert(current() == '}');
  if (template_depth == 0) {
    return token{token_type::BRACE_CLOSE, {}, {}};
  }
  if (!peek()) {
    return lexer_error{"Unexpected EOF in template literal", loc};
  }
  text << '}';
  advance();
  bool ended = false;
  do {
    if (current() == '`') {
      ended = true;
      text << '`';
      break;
    } else if (current() == '$' && *peek() == '{') {
      advance();
      text << "${";
      return token{
          token_type::TEMPLATE_MIDDLE, str_table.get_handle(text.str()), {}};
    } else if (current() == '\\') {
      if (auto err = consume_escape_seq()) {
        return *err;
      }
    } else {
      text << current();
    }
    advance();
  } while (peek());
  if (!ended) {
    return lexer_error{"Unexpected EOF in template literal", loc};
  }
  --template_depth;
  return token{token_type::TEMPLATE_END, str_table.get_handle(text.str()), {}};
}

/// Post condition: Since escape sequences can only occur in string/template
/// literals, this function will also guarantee that peek() isn't EOF after
/// an escape sequence
std::optional<lexer_error> lexer_base::consume_escape_seq() {
  assert(current() == '\\');
  if (!peek()) {
    return lexer_error{"Unexpected EOF after begin of escape sequence ('\\')",
                       loc};
  }
  advance(); // definitely consume, but line continuations may cause the
             // backslash to be dropped entirely
  // see SingleEscapeCharacter in ECMA spec
  if (current() == 'u' || current() == 'x') {
    text << '\\';
    text << current();
    auto prefix = current();
    if (!peek()) {
      return lexer_error{"Unexpected EOF after begin of escape sequence", loc};
    }
    advance();
    if (!peek()) {
      return lexer_error{"Unexpected EOF within escape sequence", loc};
    }
    if (prefix == 'x') {
      // hex escape sequence
      if (!std::isxdigit(current())) {
        return lexer_error{
            "Unexpected non-hex-digit after begin of hex escape sequence", loc};
      }
      text << current();
      advance();
      if (!std::isxdigit(current())) {
        return lexer_error{
            "Unexpected non-hex-digit as second digit in hex escape sequence",
            loc};
      }
      text << current();
    } else /* if (prefix == 'u') */ {
      // unicode escape sequence
      if (current() == '{') {
        text << current();
        bool ended = false;
        do {
          advance();
          if (!std::isxdigit(current())) {
            return lexer_error{
                "Unexpected non-hex-digit in unicode escape sequence", loc};
          }
          text << current();
          if (peek() && *peek() == '}') {
            advance();
            text << '}';
            ended = true;
            break;
          }
        } while (peek());
        if (!ended) {
          return lexer_error{"Unexpected EOF inside unicode escape sequence",
                             loc};
        }
      } else {
        for (int i = 0; i < 4; ++i) {
          if (!std::isxdigit(current())) {
            return lexer_error{
                "Unexpected non-hex-digit in unicode escape sequence", loc};
          }
          text << current();
          if (i < 3 && peek()) {
            advance();
          } else if (i < 3 && !peek()) {
            return lexer_error{"Unexpected EOF in unicode escape sequence",
                               loc};
          }
        }
      }
    }
  } else if (islineterminator(current())) {
    // line continuation
    // TODO actually we have to consume a line terminator *sequence*, not just a
    // single char

    // do nothing -- just drop the line break
  } else if (current() == '0') {
    // TODO no idea what this is supposed to do
    return lexer_error{"Not implemented (consume_escape_seq)", loc};
  } else if (one_of(current(), "'\"\\bfnrtv")) {
    // single escape characters
    // it seems like they're just regular SourceCharacters (?)
    text << '\\';
    text << current();
  } else {
    // Single SourceCodeCharacter after a backslash
    text << '\\';
    text << current();
  }
  if (!peek()) {
    return lexer_error{"Unexpected EOF after escape sequence", loc};
  }
  return std::nullopt;
}

/// This macro helps implementing binary, octal and hex literals with
/// minimal code duplication
#define LEX_SPECIAL_BASE_INT(NAME, PREFIX, TYPE, IS_DIGIT)                     \
  do { /* idiomatic do-while-false-wrapper */                                  \
    assert(current() == PREFIX[0] && *peek() == PREFIX[1]);                    \
    text << PREFIX;                                                            \
    advance(); /* now points to second char of prefix */                       \
    if (!peek() || !IS_DIGIT(*peek())) {                                       \
      return lexer_error{NAME " literal must have digits after " PREFIX, loc}; \
    }                                                                          \
    do {                                                                       \
      advance();                                                               \
      text << current();                                                       \
    } while (peek() && IS_DIGIT(*peek()));                                     \
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
#undef LEX_SPECIAL_BASE_INT

result lexer_base::lex_number() {
  assert(std::isdigit(current()) || current() == '.');
  token_type ty = token_type::INT_LITERAL;
  if (current() != '.') { // we got a digit
    if (!peek()) {
      text << current();
      return token{
          token_type::INT_LITERAL, str_table.get_handle(text.str()), {}};
    }
    if (current() == '0') {
      if (*peek() == '.') {
        // fall through
      } else if (*peek() == 'x' || *peek() == 'X') {
        return lex_hex_int();
      } else if (*peek() == 'b' || *peek() == 'B') {
        return lex_bin_int();
      } else if (*peek() == 'o' || *peek() == 'O') {
        return lex_oct_int();
      } else if (std::isdigit(*peek())) {
        return lexer_error{"Number literals mustn't have more than the first "
                           "digit when starting with '0'",
                           loc};
      }
    }
    text << current();
    // consume remaining leading digits
    while (peek() && std::isdigit(*peek())) {
      advance();
      text << current();
    }
    // a dot will also be part of the number
    if (peek() && *peek() == '.') {
      advance();
    }
  } else if (!peek() || !std::isdigit(*peek())) {
    return lexer_error{"Expected number, but no digits after leading dot ('.')",
                       loc};
  }
  // now we're looking at a leading dot (if it exists)
  if (current() == '.') {
    ty = token_type::FLOAT_LITERAL;
    text << current();
    // consume decimal places
    while (peek() && std::isdigit(*peek())) {
      advance();
      text << current();
    }
  }
  if (!peek()) {
    return token{ty, str_table.get_handle(text.str()), {}};
  }
  if (*peek() == 'e' || *peek() == 'E') {
    advance();
    text << current();
    if (!peek() ||
        (!std::isdigit(*peek()) && *peek() != '+' && *peek() != '-')) {
      return lexer_error{"Missing digits after exponent part of number literal",
                         loc};
    }
    if (*peek() == '-') {
      ty = token_type::FLOAT_LITERAL;
    }
    if (*peek() == '-' || *peek() == '+') {
      advance(); // consume sign
      text << current();
    }
    if (!peek() || !std::isdigit(*peek())) {
      return lexer_error{
          "Missing digits after exponent part's sign of number literal", loc};
    }
    // consume exponent
    while (peek() && std::isdigit(*peek())) {
      advance();
      text << current();
    }
  }
  return token{ty, str_table.get_handle(text.str()), {}};
}

result lexer_base::lex_id_keyword() {
  assert(std::isalpha(current()) || current() == '_' || current() == '$');
  text << current();
  while (peek() && (std::isalnum(*peek()) || *peek() == '_' || *peek() == '$' ||
                    *peek() == '\\')) {
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

keyword_type lexer_base::get_keyword_type(const token &t) {
  assert(t.type == token_type::KEYWORD);
#define KEYWORD(NAME)                                                          \
  if (t.text.data() == kw_##NAME##_str) {                                      \
    return keyword_type::kw_##NAME;                                            \
  }
#include "parsing/keywords.def"
  unreachable("Unknown keyword type");
}

token lexer_base::make_token(token_type ty, const char *text) {
  return token{ty, str_table.get_handle(text), {}};
}

} // namespace parsing
