#ifndef PARSING_UNITTEST_PARSE_UTILS_H
#define PARSING_UNITTEST_PARSE_UTILS_H

#include "lex_utils.h"
#include "parsing/parser.h"
namespace parsing {
class constant_string_parser : public parser_base {
public:
  constant_string_lexer lexer;
  lexer_base &get_lexer() { return lexer; }
};
} // namespace parsing
#endif // PARSING_UNITTEST_PARSE_UTILS_H