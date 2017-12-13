#ifndef JNSN_UNITTEST_PARSE_UTILS_H
#define JNSN_UNITTEST_PARSE_UTILS_H

#include "jnsn/parser.h"
#include "lex_utils.h"
namespace jnsn {
class constant_string_parser : public parser_base {
public:
  constant_string_lexer lexer;
  lexer_base &get_lexer() { return lexer; }
};
} // namespace jnsn
#endif // JNSN_UNITTEST_PARSE_UTILS_H
