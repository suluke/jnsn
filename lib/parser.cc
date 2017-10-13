#include "parsing/parser.h"

using namespace parsing;

parser_base::result parser_base::parse() {
  nodes.clear();
  module = nodes.make_module();

  lexer_base::result T;
  while (std::holds_alternative<token>(T = next_token())) {
    auto tok = std::get<token>(T);
    std::cout << tok << "\n";
    parse_expression(tok);
  }

  return &*module;
}

void parser_base::parse_expression(token first) {
  if (first.type == token_type::SEMICOLON) {
    auto empty = nodes.make_empty_expr();
    module->exprs.emplace_back(empty);
    return;
  }
}
