#include "parsing/parser.h"
#include <initializer_list>

using namespace parsing;

lexer_base::result parser_base::next_token() { return get_lexer().next(); }

#define ADVANCE_OR_ERROR(MESSAGE, RETURN_VAL)                                  \
  do {                                                                         \
    auto read_success = advance();                                             \
    if (!read_success) {                                                       \
      error = {MESSAGE, {}};                                                   \
    }                                                                          \
    if (error) {                                                               \
      return RETURN_VAL;                                                       \
    }                                                                          \
  } while (false)

#define TYPELIST(...)                                                          \
  { __VA_ARGS__ }

#define EXPECT_SEVERAL(TYPES, RETURN_VAL)                                      \
  do {                                                                         \
    bool found_expected = false;                                               \
    std::initializer_list<token_type> types TYPES;                             \
    for (auto T : types) {                                                     \
      if (current_token.type == T) {                                           \
        found_expected = true;                                                 \
        break;                                                                 \
      }                                                                        \
    }                                                                          \
    if (!found_expected) {                                                     \
      error = {"Unexpected token. Expected: " #TYPES, current_token.loc};      \
      return RETURN_VAL;                                                       \
    }                                                                          \
  } while (false)

#define EXPECT(TYPE, RETURN_VAL)                                               \
  EXPECT_SEVERAL(TYPELIST(token_type::TYPE), RETURN_VAL)

bool parser_base::advance() {
  lexer_base::result T = next_token();
  if (std::holds_alternative<lexer_base::eof_t>(T)) {
    return false;
  } else if (std::holds_alternative<lexer_error>(T)) {
    auto err = std::get<lexer_error>(T);
    error = parser_error{"Lexer Error: " + err.msg, err.loc};
    return false;
  }
  current_token = std::get<token>(T);
  std::cout << current_token << "\n"; // TODO Remove this after initial impl
  return true;
}

parser_base::result parser_base::parse() {
  nodes.clear();
  module = nodes.make_module();

  while (!error && advance() && !error) {
    auto expr = parse_expression();
    if (!expr || error) {
      break;
    }
    module->exprs.emplace_back(expr);
  }

  if (error) {
    return *error;
  }
  return module;
}

typed_ast_node_ref<expression_node> parser_base::parse_expression() {
  if (current_token.type == token_type::SEMICOLON) {
    return nodes.make_empty_expr();
  } else if (current_token.type == token_type::KEYWORD) {
    return parse_keyword_expr();
  }
  error = {"Not implemented", current_token.loc};
  return {};
}

typed_ast_node_ref<expression_node> parser_base::parse_keyword_expr() {
  EXPECT(KEYWORD, {});
  auto kw_ty = get_lexer().get_keyword_type(current_token);
  if (kw_ty == keyword_type::kw_function) {
    return parse_function();
  } else if (kw_ty == keyword_type::kw_var || kw_ty == keyword_type::kw_const ||
             kw_ty == keyword_type::kw_let) {
    return parse_var_decl();
  } else {
    error = {"Not implemented (keyword)", current_token.loc};
    return {};
  }
}

typed_ast_node_ref<function_node> parser_base::parse_function() {
  EXPECT(KEYWORD, {}); // "function"
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", {});
  auto func = nodes.make_function();
  EXPECT(IDENTIFIER, {});
  func->name = current_token.text;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", {});
  auto params = parse_param_list();
  if (!params) {
    return {};
  }
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", {});
  auto body = parse_block();
  if (!body) {
    return {};
  }
  func->body = body;
  return func;
}

typed_ast_node_ref<param_list_node> parser_base::parse_param_list() {
  EXPECT(PAREN_OPEN, {});
  auto node = nodes.make_param_list();
  do {
    ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", {});
    if (current_token.type == token_type::IDENTIFIER) {
      node->names.emplace_back(current_token.text);
      ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", {});
    }
  } while (current_token.type == token_type::COMMA);
  if (current_token.type == token_type::DOTDOTDOT) {
    ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", {});
    if (current_token.type == token_type::IDENTIFIER) {
      node->rest = current_token.text;
      ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", {});
    }
  }
  if (current_token.type == token_type::PAREN_CLOSE) {
    return node;
  }

  error = {"Unexpected token in parameter list", current_token.loc};
  return {};
}

typed_ast_node_ref<block_node> parser_base::parse_block() {
  EXPECT(BRACE_OPEN, {});
  ADVANCE_OR_ERROR("Unexpected EOF while parsing block", {});
  auto block = nodes.make_block();
  while (current_token.type != token_type::BRACE_CLOSE) {
    // TODO
    ADVANCE_OR_ERROR("Unexpected EOF while parsing block", {});
  }
  return block;
}

typed_ast_node_ref<var_decl_node> parser_base::parse_var_decl() {
  EXPECT(KEYWORD, {});
  auto decl = nodes.make_var_decl();
  decl->keyword = current_token.text;
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration", {});
  EXPECT(IDENTIFIER, {});
  decl->name = current_token.text;
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration", {});
  EXPECT_SEVERAL(TYPELIST(token_type::SEMICOLON, token_type::EQ), {});
  if (current_token.type == token_type::SEMICOLON) {
    return decl;
  } else if (current_token.type == token_type::EQ) {
    ADVANCE_OR_ERROR("TODO", {});
    auto init = parse_expression();
    if (!init) {
      assert(error);
      return {};
    }
    decl->init = init;
    return decl;
  }
  error = {"Unecpected token", current_token.loc};
  return {};
}