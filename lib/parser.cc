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
    auto stmt = parse_statement();
    if (!stmt || error) {
      break;
    }
    module->stmts.emplace_back(stmt);
  }

  if (error) {
    return *error;
  }
  return module;
}

static number_literal_node
*make_number_expression(token t, ast_node_store &nodes) {
  number_literal_node *res = nullptr;
  if (t.type == token_type::INT_LITERAL) {
    res = nodes.make_int_literal();
  } else if (t.type == token_type::FLOAT_LITERAL) {
    res = nodes.make_float_literal();
  } else if (t.type == token_type::HEX_LITERAL) {
    res = nodes.make_float_literal();
  } else if (t.type == token_type::OCT_LITERAL) {
    res = nodes.make_float_literal();
  } else if (t.type == token_type::BIN_LITERAL) {
    res = nodes.make_float_literal();
  }
  assert(res && "Token not a (known) number literal");
  res->val = t.text;
  return res;
}

statement_node *parser_base::parse_statement() {
  if (current_token.type == token_type::SEMICOLON) {
    return nodes.make_empty_stmt();
  } else if (current_token.type == token_type::BRACE_OPEN) {
    return parse_block();
  } else {
    return parse_expression();
  }
}

expression_node *parser_base::parse_expression() {
  if (current_token.type == token_type::KEYWORD) {
    return parse_keyword_expr();
  } else if (current_token.type == token_type::IDENTIFIER) {
    auto res = nodes.make_identifier_expr();
    res->str = current_token.text;
    auto read_success = advance();
    if (!read_success || current_token.type == token_type::SEMICOLON) {
      return res;
    }
    return parse_bin_op(res);
  } else if (current_token.is_number_literal()) {
    auto res = make_number_expression(current_token, nodes);
    res->val = current_token.text;
    auto read_success = advance();
    if (!read_success || current_token.type == token_type::SEMICOLON) {
      return res;
    }
    return parse_bin_op(res);
  } else if (current_token.type == token_type::STRING_LITERAL) {
    auto res = nodes.make_string_literal();
    res->val = current_token.text;
    auto read_success = advance();
    if (!read_success || current_token.type == token_type::SEMICOLON) {
      return res;
    }
    return parse_bin_op(res);
  }
  error = {"Not implemented (parse expression)", current_token.loc};
  return nullptr;
}

bin_op_expr_node
*parser_base::parse_bin_op(expression_node *lhs) {
  error = {"Not implemented (binary expression)", current_token.loc};
  return nullptr;
}

expression_node *parser_base::parse_keyword_expr() {
  EXPECT(KEYWORD, nullptr);
  auto kw_ty = get_lexer().get_keyword_type(current_token);
  if (kw_ty == keyword_type::kw_function) {
    return parse_function();
  } else if (kw_ty == keyword_type::kw_var || kw_ty == keyword_type::kw_const ||
             kw_ty == keyword_type::kw_let) {
    return parse_var_decl();
  } else {
    error = {"Not implemented (keyword)", current_token.loc};
    return nullptr;
  }
}

function_node *parser_base::parse_function() {
  EXPECT(KEYWORD, nullptr); // "function"
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  auto func = nodes.make_function();
  EXPECT(IDENTIFIER, nullptr);
  func->name = current_token.text;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  auto params = parse_param_list();
  if (!params) {
    return nullptr;
  }
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  auto body = parse_block();
  if (!body) {
    return nullptr;
  }
  func->body = body;
  return func;
}

param_list_node *parser_base::parse_param_list() {
  EXPECT(PAREN_OPEN, nullptr);
  auto node = nodes.make_param_list();
  do {
    ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", nullptr);
    if (current_token.type == token_type::IDENTIFIER) {
      node->names.emplace_back(current_token.text);
      ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", nullptr);
    }
  } while (current_token.type == token_type::COMMA);
  if (current_token.type == token_type::DOTDOTDOT) {
    ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", nullptr);
    if (current_token.type == token_type::IDENTIFIER) {
      node->rest = current_token.text;
      ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list", nullptr);
    }
  }
  if (current_token.type == token_type::PAREN_CLOSE) {
    return node;
  }

  error = {"Unexpected token in parameter list", current_token.loc};
  return nullptr;
}

block_node *parser_base::parse_block() {
  EXPECT(BRACE_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing block", nullptr);
  auto block = nodes.make_block();
  while (current_token.type != token_type::BRACE_CLOSE) {
    auto stmt = parse_statement();
    block->stmts.emplace_back(stmt);
    ADVANCE_OR_ERROR("Unexpected EOF while parsing block", nullptr);
  }
  return block;
}

var_decl_node *parser_base::parse_var_decl() {
  EXPECT(KEYWORD, nullptr);
  auto decl = nodes.make_var_decl();
  decl->keyword = current_token.text;
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration", nullptr);
  EXPECT(IDENTIFIER, nullptr);
  decl->name = current_token.text;
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration", nullptr);
  EXPECT_SEVERAL(TYPELIST(token_type::SEMICOLON, token_type::EQ), nullptr);
  if (current_token.type == token_type::SEMICOLON) {
    return decl;
  } else if (current_token.type == token_type::EQ) {
    ADVANCE_OR_ERROR("TODO", nullptr);
    auto init = parse_expression();
    if (!init) {
      assert(error);
      return nullptr;
    }
    decl->init = init;
    return decl;
  }
  error = {"Unecpected token", current_token.loc};
  return nullptr;
}
