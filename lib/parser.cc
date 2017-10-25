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

namespace parsing {
std::ostream &operator<<(std::ostream &stream, const parser_error &err) {
  return stream << err.msg;
}
} // namespace parsing

bool parser_base::advance() {
  if (!rewind_stack.empty()) {
    current_token = rewind_stack.top();
    rewind_stack.pop();
    //~ std::cout << "Again: " << current_token
    //~ << "\n"; // TODO Remove this after initial impl
    return true;
  }
  do {
    lexer_base::result T = next_token();
    if (std::holds_alternative<lexer_base::eof_t>(T)) {
      return false;
    } else if (std::holds_alternative<lexer_error>(T)) {
      auto err = std::get<lexer_error>(T);
      error = parser_error{"Lexer Error: " + err.msg, err.loc};
      return false;
    }
    current_token = std::get<token>(T);
    //~ std::cout << current_token << "\n"; // TODO Remove this after initial
    //impl
  } while (current_token.type == token_type::LINE_COMMENT ||
           current_token.type == token_type::BLOCK_COMMENT);
  return true;
}

void parser_base::rewind(token t) {
  rewind_stack.emplace(current_token);
  current_token = t;
}

void parser_base::reset() {
  nodes.clear();
  module = nodes.make_module();
  rewind_stack = {};
}

parser_base::result parser_base::parse() {
  reset();
  while (!error && advance()) {
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

static number_literal_node *make_number_expression(token t,
                                                   ast_node_store &nodes) {
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
    return parse_block_or_obj();
  } else {
    return parse_expression();
  }
}

expression_node *parser_base::parse_expression() {
  if (current_token.type == token_type::KEYWORD) {
    return parse_keyword_expr();
  } else if (current_token.type == token_type::IDENTIFIER) {
    auto ident = current_token;
    auto res = nodes.make_identifier_expr();
    res->str = current_token.text;
    auto read_success = advance();
    if (!read_success || current_token.type == token_type::SEMICOLON ||
        current_token.type == token_type::PAREN_CLOSE) {
      if (current_token.type == token_type::PAREN_CLOSE) {
        rewind(ident);
      }
      return res;
    } else if (current_token.type == token_type::PAREN_OPEN) {
      return parse_call(res);
    } else if (current_token.type == token_type::BRACKET_OPEN) {
      return parse_computed_access(res);
    } else if (current_token.type == token_type::DOT) {
      return parse_member_access(res);
    }
    return parse_bin_op(res);
  } else if (current_token.is_number_literal()) {
    auto literal = current_token;
    auto res = make_number_expression(current_token, nodes);
    res->val = current_token.text;
    auto read_success = advance();
    if (!read_success || current_token.type == token_type::SEMICOLON ||
        current_token.type == token_type::PAREN_CLOSE) {
      if (current_token.type == token_type::PAREN_CLOSE) {
        rewind(literal);
      }
      return res;
    }
    return parse_bin_op(res);
  } else if (current_token.type == token_type::STRING_LITERAL) {
    auto str = current_token;
    auto res = nodes.make_string_literal();
    res->val = current_token.text;
    auto read_success = advance();
    if (!read_success || current_token.type == token_type::SEMICOLON ||
        current_token.type == token_type::PAREN_CLOSE) {
      if (current_token.type == token_type::PAREN_CLOSE) {
        rewind(str);
      }
      return res;
    }
    return parse_bin_op(res);
  } else if (current_token.type == token_type::BRACKET_OPEN) {
    return parse_array_literal();
  } else if (current_token.type == token_type::BRACE_OPEN) {
    return parse_object_literal();
  } else if (current_token.type == token_type::PAREN_OPEN) {
    ADVANCE_OR_ERROR("Unexpected EOF after opening parenthesis", nullptr);
    auto expr = parse_expression();
    ADVANCE_OR_ERROR("Unexpected EOF. Expected closing brace", nullptr);
    EXPECT(PAREN_CLOSE, nullptr);
    return expr;
  }
  error = {"Not implemented (parse expression)", current_token.loc};
  return nullptr;
}

bin_op_expr_node *parser_base::parse_bin_op(expression_node *lhs) {
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
    assert(current_token.type != token_type::BRACE_OPEN);
    std::cout << "parse_block -> parse_statement\n";
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
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration",
                   nullptr);
  EXPECT(IDENTIFIER, nullptr);
  decl->name = current_token.text;
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration",
                   nullptr);
  EXPECT_SEVERAL(TYPELIST(token_type::SEMICOLON, token_type::EQ), nullptr);
  if (current_token.type == token_type::SEMICOLON) {
    return decl;
  } else if (current_token.type == token_type::EQ) {
    ADVANCE_OR_ERROR(
        "Unecpected EOF in variable initialization. Expected expression",
        nullptr);
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

array_literal_node *parser_base::parse_array_literal() {
  error = {"Not implemented (array_literal)", current_token.loc};
  return nullptr;
}
object_literal_node *parser_base::parse_object_literal() {
  error = {"Not implemented (object_literal)", current_token.loc};
  return nullptr;
}
computed_member_access_node *
parser_base::parse_computed_access(expression_node *base) {
  error = {"Not implemented (computed_access)", current_token.loc};
  return nullptr;
}
member_access_node *parser_base::parse_member_access(expression_node *base) {
  error = {"Not implemented (member_access)", current_token.loc};
  return nullptr;
}
call_expr_node *parser_base::parse_call(expression_node *callee) {
  error = {"Not implemented (call)", current_token.loc};
  return nullptr;
}
statement_node *parser_base::parse_block_or_obj() {
  EXPECT(BRACE_OPEN, nullptr);
  auto brace = current_token;
  ADVANCE_OR_ERROR("Unexpected EOF", nullptr);
  if (current_token.type == token_type::STRING_LITERAL ||
      current_token.type == token_type::IDENTIFIER) {
    auto key = current_token;
    ADVANCE_OR_ERROR("Unexpected EOF", nullptr);
    auto maybe_colon = current_token;
    rewind(key);
    rewind(brace);
    if (maybe_colon.type == token_type::COLON) {
      return parse_object_literal();
    }
  } else if (current_token.type == token_type::BRACE_CLOSE) {
    return nodes.make_object_literal();
  } else {
    rewind(brace);
  }
  return parse_block();
}
