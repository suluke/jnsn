#include "parsing/parser.h"
#include <initializer_list>

using namespace parsing;

lexer_base::result parser_base::next_token() { return get_lexer().next(); }
void parser_base::set_error(std::string msg, source_location loc) {
  error = {msg, loc};
}

static std::string to_string(token_type t) {
  switch (t) {
#define TOKEN_TYPE(NAME, STR)                                                  \
  case token_type::NAME:                                                       \
    return #NAME;
#include "parsing/tokens.def"
  }
  assert(false); // FIXME come up with some "unreachable" macro
  return "";
}

#define ADVANCE_OR_ERROR(MESSAGE, RETURN_VAL)                                  \
  do {                                                                         \
    /* Make sure we haven't already encountered an error */                    \
    if (error) {                                                               \
      return RETURN_VAL;                                                       \
    }                                                                          \
    auto read_success = advance();                                             \
    if (!read_success) {                                                       \
      set_error(MESSAGE, {});                                                  \
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
      set_error("Unexpected token. Expected: " #TYPES ". Was: " +              \
                    to_string(current_token.type),                             \
                current_token.loc);                                            \
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
    return true;
  }
  do {
    lexer_base::result T = next_token();
    if (std::holds_alternative<lexer_base::eof_t>(T)) {
      return false;
    } else if (std::holds_alternative<lexer_error>(T)) {
      auto err = std::get<lexer_error>(T);
      set_error("Lexer Error: " + err.msg, err.loc);
      return false;
    }
    current_token = std::get<token>(T);
  } while (current_token.type == token_type::LINE_COMMENT ||
           current_token.type == token_type::BLOCK_COMMENT);
  return true;
}

void parser_base::rewind(token t) {
  assert(t.type != token_type::BLOCK_COMMENT &&
         t.type != token_type::LINE_COMMENT);
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

static bool is_stmt_end(token t) {
  switch (t.type) {
  case token_type::SEMICOLON:
    return true;
  default:
    return false;
  }
  return false;
}

statement_node *parser_base::parse_statement() {
  statement_node *stmt = nullptr;
  if (current_token.type == token_type::SEMICOLON) {
    return nodes.make_empty_stmt();
  } else if (current_token.type == token_type::BRACE_OPEN) {
    return parse_block_or_obj();
  } else if (current_token.type == token_type::KEYWORD) {
    stmt = parse_keyword_stmt();
  } else {
    // parse expression statement
    stmt = parse_expression(true);
  }
  if (error || !stmt) {
    assert(error && !stmt);
    return nullptr;
  }
  auto read_success = advance();
  if (error) {
    return nullptr;
  }
  if (read_success && !is_stmt_end(current_token)) {
    set_error("Unexpected token after statement: " +
                  to_string(current_token.type),
              current_token.loc);
  }
  return stmt;
}

statement_node *parser_base::parse_keyword_stmt() {
  assert(current_token.type == token_type::KEYWORD);
  auto kwty = lexer_base::get_keyword_type(current_token);
  if (kwty == keyword_type::kw_function) {
    return parse_function_stmt();
  } else if (kwty == keyword_type::kw_if) {
    return parse_if_stmt();
  } else if (kwty == keyword_type::kw_do) {
    return parse_do_while();
  } else if (kwty == keyword_type::kw_while) {
    return parse_while_stmt();
  } else if (kwty == keyword_type::kw_for) {
    return parse_for_stmt();
  } else if (kwty == keyword_type::kw_switch) {
    return parse_switch_stmt();
  } else if (kwty == keyword_type::kw_break) {
    return nodes.make_break_stmt(); // FIXME break LABEL
  } else if (kwty == keyword_type::kw_continue) {
    return nodes.make_continue_stmt(); // FIXME continue LABEL
  } else if (kwty == keyword_type::kw_return) {
    return parse_return_stmt();
  } else if (kwty == keyword_type::kw_throw) {
    return parse_throw_stmt();
  } else if (kwty == keyword_type::kw_try) {
    return parse_try_stmt();
  } else if (kwty == keyword_type::kw_super) {
    auto *id = nodes.make_identifier_expr();
    id->str = current_token.text;
    return parse_call(id);
  } else {
    return parse_keyword_expr();
  }
}

if_stmt_node *parser_base::parse_if_stmt() {
  set_error("Not implemented (parse_if)", current_token.loc);
  return nullptr;
}
do_while_node *parser_base::parse_do_while() {
  set_error("Not implemented (parse_do_while)", current_token.loc);
  return nullptr;
}
while_stmt_node *parser_base::parse_while_stmt() {
  set_error("Not implemented (parse_while)", current_token.loc);
  return nullptr;
}
for_stmt_node *parser_base::parse_for_stmt() {
  set_error("Not implemented (parse_for)", current_token.loc);
  return nullptr;
}
switch_stmt_node *parser_base::parse_switch_stmt() {
  set_error("Not implemented (parse_switch)", current_token.loc);
  return nullptr;
}
return_stmt_node *parser_base::parse_return_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_return);
  auto *ret = nodes.make_return_stmt();
  if (advance() && !is_stmt_end(current_token)) {
    auto *expr = parse_expression(true);
    ret->value = expr;
  }
  return ret;
}
throw_stmt_node *parser_base::parse_throw_stmt() {
  set_error("Not implemented (parse_throw)", current_token.loc);
  return nullptr;
}
try_stmt_node *parser_base::parse_try_stmt() {
  set_error("Not implemented (parse_try)", current_token.loc);
  return nullptr;
}

static bool is_follow_expression(token t) {
  switch (t.type) {
  case token_type::SEMICOLON:
  case token_type::DOT:
  case token_type::COMMA:
  case token_type::PAREN_CLOSE:
  case token_type::BRACKET_CLOSE:
  case token_type::BRACE_CLOSE:
  case token_type::PLUS:
  case token_type::MINUS:
  case token_type::ASTERISK:
  case token_type::POW:
  case token_type::SLASH:
  case token_type::PERCENT:
  case token_type::EQ:
  case token_type::EQEQ:
  case token_type::EQEQEQ:
  case token_type::NEQ:
  case token_type::NEQEQ:
  case token_type::GT:
  case token_type::LT:
  case token_type::GT_EQ:
  case token_type::LT_EQ:
  case token_type::RSHIFT:
  case token_type::LOG_RSHIFT:
  case token_type::AMPERSAND:
  case token_type::VERT_BAR:
  case token_type::CARET:
  case token_type::QMARK:
  case token_type::COLON:
  case token_type::LOG_AND:
  case token_type::LOG_OR:
  case token_type::PLUS_EQ:
  case token_type::MINUS_EQ:
  case token_type::MOD_EQ:
  case token_type::MUL_EQ:
  case token_type::DIV_EQ:
  case token_type::POW_EQ:
  case token_type::LSH_EQ:
  case token_type::RSH_EQ:
  case token_type::LOG_RSH_EQ:
  case token_type::AND_EQ:
  case token_type::OR_EQ:
  case token_type::CARET_EQ:
  case token_type::TEMPLATE_MIDDLE:
  case token_type::TEMPLATE_END:
    return true;
  case token_type::KEYWORD: {
    switch (lexer_base::get_keyword_type(t)) {
    case keyword_type::kw_typeof:
    case keyword_type::kw_instanceof:
    case keyword_type::kw_in:
      return true;
    default:
      return false;
    }
  }
  default:
    return false;
  }
}
static bool is_expression_end(token t, bool comma_is_operator) {
  switch (t.type) {
  case token_type::SEMICOLON:
  case token_type::PAREN_CLOSE:
  case token_type::BRACE_CLOSE:
  case token_type::BRACKET_CLOSE:
  case token_type::TEMPLATE_MIDDLE:
  case token_type::TEMPLATE_END:
    return true;
  case token_type::COMMA:
    return !comma_is_operator;
  default:
    return false;
  }
}

static int get_precedence(token op) {
  switch (op.type) {
#define INFIX_OP(TYPE, PRECEDENCE)                                             \
  case token_type::TYPE:                                                       \
    return PRECEDENCE;
#include "parsing/operators.def"
  default:
    return -1; // FIXME more explicit error handling
  }
}

static bool is_binary_operator(token op, bool comma_is_operator = true) {
  if (op.type == token_type::COMMA) {
    return comma_is_operator;
  }
#define INFIX_OP(TYPE, X)                                                      \
  if (op.type == token_type::TYPE) {                                           \
    return true;                                                               \
  }
#include "parsing/operators.def"
  return false;
}

expression_node *parser_base::parse_expression(bool comma_is_operator) {
  expression_node *expr = parse_atomic_expr();
  if (expr) {
    auto final_token = current_token;
    if (!advance()) {
      return expr;
    }
    if (is_expression_end(current_token, comma_is_operator)) {
      rewind(final_token);
      return expr;
    }
    if (is_binary_operator(current_token, comma_is_operator)) {
      token prev_token;
      do {
        expr = parse_bin_op(expr, comma_is_operator);
        prev_token = current_token;
        if (!advance()) {
          // note that we skip rewinding here
          return expr;
        }
      } while (is_binary_operator(current_token, comma_is_operator));
      rewind(prev_token);
      return expr;
    }
  }
  set_error("Not implemented (parse expression)", current_token.loc);
  return nullptr;
}

expression_node *parser_base::parse_atomic_expr() {
  expression_node *expr = nullptr;
  if (current_token.type == token_type::KEYWORD) {
    expr = parse_keyword_expr();
  } else if (current_token.type == token_type::IDENTIFIER) {
    auto *id = nodes.make_identifier_expr();
    id->str = current_token.text;
    expr = id;
  } else if (current_token.is_number_literal()) {
    expr = parse_number_literal();
  } else if (current_token.type == token_type::STRING_LITERAL) {
    expr = parse_string_literal();
  } else if (current_token.type == token_type::BRACKET_OPEN) {
    expr = parse_array_literal();
  } else if (current_token.type == token_type::BRACE_OPEN) {
    expr = parse_object_literal();
  } else if (current_token.type == token_type::PAREN_OPEN) {
    ADVANCE_OR_ERROR("Unexpected EOF after opening parenthesis", nullptr);
    expr = parse_expression(true);
    if (error || !expr) {
      assert(error && !expr);
      return nullptr;
    }
    ADVANCE_OR_ERROR("Unexpected EOF. Expected closing brace", nullptr);
    EXPECT(PAREN_CLOSE, nullptr);
  }
  if (expr) {
    do {
      auto prev_token = current_token;
      if (advance()) {
        if (current_token.type == token_type::DOT) {
          // member access
          expr = parse_member_access(expr);
        } else if (current_token.type == token_type::PAREN_OPEN) {
          // call
          expr = parse_call(expr);
        } else if (current_token.type == token_type::BRACKET_OPEN) {
          // computed member access
          expr = parse_computed_access(expr);
        } else {
          rewind(prev_token);
          break;
        }
      } else {
        break;
      }
    } while (true);
    return expr;
  }
  set_error("Not implemented (atomic expression)", current_token.loc);
  return nullptr;
}

static bin_op_expr_node *make_binary_expr(token op, expression_node *lhs,
                                          expression_node *rhs,
                                          ast_node_store &nodes) {
  bin_op_expr_node *res = nullptr;
  switch (op.type) {
  case token_type::PLUS:
    res = nodes.make_add();
    break;
  case token_type::MINUS:
    res = nodes.make_subtract();
    break;
  case token_type::ASTERISK:
    res = nodes.make_multiply();
    break;
  case token_type::SLASH:
    res = nodes.make_divide();
    break;
  case token_type::COMMA:
    res = nodes.make_comma_operator();
    break;
  default:
    res = nullptr;
    break;
  }
  assert(res);
  res->lhs = lhs;
  res->rhs = rhs;
  return res;
}

bin_op_expr_node *parser_base::parse_bin_op(expression_node *lhs,
                                            bool comma_is_operator) {
  auto op = current_token;
  assert(is_binary_operator(op, comma_is_operator));
  ADVANCE_OR_ERROR(
      "Unexpected EOF. Expected right hand side argument of binary operation",
      nullptr);
  expression_node *rhs = parse_atomic_expr();

  auto prev_token = current_token;
  if (!advance()) {
    return make_binary_expr(op, lhs, rhs, nodes);
  }
  if (is_expression_end(current_token, comma_is_operator)) {
    rewind(prev_token);
    return make_binary_expr(op, lhs, rhs, nodes);
  }
  if (is_binary_operator(current_token, comma_is_operator)) {
    if (get_precedence(current_token) > get_precedence(op)) {
      rhs = parse_bin_op(rhs, comma_is_operator);
    }
    // FIXME associativity...
    return make_binary_expr(op, lhs, rhs, nodes);
  }

  set_error("Not implemented (binop expression)", current_token.loc);
  return nullptr;
}

expression_node *parser_base::parse_keyword_expr() {
  EXPECT(KEYWORD, nullptr);
  auto kw_ty = get_lexer().get_keyword_type(current_token);
  if (kw_ty == keyword_type::kw_function) {
    return parse_function_expr();
  } else if (kw_ty == keyword_type::kw_var || kw_ty == keyword_type::kw_const ||
             kw_ty == keyword_type::kw_let) {
    return parse_var_decl();
  } else {
    set_error("Not implemented (keyword)", current_token.loc);
    return nullptr;
  }
}

number_literal_node *parser_base::parse_number_literal() {
  auto literal = current_token;
  auto res = make_number_expression(current_token, nodes);
  res->val = current_token.text;
  auto read_success = advance();
  if (!read_success || is_follow_expression(current_token)) {
    if (read_success) {
      rewind(literal);
    }
    return res;
  }
  set_error("Unexpected token after number literal", current_token.loc);
  return nullptr;
}

string_literal_node *parser_base::parse_string_literal() {
  auto str = current_token;
  auto res = nodes.make_string_literal();
  res->val = current_token.text;
  auto read_success = advance();
  if (!read_success || is_follow_expression(current_token)) {
    if (read_success) {
      rewind(str);
    }
    return res;
  }
  set_error("Unexpected token after string literal", current_token.loc);
  return nullptr;
}

function_stmt_node *parser_base::parse_function_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_function);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  auto func = nodes.make_function_stmt();
  EXPECT(IDENTIFIER, nullptr);
  func->name = current_token.text;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  EXPECT(PAREN_OPEN, nullptr);
  auto params = parse_param_list();
  if (error || !params) {
    assert(error && !params);
    return nullptr;
  }
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  EXPECT(BRACE_OPEN, nullptr);
  auto body = parse_block();
  if (error || !body) {
    assert(error && !body);
    return nullptr;
  }
  func->body = body;
  return func;
}

function_expr_node *parser_base::parse_function_expr() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_function);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  auto func = nodes.make_function_expr();
  if (current_token.type == token_type::IDENTIFIER) {
    func->name = current_token.text;
    ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  }
  EXPECT(PAREN_OPEN, nullptr);
  auto params = parse_param_list();
  if (error || !params) {
    assert(error && !params);
    return nullptr;
  }
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  EXPECT(BRACE_OPEN, nullptr);
  auto body = parse_block();
  if (error || !body) {
    assert(error && !body);
    return nullptr;
  }
  func->body = body;
  return func;
}

param_list_node *parser_base::parse_param_list() {
  assert(current_token.type == token_type::PAREN_OPEN);
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

  set_error("Unexpected token in parameter list", current_token.loc);
  return nullptr;
}

block_node *parser_base::parse_block() {
  EXPECT(BRACE_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing block", nullptr);
  auto block = nodes.make_block();
  while (current_token.type != token_type::BRACE_CLOSE) {
    assert(current_token.type != token_type::BRACE_OPEN);
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
    auto init = parse_expression(true);
    if (!init) {
      assert(error);
      return nullptr;
    }
    decl->init = init;
    return decl;
  }
  set_error("Unecpected token", current_token.loc);
  return nullptr;
}

array_literal_node *parser_base::parse_array_literal() {
  set_error("Not implemented (array_literal)", current_token.loc);
  return nullptr;
}
object_literal_node *parser_base::parse_object_literal() {
  set_error("Not implemented (object_literal)", current_token.loc);
  return nullptr;
}
computed_member_access_node *
parser_base::parse_computed_access(expression_node *base) {
  assert(current_token.type == token_type::BRACKET_OPEN);
  ADVANCE_OR_ERROR("Unexpected EOF inside computed member access", nullptr);
  auto *member = parse_expression(true);
  if (error || !member) {
    assert(error && !member);
    return nullptr;
  }
  ADVANCE_OR_ERROR("Unexpected EOF inside computed member access", nullptr);
  EXPECT(BRACKET_CLOSE, nullptr);
  auto *access = nodes.make_computed_member_access();
  access->base = base;
  access->member = member;
  return access;
}
member_access_node *parser_base::parse_member_access(expression_node *base) {
  assert(current_token.type == token_type::DOT);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing member access", nullptr);
  EXPECT(IDENTIFIER, nullptr);
  auto *node = nodes.make_member_access();
  node->base = base;
  node->member = current_token.text;
  return node;
}
call_expr_node *parser_base::parse_call(expression_node *callee) {
  assert(current_token.type == token_type::PAREN_OPEN);
  auto *call = nodes.make_call_expr();
  auto *args = nodes.make_argument_list();
  call->callee = callee;
  call->args = args;

  ADVANCE_OR_ERROR("Unexpected EOF after begin of argument list", nullptr);
  if (current_token.type == token_type::PAREN_CLOSE) {
    return call;
  }
  do {
    auto *arg = parse_expression(false);
    if (error || !arg) {
      assert(error && !arg);
      return nullptr;
    }
    args->values.emplace_back(arg);
    ADVANCE_OR_ERROR("Unexpected EOF in argument list", nullptr);
    if (current_token.type == token_type::COMMA) {
      ADVANCE_OR_ERROR("Unexpected EOF in argument list", nullptr);
    } else if (current_token.type == token_type::PAREN_CLOSE) {
      break;
    } else {
      set_error("Unexpected token in argument list", current_token.loc);
      return nullptr;
    }
  } while (true);
  assert(current_token.type == token_type::PAREN_CLOSE);
  return call;
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
