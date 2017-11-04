#include "parsing/ast_ops.h"
#include "parsing/parser.h"
#include "parsing/util.h"
#include <algorithm>
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
  unreachable("Unknown token_type");
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

#define ASSERT_PARSE_RESULT(VARNAME)                                           \
  do {                                                                         \
    if (error || !VARNAME) {                                                   \
      assert(error && !VARNAME);                                               \
      return nullptr;                                                          \
    }                                                                          \
  } while (false)

#define SUBPARSE(VARNAME, PARSE_CALL)                                          \
  auto *VARNAME = PARSE_CALL;                                                  \
  ASSERT_PARSE_RESULT(VARNAME);

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
  case token_type::BRACE_CLOSE:
    return true;
  default:
    return false;
  }
  return false;
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
  case token_type::LSHIFT:
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
  case token_type::COLON:
    return true;
  case token_type::COMMA:
    return !comma_is_operator;
  default:
    return false;
  }
}

static bool is_unary_prefix_op(token op) {
#define PREFIX_OP(TYPE, PRECEDENCE)                                            \
  if (op.type == token_type::TYPE)                                             \
    return true;
#include "parsing/operators.def"
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
#define PREFIX_OP_KW(TYPE, PRECEDENCE)                                         \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return true;
#include "parsing/operators.def"
  }
  return false;
}

static bool is_binary_operator(token op, bool comma_is_operator = true) {
  if (op.type == token_type::COMMA) {
    return comma_is_operator;
  }
#define INFIX_OP(TYPE, X, Y)                                                   \
  if (op.type == token_type::TYPE) {                                           \
    return true;                                                               \
  }
#include "parsing/operators.def"
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
#define INFIX_OP_KW(TYPE, X, Y)                                                \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return true;
#include "parsing/operators.def"
  }
  return false;
}

static bool is_possible_object_key(token t) {
  switch (t.type) {
  case token_type::INT_LITERAL:
  case token_type::FLOAT_LITERAL:
  case token_type::HEX_LITERAL:
  case token_type::OCT_LITERAL:
  case token_type::BIN_LITERAL:
  case token_type::STRING_LITERAL:
  case token_type::IDENTIFIER:
    return true;
  default:
    return false;
  }
  unreachable("Token type not caught by switch default");
}

statement_node *parser_base::parse_statement() {
  statement_node *stmt = nullptr;
  if (current_token.type == token_type::SEMICOLON) {
    return nodes.make_empty_stmt();
  } else if (current_token.type == token_type::BRACE_OPEN) {
    return parse_block_or_obj(false);
  } else if (current_token.type == token_type::KEYWORD) {
    stmt = parse_keyword_stmt();
  } else if (current_token.type == token_type::IDENTIFIER) {
    auto ident = current_token;
    if (!advance()) {
      stmt = parse_expression(true);
    } else if (current_token.type == token_type::COLON) {
      auto *label = nodes.make_label_stmt();
      label->label = ident.text;
      ADVANCE_OR_ERROR("Unexpected EOF after label", nullptr);
      SUBPARSE(follow, parse_statement());
      label->stmt = follow;
      stmt = label;
    } else {
      rewind(ident);
      stmt = parse_expression(true);
    }
  } else {
    // parse expression statement
    stmt = parse_expression(true);
  }
  ASSERT_PARSE_RESULT(stmt);
  auto final_token = current_token;
  auto read_success = advance();
  if (error) {
    return nullptr;
  }
  if (read_success) {
    if (!is_stmt_end(current_token)) {
      set_error("Unexpected token after statement: " +
                    to_string(current_token.type),
                current_token.loc);
      return nullptr;
    }
    if (current_token.type != token_type::SEMICOLON) {
      rewind(final_token);
    }
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
  } else if (kwty == keyword_type::kw_var || kwty == keyword_type::kw_const ||
             kwty == keyword_type::kw_let) {
    return parse_var_decl();
  } else {
    return parse_keyword_expr();
  }
}

expression_node *parser_base::parse_keyword_expr() {
  if (is_unary_prefix_op(current_token)) {
    return parse_unary_or_atomic_expr();
  }
  return parse_atomic_keyword_expr();
}

if_stmt_node *parser_base::parse_if_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_if);
  ADVANCE_OR_ERROR("Unexpected EOF after if", nullptr);
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after if (", nullptr);
  SUBPARSE(condition, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF after if condition", nullptr);
  EXPECT(PAREN_CLOSE, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF. Expected if body", nullptr);
  SUBPARSE(body, parse_statement());
  auto *if_stmt = nodes.make_if_stmt();
  if_stmt->condition = condition;
  if_stmt->body = body;
  auto last_token = current_token;
  if (advance()) {
    if (current_token.type == token_type::KEYWORD &&
        lexer_base::get_keyword_type(current_token) == keyword_type::kw_else) {
      ADVANCE_OR_ERROR("Unexpected EOF after else", nullptr);
      SUBPARSE(else_stmt, parse_statement());
      if_stmt->else_stmt = else_stmt;
    } else {
      rewind(last_token);
    }
  }
  return if_stmt;
}
do_while_node *parser_base::parse_do_while() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_do);
  ADVANCE_OR_ERROR("Unexpected EOF after do", nullptr);
  SUBPARSE(body, parse_statement());
  ADVANCE_OR_ERROR("Unexpected EOF. Expected 'while'", nullptr);
  EXPECT(KEYWORD, nullptr);
  if (lexer_base::get_keyword_type(current_token) != keyword_type::kw_while) {
    set_error("Expected while after do", current_token.loc);
    return nullptr;
  }
  ADVANCE_OR_ERROR("Unexpected EOF after do...while", nullptr);
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after do...while(", nullptr);
  SUBPARSE(condition, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF after do...while condition", nullptr);
  EXPECT(PAREN_CLOSE, nullptr);
  auto *dowhile_stmt = nodes.make_do_while();
  dowhile_stmt->body = body;
  dowhile_stmt->condition = condition;
  return dowhile_stmt;
}
while_stmt_node *parser_base::parse_while_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_while);
  ADVANCE_OR_ERROR("Unexpected EOF after while", nullptr);
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after while(", nullptr);
  SUBPARSE(condition, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF after while condition", nullptr);
  EXPECT(PAREN_CLOSE, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF. Expected while body", nullptr);
  SUBPARSE(body, parse_statement());
  auto *while_stmt = nodes.make_while_stmt();
  while_stmt->condition = condition;
  while_stmt->body = body;
  return while_stmt;
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

static int get_precedence(token op) {
  switch (op.type) {
#define INFIX_OP(TYPE, PRECEDENCE, ASSOCIATIVITY)                              \
  case token_type::TYPE:                                                       \
    return PRECEDENCE;
#include "parsing/operators.def"
  case token_type::KEYWORD: {
    auto kwty = lexer_base::get_keyword_type(op);
#define INFIX_OP_KW(TYPE, PRECEDENCE, ASSOCIATIVITY)                           \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return PRECEDENCE;
#include "parsing/operators.def"
  }
  default:
    return -1; // FIXME more explicit error handling
  }
}

enum class associativity { LEFT_TO_RIGHT, RIGHT_TO_LEFT };
static associativity get_associativity(token op) {
  assert(is_binary_operator(op));
#define INFIX_OP(TYPE, PRECEDENCE, ASSOCIATIVITY)                              \
  if (op.type == token_type::TYPE)                                             \
    return associativity::ASSOCIATIVITY;
#include "parsing/operators.def"
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
#define INFIX_OP_KW(TYPE, PRECEDENCE, ASSOCIATIVITY)                           \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return associativity::ASSOCIATIVITY;
#include "parsing/operators.def"
  }
  unreachable("Unknown binary operator");
}

expression_node *parser_base::parse_expression(bool comma_is_operator) {
  expression_node *expr = parse_unary_or_atomic_expr();
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
  } else if (error) {
    return nullptr;
  }
  set_error("Not implemented (parse expression)", current_token.loc);
  return nullptr;
}

static unary_expr_node *make_unary_prefix_op(token op, expression_node *value,
                                             ast_node_store &nodes) {
  assert(is_unary_prefix_op(op));
  unary_expr_node *expr = nullptr;
  if (op.type == token_type::INCR) {
    expr = nodes.make_prefix_increment();
  } else if (op.type == token_type::DECR) {
    expr = nodes.make_prefix_decrement();
  } else if (op.type == token_type::PLUS) {
    expr = nodes.make_prefix_plus();
  } else if (op.type == token_type::MINUS) {
    expr = nodes.make_prefix_minus();
  } else if (op.type == token_type::EXMARK) {
    expr = nodes.make_not_expr();
  } else if (op.type == token_type::TILDE) {
    expr = nodes.make_binverse_expr();
  } else if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
    if (kwty == keyword_type::kw_typeof) {
      expr = nodes.make_typeof_expr();
    } else if (kwty == keyword_type::kw_void) {
      expr = nodes.make_void_expr();
    } else if (kwty == keyword_type::kw_delete) {
      expr = nodes.make_delete_expr();
    }
  }
  assert(expr && "Unary prefix operator not implemented");
  expr->value = value;
  return expr;
}

/// Parses everything with operator precedence >= 16
expression_node *parser_base::parse_unary_or_atomic_expr() {
  expression_node *expr = nullptr;
  if (is_unary_prefix_op(current_token)) {
    auto op = current_token;
    ADVANCE_OR_ERROR("Unexpected EOF after unary prefix operator", nullptr);
    SUBPARSE(value, parse_atomic_expr());
    expr = make_unary_prefix_op(op, value, nodes);
  } else {
    expr = parse_atomic_expr();
  }
  ASSERT_PARSE_RESULT(expr);
  return expr;
}

/// parses everything with operator precedence >= 17
expression_node *parser_base::parse_atomic_expr() {
  expression_node *expr = nullptr;
  if (current_token.type == token_type::KEYWORD) {
    expr = parse_atomic_keyword_expr();
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
    expr = parse_parens_expr();
  }
  if (!expr) {
    if (!error) {
      set_error("Unexpected token: " + to_string(current_token.type) +
                    ". Expected atomix expression",
                current_token.loc);
    }
    return nullptr;
  }
  /// parse everything up to operator precedence >= 18
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
  /// parse everything up to operator precedence >= 17
  auto prev_token = current_token;
  if (advance()) {
    if (current_token.type == token_type::INCR) {
      auto *incr = nodes.make_postfix_increment();
      incr->value = expr;
      expr = incr;
    } else if (current_token.type == token_type::DECR) {
      auto *decr = nodes.make_postfix_decrement();
      decr->value = expr;
      expr = decr;
    } else {
      rewind(prev_token);
    }
  }
  return expr;
}

expression_node *parser_base::parse_parens_expr() {
  assert(current_token.type == token_type::PAREN_OPEN);
  ADVANCE_OR_ERROR("Unexpected EOF after opening parenthesis", nullptr);
  std::optional<token> reason_no_paramlist;
  std::optional<token> rest_param;
  std::vector<expression_node *> exprs;
  if (current_token.type != token_type::PAREN_CLOSE) {
    do {
      token begin = current_token;
      if (current_token.type == token_type::DOTDOTDOT) {
        ADVANCE_OR_ERROR("Unexpected EOF after rest operator", nullptr);
        EXPECT(IDENTIFIER, nullptr);
        rest_param = current_token;
        rest_param->loc = begin.loc; // in case this causes an error later
        ADVANCE_OR_ERROR("Unexpected EOF in param list", nullptr);
        EXPECT(PAREN_CLOSE, nullptr);
        break;
      }
      SUBPARSE(expr, parse_expression(false));
      if (!reason_no_paramlist && !isa<identifier_expr_node>(expr)) {
        reason_no_paramlist = begin;
      }
      exprs.emplace_back(expr);
      ADVANCE_OR_ERROR(
          "Unexpected EOF before closing parenthesis was encountered", nullptr);
      EXPECT_SEVERAL(TYPELIST(token_type::PAREN_CLOSE, token_type::COMMA),
                     nullptr);
      if (current_token.type == token_type::PAREN_CLOSE) {
        break;
      } else if (current_token.type == token_type::COMMA) {
        ADVANCE_OR_ERROR(
            "Unexpected EOF before closing parenthesis was encountered",
            nullptr);
      }
    } while (true);
  }
  EXPECT(PAREN_CLOSE, nullptr);
  auto paren_close = current_token;
  if (advance()) {
    if (current_token.type == token_type::ARROW) {
      if (reason_no_paramlist) {
        set_error("Invalid entry in arrow function param list",
                  reason_no_paramlist->loc);
        return nullptr;
      }
      std::vector<string_table::entry> param_names;
      std::transform(exprs.begin(), exprs.end(),
                     std::back_inserter(param_names),
                     [](expression_node *expr) {
                       return static_cast<identifier_expr_node *>(expr)->str;
                     });
      auto *params = nodes.make_param_list();
      params->names = param_names;
      if (rest_param) {
        params->rest = rest_param->text;
      }
      ADVANCE_OR_ERROR("Unexpected EOF after arrow", nullptr);
      statement_node *body = nullptr;
      if (current_token.type == token_type::BRACE_OPEN) {
        body = parse_block_or_obj(true);
      } else {
        body = parse_expression(false);
      }
      ASSERT_PARSE_RESULT(body);
      auto *func = nodes.make_arrow_function();
      func->params = params;
      func->body = body;
      return func;
    }
    rewind(paren_close);
  }
  if (rest_param) {
    set_error("Unexpected token", rest_param->loc);
    return nullptr;
  }
  auto *expr = exprs.front();
  auto it = exprs.begin();
  ++it;
  while (it != exprs.end()) {
    auto *comma = nodes.make_comma_operator();
    comma->lhs = expr;
    comma->rhs = *it;
    expr = comma;
    ++it;
  }
  return expr;
}

static bin_op_expr_node *make_binary_expr(token op, expression_node *lhs,
                                          expression_node *rhs,
                                          ast_node_store &nodes) {
  assert(is_binary_operator(op));
  bin_op_expr_node *res = nullptr;
  // arithmetic
  if (op.type == token_type::PLUS)
    res = nodes.make_add();
  if (op.type == token_type::MINUS)
    res = nodes.make_subtract();
  if (op.type == token_type::ASTERISK)
    res = nodes.make_multiply();
  if (op.type == token_type::SLASH)
    res = nodes.make_divide();
  if (op.type == token_type::POW)
    res = nodes.make_pow_expr();
  if (op.type == token_type::PERCENT)
    res = nodes.make_modulo_expr();
  // comparison
  if (op.type == token_type::LT)
    res = nodes.make_less_expr();
  if (op.type == token_type::LT_EQ)
    res = nodes.make_less_eq_expr();
  if (op.type == token_type::GT)
    res = nodes.make_greater_expr();
  if (op.type == token_type::GT_EQ)
    res = nodes.make_greater_eq_expr();
  if (op.type == token_type::EQEQ)
    res = nodes.make_equals_expr();
  if (op.type == token_type::EQEQEQ)
    res = nodes.make_strong_equals_expr();
  if (op.type == token_type::NEQ)
    res = nodes.make_not_equals_expr();
  if (op.type == token_type::NEQEQ)
    res = nodes.make_strong_not_equals_expr();
  if (op.type == token_type::LOG_AND)
    res = nodes.make_log_and_expr();
  if (op.type == token_type::LOG_OR)
    res = nodes.make_log_or_expr();
  // bitwise
  if (op.type == token_type::LSHIFT)
    res = nodes.make_lshift_expr();
  if (op.type == token_type::RSHIFT)
    res = nodes.make_rshift_expr();
  if (op.type == token_type::LOG_RSHIFT)
    res = nodes.make_log_rshift_expr();
  if (op.type == token_type::AMPERSAND)
    res = nodes.make_bitwise_and_expr();
  if (op.type == token_type::VERT_BAR)
    res = nodes.make_bitwise_or_expr();
  if (op.type == token_type::CARET)
    res = nodes.make_bitwise_xor_expr();
  // assignments
  if (op.type == token_type::EQ)
    res = nodes.make_assign();
  if (op.type == token_type::PLUS_EQ)
    res = nodes.make_add_assign();
  if (op.type == token_type::MINUS_EQ)
    res = nodes.make_subtract_assign();
  if (op.type == token_type::MUL_EQ)
    res = nodes.make_multiply_assign();
  if (op.type == token_type::DIV_EQ)
    res = nodes.make_divide_assign();
  if (op.type == token_type::MOD_EQ)
    res = nodes.make_modulo_assign();
  if (op.type == token_type::POW_EQ)
    res = nodes.make_pow_assign();
  if (op.type == token_type::LSH_EQ)
    res = nodes.make_lshift_assign();
  if (op.type == token_type::RSH_EQ)
    res = nodes.make_rshift_assign();
  if (op.type == token_type::LOG_RSH_EQ)
    res = nodes.make_log_rshift_assign();
  if (op.type == token_type::AND_EQ)
    res = nodes.make_and_assign();
  if (op.type == token_type::OR_EQ)
    res = nodes.make_or_assign();
  if (op.type == token_type::CARET_EQ)
    res = nodes.make_xor_assign();
  // other
  if (op.type == token_type::COMMA)
    res = nodes.make_comma_operator();
  // keyword operators
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
    if (kwty == keyword_type::kw_instanceof)
      res = nodes.make_instanceof_expr();
    if (kwty == keyword_type::kw_in)
      res = nodes.make_in_expr();
  }
  assert(res && "make_binary_expr not implemented for operator");
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
  bin_op_expr_node *binop = nullptr;
  // special case for ternary operator
  if (op.type == token_type::QMARK) {
    SUBPARSE(mid, parse_expression(false));
    ADVANCE_OR_ERROR("Unexpected EOF. Expected colon for ternary operator (?:)",
                     nullptr);
    EXPECT(COLON, nullptr);
    ADVANCE_OR_ERROR(
        "Unexpected EOF. Expected third argument to ternary operator (?:)",
        nullptr);
    SUBPARSE(rhs, parse_expression(false));

    auto *ternary = nodes.make_ternary_operator();
    ternary->lhs = lhs;
    ternary->mid = mid;
    ternary->rhs = rhs;
    binop = ternary;
  } else {
    SUBPARSE(rhs, parse_unary_or_atomic_expr());
    binop = make_binary_expr(op, lhs, rhs, nodes);
  }

  auto prev_token = current_token;
  if (!advance()) {
    return binop;
  }
  if (is_expression_end(current_token, comma_is_operator)) {
    rewind(prev_token);
    return binop;
  }
  // Extend rhs if followed by stronger-binding operator
  if (is_binary_operator(current_token, comma_is_operator)) {
    auto current_prec = get_precedence(op);
    auto next_prec = get_precedence(current_token);
    if (next_prec > current_prec) {
      SUBPARSE(new_rhs, parse_bin_op(binop->rhs, comma_is_operator));
      binop->rhs = new_rhs;
    }
    if (next_prec == current_prec &&
        get_associativity(op) == associativity::RIGHT_TO_LEFT) {
      SUBPARSE(new_rhs, parse_bin_op(binop->rhs, comma_is_operator));
      binop->rhs = new_rhs;
    }
    return binop;
  }

  set_error("Unexpected token after binop expression", current_token.loc);
  return nullptr;
}

expression_node *parser_base::parse_atomic_keyword_expr() {
  EXPECT(KEYWORD, nullptr);
  auto kw_ty = get_lexer().get_keyword_type(current_token);
  if (kw_ty == keyword_type::kw_function) {
    return parse_function_expr();
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
  SUBPARSE(params, parse_param_list());
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  EXPECT(BRACE_OPEN, nullptr);
  SUBPARSE(body, parse_block());
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
  SUBPARSE(params, parse_param_list());
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function", nullptr);
  EXPECT(BRACE_OPEN, nullptr);
  SUBPARSE(body, parse_block());
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
  assert(current_token.type == token_type::KEYWORD);
  auto kw_ty = lexer_base::get_keyword_type(current_token);
  assert(kw_ty == keyword_type::kw_var || kw_ty == keyword_type::kw_const ||
         kw_ty == keyword_type::kw_let);
  auto decl = nodes.make_var_decl();
  decl->keyword = current_token.text;
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration",
                   nullptr);
  EXPECT(IDENTIFIER, nullptr);
  auto *part = nodes.make_var_decl_part();
  auto id = current_token;
  part->name = id.text;
  decl->parts.emplace_back(part);
  do {
    auto end_token = current_token;
    if (advance()) {
      if (current_token.type == token_type::EQ) {
        ADVANCE_OR_ERROR(
            "Unexpected EOF in variable initialization. Expected expression",
            nullptr);
        SUBPARSE(init, parse_expression(false));
        part->init = init;
      } else if (current_token.type == token_type::COMMA) {
        ADVANCE_OR_ERROR("Unexpected EOF in variable declaration", nullptr);
        EXPECT(IDENTIFIER, nullptr);
        part = nodes.make_var_decl_part();
        part->name = current_token.text;
        decl->parts.emplace_back(part);
      } else {
        rewind(end_token);
        break;
      }
    } else {
      break;
    }
  } while (true);
  return decl;
}

array_literal_node *parser_base::parse_array_literal() {
  assert(current_token.type == token_type::BRACKET_OPEN);
  ADVANCE_OR_ERROR("Unexpected EOF inside array literal", nullptr);
  auto *array = nodes.make_array_literal();
  if (current_token.type != token_type::BRACKET_CLOSE) {
    do {
      expression_node *expr = nullptr;
      if (current_token.type == token_type::DOTDOTDOT) {
        ADVANCE_OR_ERROR("Unexpected EOF after spread operator", nullptr);
        auto *spread = nodes.make_spread_expr();
        expr = parse_expression(false);
        spread->list = expr;
        expr = spread;
      } else {
        expr = parse_expression(false);
      }
      array->values.emplace_back(expr);
      ADVANCE_OR_ERROR("Unexpected EOF inside array literal", nullptr);
      EXPECT_SEVERAL(TYPELIST(token_type::BRACKET_CLOSE, token_type::COMMA),
                     nullptr);
      if (current_token.type == token_type::BRACKET_CLOSE) {
        break;
      }
      if (current_token.type == token_type::COMMA) {
        ADVANCE_OR_ERROR("Unexpected EOF inside array literal", nullptr);
      }
    } while (true);
  }
  return array;
}
object_literal_node *parser_base::parse_object_literal() {
  assert(current_token.type == token_type::BRACE_OPEN);
  ADVANCE_OR_ERROR("Unexpected EOF in object literal", nullptr);
  auto *object = nodes.make_object_literal();
  do {
    if (current_token.type == token_type::BRACE_CLOSE) {
      break;
    } else if (current_token.type == token_type::DOTDOTDOT) {
      ADVANCE_OR_ERROR("Unexpected EOF after spread operator", nullptr);
      auto *expr = parse_expression(false);
      auto *spread = nodes.make_spread_expr();
      spread->list = expr;
      object->entries.emplace_back(spread);
    } else if (is_possible_object_key(current_token)) {
      auto id = current_token;
      ADVANCE_OR_ERROR("Unexpected EOF in object literal", nullptr);
      if (current_token.type != token_type::COLON) {
        auto *expr = nodes.make_identifier_expr();
        expr->str = id.text;
        object->entries.emplace_back(expr);
        rewind(id);
      } else {
        ADVANCE_OR_ERROR("Unexpected EOF in object literal", nullptr);
        auto *entry = nodes.make_object_entry();
        entry->key = id.text;
        auto *val = parse_expression(false);
        entry->val = val;
        object->entries.emplace_back(entry);
      }
    } else {
      set_error("Unexpected token", current_token.loc);
      return nullptr;
    }
    ADVANCE_OR_ERROR("Unexpected EOF in object literal", nullptr);
    EXPECT_SEVERAL(TYPELIST(token_type::BRACE_CLOSE, token_type::COMMA),
                   nullptr);
    if (current_token.type == token_type::COMMA) {
      ADVANCE_OR_ERROR("Unexpected EOF in object literal", nullptr);
    }
  } while (true);
  EXPECT(BRACE_CLOSE, nullptr);
  return object;
}
computed_member_access_node *
parser_base::parse_computed_access(expression_node *base) {
  assert(current_token.type == token_type::BRACKET_OPEN);
  ADVANCE_OR_ERROR("Unexpected EOF inside computed member access", nullptr);
  SUBPARSE(member, parse_expression(true));
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
    SUBPARSE(arg, parse_expression(false));
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
statement_node *parser_base::parse_block_or_obj(bool prefer_block_over_obj) {
  EXPECT(BRACE_OPEN, nullptr);
  // Originally, I thought it should be possible to discern object literals
  // from blocks - since browsers' developer consoles can do it, too.
  // But it turns out this is pretty much impossible using standard parsing
  // techniques - and also not part of the spec.
  // See stackoverflow.com/questions/8089737/javascript-object-parsing
  // for more
  return parse_block();
}
