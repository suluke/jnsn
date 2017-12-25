#include "jnsn/js/parser.h"
#include "jnsn/js/ast_analysis.h"
#include "jnsn/js/ast_ops.h"
#include "jnsn/util.h"
#include <algorithm>
#include <initializer_list>
#include <sstream>

using namespace jnsn;
template <class nodety> using res = parser_base::res<nodety>;

template <class resty>
static std::optional<parser_error> is_error(const resty &res) {
  if (std::holds_alternative<parser_error>(res)) {
    return std::get<parser_error>(res);
  }
  return std::nullopt;
}
template <class base, class child> static res<base> upcast_res(res<child> res) {
  if (std::holds_alternative<parser_error>(res))
    return std::get<parser_error>(res);
  return std::get<0>(res);
}

lexer_base::result parser_base::next_token() { return get_lexer().next(); }

static std::string to_string(token_type t) {
  switch (t) {
#define TOKEN_TYPE(NAME, STR)                                                  \
  case token_type::NAME:                                                       \
    return #NAME;
#include "jnsn/js/tokens.def"
  }
  unreachable("Unknown token_type");
}

#define ADVANCE_OR_ERROR(MESSAGE)                                              \
  do {                                                                         \
    auto adv = advance();                                                      \
    if (auto error = is_error(adv)) {                                          \
      return *error;                                                           \
    }                                                                          \
    if (!std::get<bool>(adv)) {                                                \
      return parser_error{MESSAGE, current_token.loc};                         \
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
      return parser_error{"Unexpected token. Expected: " #TYPES ". Was: " +    \
                              to_string(current_token.type),                   \
                          current_token.loc};                                  \
    }                                                                          \
  } while (false)

#define EXPECT(TYPE, RETURN_VAL)                                               \
  EXPECT_SEVERAL(TYPELIST(token_type::TYPE), RETURN_VAL)

#define ASSERT_PARSE_RESULT(VARNAME)                                           \
  do {                                                                         \
    if (std::holds_alternative<parser_error>(VARNAME)) {                       \
      return std::get<parser_error>(VARNAME);                                  \
    }                                                                          \
  } while (false)

#define SUBPARSE(VARNAME, PARSE_CALL)                                          \
  auto VARNAME##_or_err = PARSE_CALL;                                          \
  ASSERT_PARSE_RESULT(VARNAME##_or_err);                                       \
  auto *VARNAME = std::get<0>(VARNAME##_or_err);

namespace jnsn {
std::ostream &operator<<(std::ostream &stream, const parser_error &err) {
  return stream << err.msg;
}
} // namespace jnsn

std::variant<bool, parser_error> parser_base::advance() {
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
      return parser_error{"Lexer Error: " + err.msg, err.loc};
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
  module = nodes.make_module({0, 0});
  rewind_stack = {};
}

parser_base::result parser_base::parse(bool verify) {
  reset();
  for (;;) {
    auto adv = advance();
    if (auto error = is_error(adv)) {
      return *error;
    }
    if (!std::get<bool>(adv))
      break;
    SUBPARSE(stmt, parse_statement());
    module->stmts.emplace_back(stmt);
  }

  if (verify) {
    auto report = analyze_js_ast(*module);
    if (report) {
      std::stringstream ss;
      ss << "\n" << report;
      return parser_error{ss.str(), {0, 0}};
    }
  }
  return module;
}

static number_literal_node *make_number_expression(token t,
                                                   ast_node_store &nodes) {
  number_literal_node *res = nullptr;
  if (t.type == token_type::INT_LITERAL) {
    res = nodes.make_int_literal(t.loc);
  } else if (t.type == token_type::FLOAT_LITERAL) {
    res = nodes.make_float_literal(t.loc);
  } else if (t.type == token_type::HEX_LITERAL) {
    res = nodes.make_float_literal(t.loc);
  } else if (t.type == token_type::OCT_LITERAL) {
    res = nodes.make_float_literal(t.loc);
  } else if (t.type == token_type::BIN_LITERAL) {
    res = nodes.make_float_literal(t.loc);
  }
  assert(res && "Token not a (known) number literal");
  res->val = t.text;
  return res;
}

static bool is_stmt_end(token t) {
  switch (t.type) {
  case token_type::SEMICOLON:
  case token_type::PAREN_CLOSE: // e.g. for (stmt; expr; stmt) <--
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
#include "jnsn/js/operators.def"
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
#define PREFIX_OP_KW(TYPE, PRECEDENCE)                                         \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return true;
#include "jnsn/js/operators.def"
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
#include "jnsn/js/operators.def"
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
#define INFIX_OP_KW(TYPE, X, Y)                                                \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return true;
#include "jnsn/js/operators.def"
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

static bool is_var_decl_kw(token t) {
  auto kwty = lexer_base::get_keyword_type(t);
  return kwty == keyword_type::kw_var || kwty == keyword_type::kw_const ||
         kwty == keyword_type::kw_let;
}

static int get_precedence(token op) {
  switch (op.type) {
#define INFIX_OP(TYPE, PRECEDENCE, ASSOCIATIVITY)                              \
  case token_type::TYPE:                                                       \
    return PRECEDENCE;
#include "jnsn/js/operators.def"
  case token_type::KEYWORD: {
    auto kwty = lexer_base::get_keyword_type(op);
#define INFIX_OP_KW(TYPE, PRECEDENCE, ASSOCIATIVITY)                           \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return PRECEDENCE;
#include "jnsn/js/operators.def"
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
#include "jnsn/js/operators.def"
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
#define INFIX_OP_KW(TYPE, PRECEDENCE, ASSOCIATIVITY)                           \
  if (kwty == keyword_type::kw_##TYPE)                                         \
    return associativity::ASSOCIATIVITY;
#include "jnsn/js/operators.def"
  }
  unreachable("Unknown binary operator");
}

static unary_expr_node *make_unary_prefix_op(token op, expression_node *value,
                                             ast_node_store &nodes) {
  assert(is_unary_prefix_op(op));
  unary_expr_node *expr = nullptr;
  if (op.type == token_type::INCR) {
    expr = nodes.make_prefix_increment(op.loc);
  } else if (op.type == token_type::DECR) {
    expr = nodes.make_prefix_decrement(op.loc);
  } else if (op.type == token_type::PLUS) {
    expr = nodes.make_prefix_plus(op.loc);
  } else if (op.type == token_type::MINUS) {
    expr = nodes.make_prefix_minus(op.loc);
  } else if (op.type == token_type::EXMARK) {
    expr = nodes.make_not_expr(op.loc);
  } else if (op.type == token_type::TILDE) {
    expr = nodes.make_binverse_expr(op.loc);
  } else if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
    if (kwty == keyword_type::kw_typeof) {
      expr = nodes.make_typeof_expr(op.loc);
    } else if (kwty == keyword_type::kw_void) {
      expr = nodes.make_void_expr(op.loc);
    } else if (kwty == keyword_type::kw_delete) {
      expr = nodes.make_delete_expr(op.loc);
    }
  }
  assert(expr && "Unary prefix operator not implemented");
  expr->value = value;
  return expr;
}

static bin_op_expr_node *make_binary_expr(token op, expression_node *lhs,
                                          expression_node *rhs,
                                          ast_node_store &nodes) {
  assert(is_binary_operator(op));
  bin_op_expr_node *res = nullptr;
  // arithmetic
  if (op.type == token_type::PLUS)
    res = nodes.make_add(op.loc);
  if (op.type == token_type::MINUS)
    res = nodes.make_subtract(op.loc);
  if (op.type == token_type::ASTERISK)
    res = nodes.make_multiply(op.loc);
  if (op.type == token_type::SLASH)
    res = nodes.make_divide(op.loc);
  if (op.type == token_type::POW)
    res = nodes.make_pow_expr(op.loc);
  if (op.type == token_type::PERCENT)
    res = nodes.make_modulo_expr(op.loc);
  // comparison
  if (op.type == token_type::LT)
    res = nodes.make_less_expr(op.loc);
  if (op.type == token_type::LT_EQ)
    res = nodes.make_less_eq_expr(op.loc);
  if (op.type == token_type::GT)
    res = nodes.make_greater_expr(op.loc);
  if (op.type == token_type::GT_EQ)
    res = nodes.make_greater_eq_expr(op.loc);
  if (op.type == token_type::EQEQ)
    res = nodes.make_equals_expr(op.loc);
  if (op.type == token_type::EQEQEQ)
    res = nodes.make_strong_equals_expr(op.loc);
  if (op.type == token_type::NEQ)
    res = nodes.make_not_equals_expr(op.loc);
  if (op.type == token_type::NEQEQ)
    res = nodes.make_strong_not_equals_expr(op.loc);
  if (op.type == token_type::LOG_AND)
    res = nodes.make_log_and_expr(op.loc);
  if (op.type == token_type::LOG_OR)
    res = nodes.make_log_or_expr(op.loc);
  // bitwise
  if (op.type == token_type::LSHIFT)
    res = nodes.make_lshift_expr(op.loc);
  if (op.type == token_type::RSHIFT)
    res = nodes.make_rshift_expr(op.loc);
  if (op.type == token_type::LOG_RSHIFT)
    res = nodes.make_log_rshift_expr(op.loc);
  if (op.type == token_type::AMPERSAND)
    res = nodes.make_bitwise_and_expr(op.loc);
  if (op.type == token_type::VERT_BAR)
    res = nodes.make_bitwise_or_expr(op.loc);
  if (op.type == token_type::CARET)
    res = nodes.make_bitwise_xor_expr(op.loc);
  // assignments
  if (op.type == token_type::EQ)
    res = nodes.make_assign(op.loc);
  if (op.type == token_type::PLUS_EQ)
    res = nodes.make_add_assign(op.loc);
  if (op.type == token_type::MINUS_EQ)
    res = nodes.make_subtract_assign(op.loc);
  if (op.type == token_type::MUL_EQ)
    res = nodes.make_multiply_assign(op.loc);
  if (op.type == token_type::DIV_EQ)
    res = nodes.make_divide_assign(op.loc);
  if (op.type == token_type::MOD_EQ)
    res = nodes.make_modulo_assign(op.loc);
  if (op.type == token_type::POW_EQ)
    res = nodes.make_pow_assign(op.loc);
  if (op.type == token_type::LSH_EQ)
    res = nodes.make_lshift_assign(op.loc);
  if (op.type == token_type::RSH_EQ)
    res = nodes.make_rshift_assign(op.loc);
  if (op.type == token_type::LOG_RSH_EQ)
    res = nodes.make_log_rshift_assign(op.loc);
  if (op.type == token_type::AND_EQ)
    res = nodes.make_and_assign(op.loc);
  if (op.type == token_type::OR_EQ)
    res = nodes.make_or_assign(op.loc);
  if (op.type == token_type::CARET_EQ)
    res = nodes.make_xor_assign(op.loc);
  // other
  if (op.type == token_type::COMMA)
    res = nodes.make_comma_operator(op.loc);
  // keyword operators
  if (op.type == token_type::KEYWORD) {
    auto kwty = lexer_base::get_keyword_type(op);
    if (kwty == keyword_type::kw_instanceof)
      res = nodes.make_instanceof_expr(op.loc);
    if (kwty == keyword_type::kw_in)
      res = nodes.make_in_expr(op.loc);
  }
  assert(res && "make_binary_expr not implemented for operator");
  res->lhs = lhs;
  res->rhs = rhs;
  return res;
}

res<statement_node> parser_base::parse_statement() {
  statement_node *stmt = nullptr;
  if (current_token.type == token_type::SEMICOLON) {
    return nodes.make_empty_stmt(current_token.loc);
  } else if (current_token.type == token_type::BRACE_OPEN) {
    SUBPARSE(block, parse_block());
    return block;
  } else if (current_token.type == token_type::KEYWORD) {
    SUBPARSE(kw_stmt, parse_keyword_stmt());
    stmt = kw_stmt;
  } else if (current_token.type == token_type::IDENTIFIER) {
    auto ident = current_token;
    auto adv = advance();
    if (auto error = is_error(adv)) {
      return *error;
    } else if (!std::get<bool>(adv)) {
      SUBPARSE(expr, parse_expression(true));
      stmt = expr;
    } else if (current_token.type == token_type::COLON) {
      auto *label = nodes.make_label_stmt(current_token.loc);
      label->label = ident.text;
      ADVANCE_OR_ERROR("Unexpected EOF after label");
      SUBPARSE(follow, parse_statement());
      label->stmt = follow;
      stmt = label;
    } else {
      rewind(ident);
      SUBPARSE(expr, parse_expression(true));
      stmt = expr;
    }
  } else {
    // parse expression statement
    SUBPARSE(expr, parse_expression(true));
    stmt = expr;
  }
  assert(stmt);
  auto final_token = current_token;
  auto adv = advance();
  if (auto error = is_error(adv)) {
    return *error;
  } else if (std::get<bool>(adv)) {
    if (!is_stmt_end(current_token)) {
      return parser_error{"Unexpected token after statement: " +
                              to_string(current_token.type),
                          current_token.loc};
    }
    if (current_token.type != token_type::SEMICOLON) {
      rewind(final_token);
    }
  }
  return stmt;
}

res<statement_node> parser_base::parse_keyword_stmt() {
  assert(current_token.type == token_type::KEYWORD);
  auto kwty = lexer_base::get_keyword_type(current_token);
  if (kwty == keyword_type::kw_function) {
    return upcast_res<statement_node>(parse_function_stmt());
  } else if (kwty == keyword_type::kw_if) {
    return upcast_res<statement_node>(parse_if_stmt());
  } else if (kwty == keyword_type::kw_do) {
    return upcast_res<statement_node>(parse_do_while());
  } else if (kwty == keyword_type::kw_while) {
    return upcast_res<statement_node>(parse_while_stmt());
  } else if (kwty == keyword_type::kw_for) {
    return upcast_res<statement_node>(parse_for_stmt());
  } else if (kwty == keyword_type::kw_switch) {
    return upcast_res<statement_node>(parse_switch_stmt());
  } else if (kwty == keyword_type::kw_break) {
    return nodes.make_break_stmt(current_token.loc); // FIXME break LABEL
  } else if (kwty == keyword_type::kw_continue) {
    return nodes.make_continue_stmt(current_token.loc); // FIXME continue LABEL
  } else if (kwty == keyword_type::kw_return) {
    return upcast_res<statement_node>(parse_return_stmt());
  } else if (kwty == keyword_type::kw_throw) {
    return upcast_res<statement_node>(parse_throw_stmt());
  } else if (kwty == keyword_type::kw_try) {
    return upcast_res<statement_node>(parse_try_stmt());
  } else if (kwty == keyword_type::kw_import) {
    return upcast_res<statement_node>(parse_import());
  } else if (kwty == keyword_type::kw_export) {
    return upcast_res<statement_node>(parse_export());
  } else if (kwty == keyword_type::kw_class) {
    return upcast_res<statement_node>(parse_class_stmt());
  } else if (kwty == keyword_type::kw_super) {
    auto *id = nodes.make_identifier_expr(current_token.loc);
    id->str = current_token.text;
    return upcast_res<statement_node>(parse_call(id));
  } else if (is_var_decl_kw(current_token)) {
    return upcast_res<statement_node>(parse_var_decl());
  } else {
    return upcast_res<statement_node>(parse_keyword_expr());
  }
}

res<expression_node> parser_base::parse_keyword_expr() {
  assert(current_token.type == token_type::KEYWORD);
  if (is_unary_prefix_op(current_token)) {
    return parse_unary_or_atomic_expr();
  }
  return parse_atomic_keyword_expr();
}

res<if_stmt_node> parser_base::parse_if_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_if);
  auto *if_stmt = nodes.make_if_stmt(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF after if");
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after if (");
  SUBPARSE(condition, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF after if condition");
  EXPECT(PAREN_CLOSE, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF. Expected if body");
  SUBPARSE(body, parse_statement());
  if_stmt->condition = condition;
  if_stmt->body = body;
  auto last_token = current_token;
  auto adv = advance();
  if (auto error = is_error(adv)) {
    return *error;
  } else if (std::get<bool>(adv)) {
    if (current_token.type == token_type::KEYWORD &&
        lexer_base::get_keyword_type(current_token) == keyword_type::kw_else) {
      ADVANCE_OR_ERROR("Unexpected EOF after else");
      SUBPARSE(else_stmt, parse_statement());
      if_stmt->else_stmt = else_stmt;
    } else {
      rewind(last_token);
    }
  }
  return if_stmt;
}

res<do_while_node> parser_base::parse_do_while() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_do);
  auto *dowhile_stmt = nodes.make_do_while(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF after do");
  SUBPARSE(body, parse_statement());
  ADVANCE_OR_ERROR("Unexpected EOF. Expected 'while'");
  EXPECT(KEYWORD, nullptr);
  if (lexer_base::get_keyword_type(current_token) != keyword_type::kw_while) {
    return parser_error{"Expected while after do", current_token.loc};
  }
  ADVANCE_OR_ERROR("Unexpected EOF after do...while");
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after do...while(");
  SUBPARSE(condition, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF after do...while condition");
  EXPECT(PAREN_CLOSE, nullptr);
  dowhile_stmt->body = body;
  dowhile_stmt->condition = condition;
  return dowhile_stmt;
}

res<while_stmt_node> parser_base::parse_while_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_while);
  auto *while_stmt = nodes.make_while_stmt(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF after while");
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after while(");
  SUBPARSE(condition, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF after while condition");
  EXPECT(PAREN_CLOSE, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF. Expected while body");
  SUBPARSE(body, parse_statement());
  while_stmt->condition = condition;
  while_stmt->body = body;
  return while_stmt;
}

res<statement_node> parser_base::parse_for_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_for);
  auto for_tok = current_token;
  ADVANCE_OR_ERROR("Unexpected EOF after for");
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after for (");

  std::optional<token> keyword;
  if (current_token.type == token_type::KEYWORD &&
      is_var_decl_kw(current_token)) {
    keyword = current_token;
    ADVANCE_OR_ERROR("Unexpected EOF after variable decl keyword");
    EXPECT(IDENTIFIER, nullptr);
  }
  // Try for (... in ... ) or for (... of ...)
  if (current_token.type == token_type::IDENTIFIER) {
    auto var = current_token;
    ADVANCE_OR_ERROR("Unexpected EOF in for head");
    if (current_token.type == token_type::IDENTIFIER &&
        current_token.text == "of") {
      ADVANCE_OR_ERROR("Unexpected EOF after for (... of");
      SUBPARSE(iterable, parse_expression(true));
      ADVANCE_OR_ERROR("Unexpected EOF after for (... of <iterable>");
      EXPECT(PAREN_CLOSE, nullptr);
      ADVANCE_OR_ERROR("Unexpected EOF after for (... of <iterable>)");
      SUBPARSE(body, parse_statement());
      auto *forof = nodes.make_for_of(for_tok.loc);
      if (keyword) {
        forof->keyword = keyword->text;
      }
      forof->var = var.text;
      forof->iterable = iterable;
      forof->body = body;
      return forof;
    } else if (current_token.type == token_type::KEYWORD &&
               lexer_base::get_keyword_type(current_token) ==
                   keyword_type::kw_in) {
      ADVANCE_OR_ERROR("Unexpected EOF after for (... in");
      SUBPARSE(iterable, parse_expression(true));
      ADVANCE_OR_ERROR("Unexpected EOF after for (... in <iterable>");
      EXPECT(PAREN_CLOSE, nullptr);
      ADVANCE_OR_ERROR("Unexpected EOF after for (... in <iterable>)");
      SUBPARSE(body, parse_statement());
      auto *forin = nodes.make_for_in(for_tok.loc);
      if (keyword) {
        forin->keyword = keyword->text;
      }
      forin->var = var.text;
      forin->iterable = iterable;
      forin->body = body;
      return forin;
    } else {
      rewind(var);
    }
  }
  if (keyword) {
    rewind(*keyword);
  }
  // parse c-style for (...;...;...)
  SUBPARSE(pre_stmt, parse_statement());
  EXPECT(SEMICOLON, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after for-loop pre-statement");
  SUBPARSE(condition, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF after for-loop condition");
  EXPECT(SEMICOLON, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after for-loop condition;");
  SUBPARSE(latch_stmt, parse_statement());
  ADVANCE_OR_ERROR("Unexpected EOF after for-loop latch stmt");
  EXPECT(PAREN_CLOSE, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after for(...)");
  SUBPARSE(body, parse_statement());
  auto *for_stmt = nodes.make_for_stmt(for_tok.loc);
  for_stmt->pre_stmt = pre_stmt;
  for_stmt->condition = condition;
  for_stmt->latch_stmt = latch_stmt;
  for_stmt->body = body;
  return for_stmt;
}

res<switch_stmt_node> parser_base::parse_switch_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_switch);
  auto *switch_stmt = nodes.make_switch_stmt(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF after switch");
  EXPECT(PAREN_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after switch (");
  SUBPARSE(value, parse_expression(true));
  switch_stmt->value = value;
  ADVANCE_OR_ERROR("Unexpected EOF after switch value");
  EXPECT(PAREN_CLOSE, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after switch (value)");
  EXPECT(BRACE_OPEN, nullptr);
  ADVANCE_OR_ERROR("Unexpected EOF after switch (value) {");
  bool hasDefault = false;
  do {
    EXPECT_SEVERAL(TYPELIST(token_type::KEYWORD, token_type::BRACE_CLOSE),
                   nullptr);
    if (current_token.type == token_type::BRACE_CLOSE) {
      break;
    }
    auto loc = current_token.loc;
    auto kwty = lexer_base::get_keyword_type(current_token);
    switch_clause_node *clause = nullptr;
    if (kwty == keyword_type::kw_default) {
      if (hasDefault) {
        return parser_error{"Switch statement already has a default clause",
                            current_token.loc};
      }
      hasDefault = true;
      clause = nodes.make_switch_clause(loc);
      ADVANCE_OR_ERROR("Unexpected EOF after default");
    } else if (kwty == keyword_type::kw_case) {
      auto *case_clause = nodes.make_case(loc);
      ADVANCE_OR_ERROR("Unexpected EOF after case");
      SUBPARSE(condition, parse_expression(true));
      case_clause->condition = condition;
      ADVANCE_OR_ERROR("Unexpected EOF after case condition");
      clause = case_clause;
    } else {
      return parser_error{"Unexpected keyword in switch", current_token.loc};
    }
    EXPECT(COLON, nullptr);
    switch_stmt->clauses.emplace_back(clause);
    ADVANCE_OR_ERROR("Unexpected EOF after colon (switch clause)");
    do {
      if (current_token.type == token_type::KEYWORD) {
        auto kwty = lexer_base::get_keyword_type(current_token);
        if (kwty == keyword_type::kw_case || kwty == keyword_type::kw_default) {
          break;
        }
      } else if (current_token.type == token_type::BRACE_CLOSE) {
        break;
      }
      SUBPARSE(stmt, parse_statement());
      clause->stmts.emplace_back(stmt);
      ADVANCE_OR_ERROR("Unexpected EOF in switch clauses block");
    } while (true);
  } while (true);
  return switch_stmt;
}

res<return_stmt_node> parser_base::parse_return_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_return);
  auto *ret = nodes.make_return_stmt(current_token.loc);
  auto adv = advance();
  if (auto error = is_error(adv))
    return *error;
  if (std::get<bool>(adv) && !is_stmt_end(current_token)) {
    SUBPARSE(expr, parse_expression(true));
    ret->value = expr;
  }
  return ret;
}

res<throw_stmt_node> parser_base::parse_throw_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_throw);
  auto *thro = nodes.make_throw_stmt(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF after throw");
  SUBPARSE(expr, parse_expression(true));
  thro->value = expr;
  return thro;
}

res<try_stmt_node> parser_base::parse_try_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_try);
  auto *try_stmt = nodes.make_try_stmt(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF after try");
  EXPECT(BRACE_OPEN, nullptr);
  SUBPARSE(body, parse_block());
  try_stmt->body = body;
  ADVANCE_OR_ERROR("Unexpected EOF after try {}");
  if (current_token.type == token_type::KEYWORD &&
      lexer_base::get_keyword_type(current_token) == keyword_type::kw_catch) {
    auto *ctch = nodes.make_catch(current_token.loc);
    ADVANCE_OR_ERROR("Unexpected EOF after catch");
    EXPECT(PAREN_OPEN, nullptr);
    ADVANCE_OR_ERROR("Unexpected EOF after catch(");
    EXPECT(IDENTIFIER, nullptr);
    auto id = current_token;
    ADVANCE_OR_ERROR("Unexpected EOF after catch(<name>");
    EXPECT(PAREN_CLOSE, nullptr);
    ADVANCE_OR_ERROR("Unexpected EOF after catch(<name>)");
    EXPECT(BRACE_OPEN, nullptr);
    SUBPARSE(catch_block, parse_block());
    ctch->var = id.text;
    ctch->body = catch_block;
    try_stmt->catch_block = ctch;
    auto adv = advance();
    if (auto error = is_error(adv))
      return *error;
    if (!std::get<bool>(adv)) {
      return try_stmt;
    }
  }
  if (current_token.type == token_type::KEYWORD &&
      lexer_base::get_keyword_type(current_token) == keyword_type::kw_finally) {
    ADVANCE_OR_ERROR("Unexpected EOF after finally");
    EXPECT(BRACE_OPEN, nullptr);
    SUBPARSE(finally_block, parse_block());
    try_stmt->finally = finally_block;
  }
  if (!try_stmt->catch_block && !try_stmt->finally) {
    return parser_error{"Encountered try without any catch or finally block",
                        current_token.loc};
  }
  return try_stmt;
}

res<expression_node> parser_base::parse_expression(bool comma_is_operator) {
  SUBPARSE(front, parse_unary_or_atomic_expr());
  expression_node *expr = front;
  auto final_token = current_token;
  auto adv = advance();
  if (auto error = is_error(adv))
    return *error;
  if (!std::get<bool>(adv))
    return expr;
  if (is_expression_end(current_token, comma_is_operator)) {
    rewind(final_token);
    return expr;
  }
  if (is_binary_operator(current_token, comma_is_operator)) {
    return upcast_res<expression_node>(parse_bin_op(expr, comma_is_operator));
  }
  rewind(final_token);
  return expr;
}

/// Parses everything with operator precedence >= 16
res<expression_node> parser_base::parse_unary_or_atomic_expr() {
  expression_node *expr = nullptr;
  if (is_unary_prefix_op(current_token)) {
    auto op = current_token;
    ADVANCE_OR_ERROR("Unexpected EOF after unary prefix operator");
    SUBPARSE(value, parse_atomic_expr());
    expr = make_unary_prefix_op(op, value, nodes);
  } else {
    SUBPARSE(atomic, parse_atomic_expr());
    expr = atomic;
  }
  assert(expr);
  return expr;
}

/// parses everything with operator precedence >= 17
res<expression_node> parser_base::parse_atomic_expr() {
  expression_node *expr = nullptr;
  if (current_token.type == token_type::KEYWORD) {
    SUBPARSE(atomic, parse_atomic_keyword_expr());
    expr = atomic;
  } else if (current_token.type == token_type::IDENTIFIER) {
    auto *id = nodes.make_identifier_expr(current_token.loc);
    id->str = current_token.text;
    expr = id;
  } else if (current_token.is_number_literal()) {
    SUBPARSE(num, parse_number_literal());
    expr = num;
  } else if (current_token.type == token_type::STRING_LITERAL ||
             current_token.type == token_type::TEMPLATE_STRING) {
    SUBPARSE(str, parse_string_literal());
    expr = str;
  } else if (current_token.type == token_type::TEMPLATE_HEAD) {
    SUBPARSE(tmplt, parse_template_literal());
    expr = tmplt;
  } else if (current_token.type == token_type::REGEX_LITERAL) {
    auto *regex = nodes.make_regex_literal(current_token.loc);
    regex->val = current_token.text;
    expr = regex;
  } else if (current_token.type == token_type::BRACKET_OPEN) {
    SUBPARSE(arr, parse_array_literal());
    expr = arr;
  } else if (current_token.type == token_type::BRACE_OPEN) {
    SUBPARSE(obj, parse_object_literal());
    expr = obj;
  } else if (current_token.type == token_type::PAREN_OPEN) {
    SUBPARSE(parens, parse_parens_expr());
    expr = parens;
  } else {
    return parser_error{"Unexpected token: " + to_string(current_token.type) +
                            ". Expected atomic expression",
                        current_token.loc};
  }
  /// parse everything up to operator precedence >= 18
  do {
    auto prev_token = current_token;
    auto adv = advance();
    if (auto error = is_error(adv))
      return *error;
    if (std::get<bool>(adv)) {
      if (current_token.type == token_type::DOT) {
        // member access
        SUBPARSE(acc, parse_member_access(expr));
        expr = acc;
      } else if (current_token.type == token_type::PAREN_OPEN) {
        // call
        SUBPARSE(call, parse_call(expr));
        expr = call;
      } else if (current_token.type == token_type::BRACKET_OPEN) {
        // computed member access
        SUBPARSE(acc, parse_computed_access(expr));
        expr = acc;
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
  auto adv = advance();
  if (auto error = is_error(adv))
    return *error;
  if (std::get<bool>(adv)) {
    if (current_token.type == token_type::INCR) {
      auto *incr = nodes.make_postfix_increment(current_token.loc);
      incr->value = expr;
      expr = incr;
    } else if (current_token.type == token_type::DECR) {
      auto *decr = nodes.make_postfix_decrement(current_token.loc);
      decr->value = expr;
      expr = decr;
    } else {
      rewind(prev_token);
    }
  }
  return expr;
}

res<expression_node> parser_base::parse_parens_expr() {
  assert(current_token.type == token_type::PAREN_OPEN);
  auto loc = current_token.loc;
  ADVANCE_OR_ERROR("Unexpected EOF after opening parenthesis");
  std::optional<token> reason_no_paramlist;
  std::optional<token> rest_param;
  std::vector<expression_node *> exprs;
  if (current_token.type != token_type::PAREN_CLOSE) {
    do {
      token begin = current_token;
      if (current_token.type == token_type::DOTDOTDOT) {
        ADVANCE_OR_ERROR("Unexpected EOF after rest operator");
        EXPECT(IDENTIFIER, nullptr);
        rest_param = current_token;
        rest_param->loc = begin.loc; // in case this causes an error later
        ADVANCE_OR_ERROR("Unexpected EOF in param list");
        EXPECT(PAREN_CLOSE, nullptr);
        break;
      }
      SUBPARSE(expr, parse_expression(false));
      if (!reason_no_paramlist && !isa<identifier_expr_node>(expr)) {
        reason_no_paramlist = begin;
      }
      exprs.emplace_back(expr);
      ADVANCE_OR_ERROR(
          "Unexpected EOF before closing parenthesis was encountered");
      EXPECT_SEVERAL(TYPELIST(token_type::PAREN_CLOSE, token_type::COMMA),
                     nullptr);
      if (current_token.type == token_type::PAREN_CLOSE) {
        break;
      } else if (current_token.type == token_type::COMMA) {
        ADVANCE_OR_ERROR(
            "Unexpected EOF before closing parenthesis was encountered");
      }
    } while (true);
  }
  EXPECT(PAREN_CLOSE, nullptr);
  auto paren_close = current_token;
  auto adv = advance();
  if (auto error = is_error(adv))
    return *error;
  if (std::get<bool>(adv)) {
    if (current_token.type == token_type::ARROW) {
      if (reason_no_paramlist) {
        return parser_error{"Invalid entry in arrow function param list",
                            reason_no_paramlist->loc};
      }
      std::vector<string_table::entry> param_names;
      std::transform(exprs.begin(), exprs.end(),
                     std::back_inserter(param_names),
                     [](expression_node *expr) {
                       return static_cast<identifier_expr_node *>(expr)->str;
                     });
      auto *params = nodes.make_param_list(loc);
      params->names = param_names;
      if (rest_param) {
        params->rest = rest_param->text;
      }
      ADVANCE_OR_ERROR("Unexpected EOF after arrow");
      statement_node *body = nullptr;
      if (current_token.type == token_type::BRACE_OPEN) {
        SUBPARSE(blk, parse_block());
        body = blk;
      } else {
        SUBPARSE(expr, parse_expression(false));
        body = expr;
      }
      assert(body);
      auto *func = nodes.make_arrow_function(loc);
      func->params = params;
      func->body = body;
      return func;
    }
    rewind(paren_close);
  }
  if (rest_param) {
    return parser_error{"Unexpected token", rest_param->loc};
  }
  auto *expr = exprs.front();
  auto it = exprs.begin();
  ++it;
  while (it != exprs.end()) {
    auto *comma =
        nodes.make_comma_operator(expr->loc); // FIXME bad location estimation
    comma->lhs = expr;
    comma->rhs = *it;
    expr = comma;
    ++it;
  }
  return expr;
}

res<bin_op_expr_node> parser_base::parse_bin_op(expression_node *lhs,
                                                bool comma_is_operator) {
  auto op = current_token;
  assert(is_binary_operator(op, comma_is_operator));
  ADVANCE_OR_ERROR(
      "Unexpected EOF. Expected right hand side argument of binary operation");
  bin_op_expr_node *binop = nullptr;
  // special case for ternary operator
  if (op.type == token_type::QMARK) {
    SUBPARSE(mid, parse_expression(false));
    ADVANCE_OR_ERROR(
        "Unexpected EOF. Expected colon for ternary operator (?:)");
    EXPECT(COLON, nullptr);
    ADVANCE_OR_ERROR(
        "Unexpected EOF. Expected third argument to ternary operator (?:)");
    SUBPARSE(rhs, parse_expression(false));

    auto *ternary = nodes.make_ternary_operator(op.loc);
    ternary->lhs = lhs;
    ternary->mid = mid;
    ternary->rhs = rhs;
    binop = ternary;
  } else {
    SUBPARSE(rhs, parse_unary_or_atomic_expr());
    binop = make_binary_expr(op, lhs, rhs, nodes);
  }

  auto prev_token = current_token;
  auto adv = advance();
  if (auto error = is_error(adv))
    return *error;
  if (!std::get<bool>(adv))
    return binop;
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
    } else if (next_prec == current_prec &&
               get_associativity(op) == associativity::RIGHT_TO_LEFT) {
      SUBPARSE(new_rhs, parse_bin_op(binop->rhs, comma_is_operator));
      binop->rhs = new_rhs;
    } else {
      SUBPARSE(new_binop, parse_bin_op(binop, comma_is_operator));
      binop = new_binop;
    }
    return binop;
  }

  return parser_error{"Unexpected token after binop expression",
                      current_token.loc};
}

res<expression_node> parser_base::parse_atomic_keyword_expr() {
  EXPECT(KEYWORD, nullptr);
  auto kwty = get_lexer().get_keyword_type(current_token);
  if (kwty == keyword_type::kw_class) {
    return upcast_res<expression_node>(parse_class_expr());
  } else if (kwty == keyword_type::kw_function) {
    return upcast_res<expression_node>(parse_function_expr());
  } else if (kwty == keyword_type::kw_new) {
    return upcast_res<expression_node>(parse_new_keyword());
  } else {
    return parser_error{"Not implemented (keyword)", current_token.loc};
  }
}

res<expression_node> parser_base::parse_new_keyword() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_new);
  auto loc = current_token.loc;
  ADVANCE_OR_ERROR("Unexpected EOF after new");
  if (current_token.type == token_type::DOT) {
    ADVANCE_OR_ERROR("Unexpected EOF after new.");
    EXPECT(IDENTIFIER, nullptr);
    if (static_cast<std::string_view>(current_token.text) != "target") {
      return parser_error{"Expected new.target after new.", current_token.loc};
    }
    return nodes.make_new_target(loc);
  }
  SUBPARSE(constructor, parse_atomic_expr());
  auto *new_expr = nodes.make_new_expr(loc);
  if (isa<call_expr_node>(constructor)) {
    auto *call = static_cast<call_expr_node *>(constructor);
    new_expr->constructor = call->callee;
    new_expr->args = call->args;
  } else {
    new_expr->constructor = constructor;
  }
  return new_expr;
}

res<statement_node> parser_base::parse_import() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_import);
  return parser_error{"Not implemented (parse_import)", current_token.loc};
}

res<statement_node> parser_base::parse_export() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_export);
  return parser_error{"Not implemented (parse_export)", current_token.loc};
}

res<class_stmt_node> parser_base::parse_class_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_class);
  return parser_error{"Not implemented (parse_class_stmt)", current_token.loc};
}

res<number_literal_node> parser_base::parse_number_literal() {
  auto literal = current_token;
  auto res = make_number_expression(current_token, nodes);
  res->val = current_token.text;
  auto adv = advance();
  if (auto error = is_error(adv))
    return *error;
  bool eof = !std::get<bool>(adv);
  if (eof || is_follow_expression(current_token)) {
    if (!eof) {
      rewind(literal);
    }
    return res;
  }
  return parser_error{"Unexpected token after number literal",
                      current_token.loc};
}

res<string_literal_node> parser_base::parse_string_literal() {
  assert(current_token.type == token_type::STRING_LITERAL ||
         current_token.type == token_type::TEMPLATE_STRING);
  auto str = current_token;
  auto res = nodes.make_string_literal(current_token.loc);
  res->val = current_token.text;
  auto adv = advance();
  if (auto error = is_error(adv))
    return *error;
  bool eof = !std::get<bool>(adv);
  if (eof || is_follow_expression(current_token)) {
    if (!eof) {
      rewind(str);
    }
    return res;
  }
  return parser_error{"Unexpected token after string literal",
                      current_token.loc};
}

res<template_literal_node> parser_base::parse_template_literal() {
  assert(current_token.type == token_type::TEMPLATE_HEAD);
  auto *tmplt = nodes.make_template_literal(current_token.loc);
  tmplt->strs.emplace_back(current_token.text);
  do {
    ADVANCE_OR_ERROR("Unexpected EOF in template literal");
    SUBPARSE(expr, parse_expression(true));
    tmplt->exprs.emplace_back(expr);
    ADVANCE_OR_ERROR(
        "Unexpected EOF after interpolated expression in template literal");
    EXPECT_SEVERAL(
        TYPELIST(token_type::TEMPLATE_MIDDLE, token_type::TEMPLATE_END),
        nullptr);
    tmplt->strs.emplace_back(current_token.text);
  } while (current_token.type == token_type::TEMPLATE_MIDDLE);
  assert(tmplt->strs.size() == tmplt->exprs.size() + 1);
  return tmplt;
}

res<function_stmt_node> parser_base::parse_function_stmt() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_function);
  auto func = nodes.make_function_stmt(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function");
  EXPECT(IDENTIFIER, nullptr);
  func->name = current_token.text;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function");
  EXPECT(PAREN_OPEN, nullptr);
  SUBPARSE(params, parse_param_list());
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function");
  EXPECT(BRACE_OPEN, nullptr);
  SUBPARSE(body, parse_block());
  func->body = body;
  return func;
}

res<function_expr_node> parser_base::parse_function_expr() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) ==
             keyword_type::kw_function);
  auto func = nodes.make_function_expr(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function");
  if (current_token.type == token_type::IDENTIFIER) {
    func->name = current_token.text;
    ADVANCE_OR_ERROR("Unexpected EOF while parsing function");
  }
  EXPECT(PAREN_OPEN, nullptr);
  SUBPARSE(params, parse_param_list());
  func->params = params;
  ADVANCE_OR_ERROR("Unexpected EOF while parsing function");
  EXPECT(BRACE_OPEN, nullptr);
  SUBPARSE(body, parse_block());
  func->body = body;
  return func;
}

res<class_expr_node> parser_base::parse_class_expr() {
  assert(current_token.type == token_type::KEYWORD &&
         lexer_base::get_keyword_type(current_token) == keyword_type::kw_class);
  return parser_error{"Not implemented (parse_class_expr)", current_token.loc};
}

res<param_list_node> parser_base::parse_param_list() {
  assert(current_token.type == token_type::PAREN_OPEN);
  auto node = nodes.make_param_list(current_token.loc);
  do {
    ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list");
    if (current_token.type == token_type::IDENTIFIER) {
      node->names.emplace_back(current_token.text);
      ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list");
    }
  } while (current_token.type == token_type::COMMA);
  if (current_token.type == token_type::DOTDOTDOT) {
    ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list");
    if (current_token.type == token_type::IDENTIFIER) {
      node->rest = current_token.text;
      ADVANCE_OR_ERROR("Unexpected EOF while parsing parameter list");
    }
  }
  if (current_token.type == token_type::PAREN_CLOSE) {
    return node;
  }

  return parser_error{"Unexpected token in parameter list", current_token.loc};
}

res<block_node> parser_base::parse_block() {
  EXPECT(BRACE_OPEN, nullptr);
  auto block = nodes.make_block(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing block");
  while (current_token.type != token_type::BRACE_CLOSE) {
    assert(current_token.type != token_type::BRACE_OPEN);
    SUBPARSE(stmt, parse_statement());
    block->stmts.emplace_back(stmt);
    ADVANCE_OR_ERROR("Unexpected EOF while parsing block");
  }
  return block;
}

res<var_decl_node> parser_base::parse_var_decl() {
  assert(is_var_decl_kw(current_token));
  auto decl = nodes.make_var_decl(current_token.loc);
  decl->keyword = current_token.text;
  ADVANCE_OR_ERROR("Unecpected EOF while parsing variable declaration");
  EXPECT(IDENTIFIER, nullptr);
  auto *part = nodes.make_var_decl_part(current_token.loc);
  auto id = current_token;
  part->name = id.text;
  decl->parts.emplace_back(part);
  do {
    auto end_token = current_token;
    auto adv = advance();
    if (auto error = is_error(adv))
      return *error;
    if (std::get<bool>(adv)) {
      if (current_token.type == token_type::EQ) {
        ADVANCE_OR_ERROR(
            "Unexpected EOF in variable initialization. Expected expression");
        SUBPARSE(init, parse_expression(false));
        part->init = init;
      } else if (current_token.type == token_type::COMMA) {
        ADVANCE_OR_ERROR("Unexpected EOF in variable declaration");
        EXPECT(IDENTIFIER, nullptr);
        part = nodes.make_var_decl_part(current_token.loc);
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

res<array_literal_node> parser_base::parse_array_literal() {
  assert(current_token.type == token_type::BRACKET_OPEN);
  ADVANCE_OR_ERROR("Unexpected EOF inside array literal");
  auto *array = nodes.make_array_literal(current_token.loc);
  if (current_token.type != token_type::BRACKET_CLOSE) {
    do {
      expression_node *expr = nullptr;
      if (current_token.type == token_type::DOTDOTDOT) {
        auto *spread = nodes.make_spread_expr(current_token.loc);
        ADVANCE_OR_ERROR("Unexpected EOF after spread operator");
        SUBPARSE(tail, parse_expression(false));
        spread->list = tail;
        expr = spread;
      } else {
        SUBPARSE(sub, parse_expression(false));
        expr = sub;
      }
      array->values.emplace_back(expr);
      ADVANCE_OR_ERROR("Unexpected EOF inside array literal");
      EXPECT_SEVERAL(TYPELIST(token_type::BRACKET_CLOSE, token_type::COMMA),
                     nullptr);
      if (current_token.type == token_type::BRACKET_CLOSE) {
        break;
      }
      if (current_token.type == token_type::COMMA) {
        ADVANCE_OR_ERROR("Unexpected EOF inside array literal");
      }
    } while (true);
  }
  return array;
}

res<object_literal_node> parser_base::parse_object_literal() {
  assert(current_token.type == token_type::BRACE_OPEN);
  auto *object = nodes.make_object_literal(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF in object literal");
  do {
    if (current_token.type == token_type::BRACE_CLOSE) {
      break;
    } else if (current_token.type == token_type::DOTDOTDOT) {
      auto *spread = nodes.make_spread_expr(current_token.loc);
      ADVANCE_OR_ERROR("Unexpected EOF after spread operator");
      SUBPARSE(expr, parse_expression(false));
      spread->list = expr;
      object->entries.emplace_back(spread);
    } else if (is_possible_object_key(current_token)) {
      auto id = current_token;
      ADVANCE_OR_ERROR("Unexpected EOF in object literal");
      if (current_token.type != token_type::COLON) {
        auto *expr = nodes.make_identifier_expr(id.loc);
        expr->str = id.text;
        object->entries.emplace_back(expr);
        rewind(id);
      } else {
        ADVANCE_OR_ERROR("Unexpected EOF in object literal");
        auto *entry = nodes.make_object_entry(id.loc);
        entry->key = id.text;
        SUBPARSE(val, parse_expression(false));
        entry->val = val;
        object->entries.emplace_back(entry);
      }
    } else {
      return parser_error{"Unexpected token", current_token.loc};
    }
    ADVANCE_OR_ERROR("Unexpected EOF in object literal");
    EXPECT_SEVERAL(TYPELIST(token_type::BRACE_CLOSE, token_type::COMMA),
                   nullptr);
    if (current_token.type == token_type::COMMA) {
      ADVANCE_OR_ERROR("Unexpected EOF in object literal");
    }
  } while (true);
  EXPECT(BRACE_CLOSE, nullptr);
  return object;
}

res<computed_member_access_node>
parser_base::parse_computed_access(expression_node *base) {
  auto *access = nodes.make_computed_member_access(current_token.loc);
  assert(current_token.type == token_type::BRACKET_OPEN);
  ADVANCE_OR_ERROR("Unexpected EOF inside computed member access");
  SUBPARSE(member, parse_expression(true));
  ADVANCE_OR_ERROR("Unexpected EOF inside computed member access");
  EXPECT(BRACKET_CLOSE, nullptr);
  access->base = base;
  access->member = member;
  return access;
}
res<member_access_node>
parser_base::parse_member_access(expression_node *base) {
  assert(current_token.type == token_type::DOT);
  auto *node = nodes.make_member_access(current_token.loc);
  ADVANCE_OR_ERROR("Unexpected EOF while parsing member access");
  EXPECT(IDENTIFIER, nullptr);
  node->base = base;
  node->member = current_token.text;
  return node;
}

res<call_expr_node> parser_base::parse_call(expression_node *callee) {
  assert(current_token.type == token_type::PAREN_OPEN);
  auto *call = nodes.make_call_expr(current_token.loc);
  auto *args = nodes.make_argument_list(current_token.loc);
  call->callee = callee;
  call->args = args;

  ADVANCE_OR_ERROR("Unexpected EOF after begin of argument list");
  if (current_token.type == token_type::PAREN_CLOSE) {
    return call;
  }
  do {
    SUBPARSE(arg, parse_expression(false));
    args->values.emplace_back(arg);
    ADVANCE_OR_ERROR("Unexpected EOF in argument list");
    if (current_token.type == token_type::COMMA) {
      ADVANCE_OR_ERROR("Unexpected EOF in argument list");
    } else if (current_token.type == token_type::PAREN_CLOSE) {
      break;
    } else {
      return parser_error{"Unexpected token in argument list",
                          current_token.loc};
    }
  } while (true);
  assert(current_token.type == token_type::PAREN_CLOSE);
  return call;
}
