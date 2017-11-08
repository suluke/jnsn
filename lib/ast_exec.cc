#include "parsing/ast.h"
#include "parsing/ast_exec.h"
#include <cmath>
#include <cstdlib>
#include <map>

using namespace parsing;

namespace parsing {
std::ostream &operator<<(std::ostream &stream, const exec_error error) {
  return stream << error.msg;
}
} // namespace parsing

exec_value_base &exec_value::upcast_content() {
  return std::visit([](auto &val) -> exec_value_base & { return val; }, val);
}

void exec_value::print(std::ostream &stream) { upcast_content().print(stream); }

array_value::array_value() = default;
void array_value::print(std::ostream &stream) {
  stream << "[";
  if (!elms.empty()) {
    auto It = elms.begin();
    It->print(stream);
    ++It;
    while (It != elms.end()) {
      stream << ", ";
      It->print(stream);
      ++It;
    }
  }
  stream << "]";
}

exec_value exec_vm::lookup(std::string_view identifier) {
  static const std::map<std::string_view, exec_value> builtin_values = {
      {"undefined", undefined_value{}},
      {"null", null_value{}},
      {"true", bool_value{true}},
      {"false", bool_value{false}}};
  if (builtin_values.count(identifier)) {
    return builtin_values.at(identifier);
  }
  return undefined_value{};
}

struct exec_visitor : const_ast_node_visitor<ast_executor::result> {
  using result = ast_executor::result;
  exec_vm &vm;
  exec_visitor(exec_vm &vm) : vm(vm) {}

  result accept(const statement_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const module_node &node) override {
    result res = exec_value{undefined_value{}};
    for (auto *stmt : node.stmts) {
      res = visit(*stmt);
      if (std::holds_alternative<exec_error>(res)) {
        break;
      }
    }
    return res;
  }
  result accept(const expression_node &) override {
    return exec_error{"Encountered abstract class expression_node"};
  }
  result accept(const param_list_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const block_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const function_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const class_func_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const class_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const arrow_function_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const identifier_expr_node &node) override {
    return vm.lookup(node.str);
  }

  result accept(const number_literal_node &) override {
    return exec_error{"Encountered abstract class number_literal"};
  }
  result accept(const int_literal_node &node) override {
    const auto *begin = node.val.data();
    char *end = nullptr;
    auto val = static_cast<double>(std::strtol(begin, &end, 10));
    return exec_value(number_value(val));
  }
  result accept(const float_literal_node &node) override {
    const auto *begin = node.val.data();
    char *end = nullptr;
    auto val = static_cast<double>(std::strtof(begin, &end));
    return exec_value(number_value(val));
  }
  result accept(const hex_literal_node &node) override {
    const auto *begin = node.val.data();
    char *end = nullptr;
    auto val = static_cast<double>(std::strtol(begin, &end, 16));
    return exec_value(number_value(val));
  }
  result accept(const oct_literal_node &node) override {
    const auto *begin = node.val.data() + 2;
    char *end = nullptr;
    auto val = static_cast<double>(std::strtol(begin, &end, 8));
    return exec_value(number_value(val));
  }
  result accept(const bin_literal_node &node) override {
    const auto *begin = node.val.data() + 2;
    char *end = nullptr;
    auto val = static_cast<double>(std::strtol(begin, &end, 2));
    return exec_value(number_value(val));
  }
  result accept(const string_literal_node &node) override {
    return exec_value(string_value(node.val->substr(1, node.val->size() - 2)));
  }
  result accept(const regex_literal_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const template_string_node &node) override {
    return exec_value(string_value(node.val->substr(1, node.val->size() - 2)));
  }
  result accept(const template_literal_node &node) override {
    return exec_error{"Not implemented"};
  }
  result accept(const array_literal_node &node) override {
    array_value arr;
    for (auto *val : node.values) {
      auto res = visit(*val);
      if (std::holds_alternative<exec_error>(res)) {
        return res;
      }
      arr.elms.emplace_back(std::get<exec_value>(res));
    }
    return exec_value(arr);
  }
  result accept(const object_entry_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const object_literal_node &) override {
    return exec_error{"Not implemented"};
  }

  result accept(const member_access_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const computed_member_access_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const argument_list_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const call_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const spread_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const new_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const new_target_node &) override {
    return exec_error{"Not implemented"};
  }
  // Unary expressions
  result accept(const unary_expr_node &) override {
    return exec_error{"Encountered abstract class unary_expr_node"};
  }
  result accept(const postfix_increment_node &node) override {
    return exec_error{"Not implemented"};
  }
  result accept(const postfix_decrement_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const prefix_increment_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const prefix_decrement_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const prefix_plus_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const prefix_minus_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const not_expr_node &node) override {
    auto subexpr = visit(node);
    if (std::holds_alternative<exec_error>(subexpr)) {
      return subexpr;
    }
    auto val = std::get<exec_value>(subexpr);
    if (isa<bool_value>(val)) {
      return {bool_value(val.get<bool_value>().value)};
    }
    return exec_error{"Not implemented"};
  }
  result accept(const binverse_expr_node &node) override {
    auto subexpr = visit(*node.value);
    if (std::holds_alternative<exec_error>(subexpr)) {
      return subexpr;
    }
    auto val = std::get<exec_value>(subexpr);
    if (isa<number_value>(val)) {
      int64_t num = val.get<number_value>().value;
      return {number_value{static_cast<double>(~num)}};
    }
    return exec_error{"Not implemented"};
  }
  result accept(const typeof_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const void_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const delete_expr_node &) override {
    return exec_error{"Not implemented"};
  }

  result accept(const bin_op_expr_node &) override {
    return exec_error{"Encountered abstract class bin_op_expr_node"};
  }

    // arithmetic binops
#define GET_EVALED_OPERANDS()                                                  \
  auto lhs_or_err = visit(*node.lhs);                                          \
  if (std::holds_alternative<exec_error>(lhs_or_err)) {                        \
    return lhs_or_err;                                                         \
  }                                                                            \
  auto lhs = std::get<exec_value>(lhs_or_err);                                 \
  auto rhs_or_err = visit(*node.rhs);                                          \
  if (std::holds_alternative<exec_error>(rhs_or_err)) {                        \
    return rhs_or_err;                                                         \
  }                                                                            \
  auto rhs = std::get<exec_value>(rhs_or_err)

  result accept(const add_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return number_value(lhs.get<number_value>().value +
                          rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const subtract_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return number_value(lhs.get<number_value>().value -
                          rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const multiply_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return number_value(lhs.get<number_value>().value *
                          rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const divide_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return number_value(lhs.get<number_value>().value /
                          rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const pow_expr_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return number_value(std::pow(lhs.get<number_value>().value,
                                   rhs.get<number_value>().value));
    }
    return exec_error{"Not implemented"};
  }
  result accept(const modulo_expr_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      int64_t l = lhs.get<number_value>().value;
      int64_t r = rhs.get<number_value>().value;
      return number_value(static_cast<double>(l % r));
    }
    return exec_error{"Not implemented"};
  }
  // comparison binops
  result accept(const less_expr_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return bool_value(lhs.get<number_value>().value <
                        rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const less_eq_expr_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return bool_value(lhs.get<number_value>().value <=
                        rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const greater_expr_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return bool_value(lhs.get<number_value>().value >
                        rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const greater_eq_expr_node &node) override {
    GET_EVALED_OPERANDS();
    if (isa<number_value>(lhs) && isa<number_value>(rhs)) {
      return bool_value(lhs.get<number_value>().value >=
                        rhs.get<number_value>().value);
    }
    return exec_error{"Not implemented"};
  }
  result accept(const equals_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const strong_equals_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const not_equals_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const strong_not_equals_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const log_and_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const log_or_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  // bitwise binops
  result accept(const lshift_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const rshift_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const log_rshift_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const bitwise_and_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const bitwise_or_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const bitwise_xor_expr_node &) override {
    return exec_error{"Not implemented"};
  }
#undef GET_EVALED_OPERANDS
  // assignment binops
  result accept(const assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const add_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const subtract_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const multiply_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const divide_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const modulo_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const pow_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const lshift_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const rshift_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const log_rshift_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const and_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const or_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const xor_assign_node &) override {
    return exec_error{"Not implemented"};
  }
  // other binops
  result accept(const comma_operator_node &node) override {
    auto res = visit(*node.lhs);
    if (std::holds_alternative<exec_error>(res)) {
      return res;
    }
    return visit(*node.rhs);
  }
  result accept(const ternary_operator_node &) override {
    return exec_error{"Not implemented"};
  }
  // keyword binops
  result accept(const in_expr_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const instanceof_expr_node &) override {
    return exec_error{"Not implemented"};
  }

  result accept(const function_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const class_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const label_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const var_decl_part_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const empty_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const var_decl_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const if_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const do_while_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const while_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const for_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const for_in_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const for_of_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const switch_clause_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const case_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const switch_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const break_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const continue_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const return_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const throw_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const catch_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const try_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  // FIXME these are not yet correctly modeled
  result accept(const import_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const export_stmt_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const import_wildcard_node &) override {
    return exec_error{"Not implemented"};
  }
  result accept(const export_wildcard_node &) override {
    return exec_error{"Not implemented"};
  }
};

ast_executor::result ast_executor::execute(module_node &ast) {
  return exec_visitor(vm).visit(ast);
}