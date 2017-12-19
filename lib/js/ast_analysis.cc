#include "ast_analysis_internal.h"

namespace jnsn {
ast_analysis_report analyze_js_ast(ast_node &ast) {
  return ast_analysis_manager::analyze(ast);
}
std::ostream &operator<<(std::ostream &stream, const ast_error &error) {
  stream << error.msg;
  return stream;
}
std::ostream &operator<<(std::ostream &stream,
                         const ast_analysis_report &report) {
  for (auto &err : report.errors) {
    stream << err << "\n";
  }
  return stream;
}
} // namespace jnsn

// ================== Implementation of check_not_null =================
using namespace jnsn;
#define ONE(OF, NAME)                                                          \
  if (!node.NAME) {                                                            \
    has_errors = true;                                                         \
    report.errors.emplace_back("", node.loc);                                  \
  }
#define MANY(OF, NAME)                                                         \
  for (const auto *child : node.NAME)                                          \
    if (!child) {                                                              \
      has_errors = true;                                                       \
      report.errors.emplace_back("", node.loc);                                \
    }
#define CHILDREN(...)                                                          \
  do {                                                                         \
    __VA_ARGS__                                                                \
  } while (false)
#define EXTENDS(NAME) NAME##_node
#define NODE(NAME, CHECK_CHILDREN)                                             \
  bool children_not_null::check(const NAME##_node &node,                       \
                                ast_analysis_report &report) {                 \
    bool has_errors = false;                                                   \
    CHECK_CHILDREN;                                                            \
    return has_errors;                                                         \
  }
#define DERIVED(NAME, ANCESTOR, CHECK_CHILDREN)                                \
  bool children_not_null::check(const NAME##_node &node,                       \
                                ast_analysis_report &report) {                 \
    bool has_errors = false;                                                   \
    has_errors |=                                                              \
        children_not_null::check(static_cast<const ANCESTOR &>(node), report); \
    CHECK_CHILDREN;                                                            \
    return has_errors;                                                         \
  }
#include "jnsn/js/ast.def"
// ===================== End of check_not_null impl ====================

ast_analysis_manager::result
ast_analysis_manager::accept(const module_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const param_list_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const block_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const function_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const class_func_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const class_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const arrow_function_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const identifier_expr_node &node) {
  return children_not_null::check(node, report);
}

ast_analysis_manager::result
ast_analysis_manager::accept(const int_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const float_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const hex_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const oct_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const bin_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const string_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const regex_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const template_string_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const template_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const tagged_template_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const array_literal_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const object_entry_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const object_literal_node &node) {
  return children_not_null::check(node, report);
}

ast_analysis_manager::result
ast_analysis_manager::accept(const member_access_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const computed_member_access_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const argument_list_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const call_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const spread_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const new_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const new_target_node &node) {
  return children_not_null::check(node, report);
}

// Unary expressions
ast_analysis_manager::result
ast_analysis_manager::accept(const postfix_increment_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const postfix_decrement_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const prefix_increment_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const prefix_decrement_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const prefix_plus_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const prefix_minus_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const not_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const binverse_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const typeof_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const void_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const delete_expr_node &node) {
  return children_not_null::check(node, report);
}

// arithmetic binops
ast_analysis_manager::result
ast_analysis_manager::accept(const add_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const subtract_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const multiply_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const divide_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const pow_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const modulo_expr_node &node) {
  return children_not_null::check(node, report);
}
// comparison binops
ast_analysis_manager::result
ast_analysis_manager::accept(const less_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const less_eq_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const greater_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const greater_eq_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const equals_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const strong_equals_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const not_equals_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const strong_not_equals_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const log_and_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const log_or_expr_node &node) {
  return children_not_null::check(node, report);
}
// bitwise binops
ast_analysis_manager::result
ast_analysis_manager::accept(const lshift_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const rshift_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const log_rshift_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const bitwise_and_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const bitwise_or_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const bitwise_xor_expr_node &node) {
  return children_not_null::check(node, report);
}
// assignment binops
ast_analysis_manager::result
ast_analysis_manager::accept(const assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const add_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const subtract_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const multiply_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const divide_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const modulo_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const pow_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const lshift_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const rshift_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const log_rshift_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const and_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const or_assign_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const xor_assign_node &node) {
  return children_not_null::check(node, report);
}
// destructuring assignments
ast_analysis_manager::result ast_analysis_manager::accept(const array_destruct_key_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result ast_analysis_manager::accept(const array_destruct_keys_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result ast_analysis_manager::accept(const array_destruct_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result ast_analysis_manager::accept(const object_destruct_key_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result ast_analysis_manager::accept(const object_destruct_keys_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result ast_analysis_manager::accept(const object_destruct_node &node) {
  return children_not_null::check(node, report);
}
// other binops
ast_analysis_manager::result
ast_analysis_manager::accept(const comma_operator_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const ternary_operator_node &node) {
  return children_not_null::check(node, report);
}
// keyword binops
ast_analysis_manager::result
ast_analysis_manager::accept(const in_expr_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const instanceof_expr_node &node) {
  return children_not_null::check(node, report);
}

ast_analysis_manager::result
ast_analysis_manager::accept(const function_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const class_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const label_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const var_decl_part_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const empty_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const var_decl_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result ast_analysis_manager::accept(const decl_array_destruct_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result ast_analysis_manager::accept(const decl_object_destruct_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const if_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const do_while_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const while_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const for_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const for_in_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const for_of_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const switch_clause_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const case_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const switch_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const break_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const continue_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const return_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const throw_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const catch_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const try_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const import_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const export_stmt_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const import_wildcard_node &node) {
  return children_not_null::check(node, report);
}
ast_analysis_manager::result
ast_analysis_manager::accept(const export_wildcard_node &node) {
  return children_not_null::check(node, report);
}
