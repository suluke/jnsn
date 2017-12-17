#ifndef JNSN_JS_AST_ANALYSIS_INTERNAL_H
#define JNSN_JS_AST_ANALYSIS_INTERNAL_H
#include "jnsn/js/ast_analysis.h"
#include "jnsn/js/ast.h"

namespace jnsn {
class ast_analysis_manager : public const_ast_node_visitor<bool> {
public:
  static ast_analysis_report analyze(ast_node &ast) {
    ast_analysis_manager am;
    am.visit(ast);
    return am.report;
  }
private:
  using result = bool;
  ast_analysis_report report;
  result accept(const statement_node &node) override {
    report.errors.emplace_back("Encountered abstract class statement_node", node.loc);
    return true;
  }
  result accept(const module_node &node) override;
  result accept(const expression_node &node) override {
    report.errors.emplace_back("Encountered abstract class expression_node", node.loc);
    return true;
  }
  result accept(const param_list_node &node) override;
  result accept(const block_node &node) override;
  result accept(const function_expr_node &) override;
  result accept(const class_func_node &) override;
  result accept(const class_expr_node &) override;
  result accept(const arrow_function_node &) override;
  result accept(const identifier_expr_node &node) override;

  result accept(const number_literal_node &node) override {
    report.errors.emplace_back("Encountered abstract class number_literal_node", node.loc);
    return true;
  }
  result accept(const int_literal_node &node) override;
  result accept(const float_literal_node &node) override;
  result accept(const hex_literal_node &node) override;
  result accept(const oct_literal_node &node) override;
  result accept(const bin_literal_node &node) override;
  result accept(const string_literal_node &node) override;
  result accept(const regex_literal_node &) override;
  result accept(const template_node &node) override {
    report.errors.emplace_back("Encountered abstract class template_node", node.loc);
    return true;
  }
  result accept(const template_string_node &node) override;
  result accept(const template_literal_node &node) override;
  result accept(const tagged_template_node &node) override;
  result accept(const array_literal_node &node) override;
  result accept(const object_entry_node &) override;
  result accept(const object_literal_node &) override;

  result accept(const member_access_node &) override;
  result accept(const computed_member_access_node &) override;
  result accept(const argument_list_node &) override;
  result accept(const call_expr_node &) override;
  result accept(const spread_expr_node &) override;
  result accept(const new_expr_node &) override;
  result accept(const new_target_node &) override;
  // Unary expressions
  result accept(const unary_expr_node &node) override {
    report.errors.emplace_back("Encountered abstract class unary_expr_node", node.loc);
    return true;
  }
  result accept(const postfix_increment_node &node) override;
  result accept(const postfix_decrement_node &) override;
  result accept(const prefix_increment_node &) override;
  result accept(const prefix_decrement_node &) override;
  result accept(const prefix_plus_node &) override;
  result accept(const prefix_minus_node &) override;
  result accept(const not_expr_node &node) override;
  result accept(const binverse_expr_node &node) override;
  result accept(const typeof_expr_node &) override;
  result accept(const void_expr_node &node) override;
  result accept(const delete_expr_node &) override;

  // arithmetic binops
  result accept(const bin_op_expr_node &node) override {
    report.errors.emplace_back("Encountered abstract class bin_op_expr_node", node.loc);
    return true;
  }
  result accept(const add_node &node) override;
  result accept(const subtract_node &node) override;
  result accept(const multiply_node &node) override;
  result accept(const divide_node &node) override;
  result accept(const pow_expr_node &node) override;
  result accept(const modulo_expr_node &node) override;
  // comparison binops
  result accept(const less_expr_node &node) override;
  result accept(const less_eq_expr_node &node) override;
  result accept(const greater_expr_node &node) override;
  result accept(const greater_eq_expr_node &node) override;
  result accept(const equals_expr_node &) override;
  result accept(const strong_equals_expr_node &) override;
  result accept(const not_equals_expr_node &) override;
  result accept(const strong_not_equals_expr_node &) override;
  result accept(const log_and_expr_node &) override;
  result accept(const log_or_expr_node &) override;
  // bitwise binops
  result accept(const lshift_expr_node &) override;
  result accept(const rshift_expr_node &) override;
  result accept(const log_rshift_expr_node &) override;
  result accept(const bitwise_and_expr_node &) override;
  result accept(const bitwise_or_expr_node &) override;
  result accept(const bitwise_xor_expr_node &) override;
  // assignment binops
  result accept(const assign_node &) override;
  result accept(const add_assign_node &) override;
  result accept(const subtract_assign_node &) override;
  result accept(const multiply_assign_node &) override;
  result accept(const divide_assign_node &) override;
  result accept(const modulo_assign_node &) override;
  result accept(const pow_assign_node &) override;
  result accept(const lshift_assign_node &) override;
  result accept(const rshift_assign_node &) override;
  result accept(const log_rshift_assign_node &) override;
  result accept(const and_assign_node &) override;
  result accept(const or_assign_node &) override;
  result accept(const xor_assign_node &) override;
  // other binops
  result accept(const comma_operator_node &node) override;
  result accept(const ternary_operator_node &) override;
  // keyword binops
  result accept(const in_expr_node &) override;
  result accept(const instanceof_expr_node &) override;

  result accept(const function_stmt_node &) override;
  result accept(const class_stmt_node &) override;
  result accept(const label_stmt_node &) override;
  result accept(const var_decl_part_node &) override;
  result accept(const empty_stmt_node &) override;
  result accept(const var_decl_node &) override;
  result accept(const if_stmt_node &) override;
  result accept(const do_while_node &) override;
  result accept(const while_stmt_node &) override;
  result accept(const for_stmt_node &) override;
  result accept(const for_in_node &) override;
  result accept(const for_of_node &) override;
  result accept(const switch_clause_node &) override;
  result accept(const case_node &) override;
  result accept(const switch_stmt_node &) override;
  result accept(const break_stmt_node &) override;
  result accept(const continue_stmt_node &) override;
  result accept(const return_stmt_node &) override;
  result accept(const throw_stmt_node &) override;
  result accept(const catch_node &) override;
  result accept(const try_stmt_node &) override;

  result accept(const import_stmt_node &) override;
  result accept(const export_stmt_node &) override;
  result accept(const import_wildcard_node &) override;
  result accept(const export_wildcard_node &) override;
};
struct children_not_null {
#define NODE(NAME, CHECK_CHILDREN) \
  static bool check(const NAME##_node &node, ast_analysis_report &report);
#define DERIVED(NAME, ANCESTORS, CHECK_CHILDREN) NODE(NAME, CHECK_CHILDREN)
#include "jnsn/js/ast.def"
};
} // namespace jnsn
#endif // JNSN_JS_AST_ANALYSIS_INTERNAL_H
