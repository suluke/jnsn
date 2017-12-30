#ifndef JNSN_JS_IR_CONSTRUCTION_INTERNAL_H
#define JNSN_JS_IR_CONSTRUCTION_INTERNAL_H
#include "jnsn/ir/ir_builder.h"
#include "jnsn/ir/module.h"
#include "jnsn/js/ast_walker.h"
#include "jnsn/js/ir_construction.h"
#include "jnsn/source_location.h"

#include <array>
#include <map>
#include <optional>
#include <string>

namespace jnsn {

struct ir_error {
  std::string msg;
  source_location loc;
};

struct ast_ir_mappings {
  std::map<const ast_node *, function *> funcs;
};

struct ast_to_ir {
  using result = ast_to_ir_result;
  std::unique_ptr<module> mod;
  ir_context ctx;
  ir_builder builder;
  ast_ir_mappings mappings;
  ast_to_ir(ir_context &ctx) : mod(new module(ctx)), ctx(ctx), builder(*mod) {}
  result build(const module_node &ast);
  std::optional<semantic_error> build_function_body(const ast_node &body,
                                                    basic_block &BB);
  std::optional<semantic_error> build_function_params(const ast_node &func,
                                                      basic_block &BB);
};

struct hoist_collector : public ast_walker<hoist_collector> {
  std::vector<const var_decl_node *> vars;
  std::vector<const function_stmt_node *> funcs;

  bool on_enter(const class_expr_node &node) override { return false; }
  bool on_enter(const class_stmt_node &node) override { return false; }
  bool on_enter(const function_expr_node &node) override { return false; }
  bool on_enter(const function_stmt_node &node) override {
    funcs.emplace_back(&node);
    return false;
  }
  bool on_enter(const arrow_function_node &node) override { return false; }
  bool on_enter(const class_func_node &node) override { return false; }
  bool on_enter(const var_decl_node &node) override {
    if (node.keyword == "var") {
      vars.emplace_back(&node);
    }
    return false;
  }
};

/// Collects all functions in post order
struct function_collector : public ast_walker<function_collector> {
  std::vector<const ast_node *> funcs;
  void on_leave(const function_expr_node &node) { funcs.emplace_back(&node); }
  void on_leave(const function_stmt_node &node) { funcs.emplace_back(&node); }
  void on_leave(const arrow_function_node &node) { funcs.emplace_back(&node); }
  void on_leave(const class_func_node &node) { funcs.emplace_back(&node); }
};

using inst_result = std::variant<ir_error, value *>;
struct inst_creator : public const_ast_node_visitor<inst_result> {
  using result = inst_result;
  ir_builder &builder;
  ast_ir_mappings &mappings;
  basic_block *IP;
  inst_creator(ir_builder &builder, ast_ir_mappings &mappings, basic_block *IP)
      : builder(builder), mappings(mappings), IP(IP) {}
  result accept(const statement_node &node) override {
    return ir_error{"Encountered abstract class statement_node", node.loc};
  }
  result accept(const module_node &node) override;
  result accept(const expression_node &node) override {
    return ir_error{"Encountered abstract class expression_node", node.loc};
  }
  result accept(const param_list_node &node) override {
    return ir_error{"param_list_nodes are codegen'ed separately", node.loc};
  }
  result accept(const block_node &node) override;
  result accept(const function_expr_node &) override;
  result accept(const class_func_node &) override;
  result accept(const class_expr_node &) override;
  result accept(const arrow_function_node &) override;
  result accept(const identifier_expr_node &node) override;

  result accept(const null_literal_node &node) override;
  result accept(const bool_literal_node &node) override {
    return ir_error{"Encountered abstract class bool_literal_node", {}};
  }
  result accept(const true_literal_node &node) override;
  result accept(const false_literal_node &node) override;
  result accept(const number_literal_node &) override {
    return ir_error{"Encountered abstract class number_literal_node", {}};
  }
  result accept(const int_literal_node &node) override;
  result accept(const float_literal_node &node) override;
  result accept(const hex_literal_node &node) override;
  result accept(const oct_literal_node &node) override;
  result accept(const bin_literal_node &node) override;
  result accept(const string_literal_node &node) override;
  result accept(const regex_literal_node &) override;
  result accept(const template_node &node) override {
    return ir_error{"Encountered abstract node type 'template_node'", node.loc};
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
    return ir_error{"Encountered abstract class unary_expr_node", node.loc};
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
  result accept(const bin_op_expr_node &) override {
    return ir_error{"Encountered abstract class bin_op_expr_node", {}};
  }
  template <class AstNodeTy>
  std::optional<ir_error> load_binop_args(const AstNodeTy &node, value **lhs,
                                          value **rhs);
  template <class AstNodeTy, class InstTy>
  std::variant<ir_error, InstTy *>
  arithmetic_binop_codegen(const AstNodeTy &node);
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
  // destructuring assignments
  result accept(const array_destruct_key_node &node) override;
  result accept(const array_destruct_keys_node &node) override;
  result accept(const array_destruct_node &node) override;
  result accept(const object_destruct_key_node &node) override {
    return ir_error{"Encountered abstract class object_destruct_key_node",
                    node.loc};
  }
  result accept(const object_destruct_bind_node &node) override;
  result accept(const object_destruct_nest_node &node) override;
  result accept(const object_destruct_keys_node &node) override;
  result accept(const object_destruct_node &node) override;
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
  result accept(const decl_array_destruct_node &node) override;
  result accept(const decl_object_destruct_node &node) override;
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
  // FIXME these are not yet correctly modeled
  result accept(const import_stmt_node &) override;
  result accept(const export_stmt_node &) override;
  result accept(const import_wildcard_node &) override;
  result accept(const export_wildcard_node &) override;
};
} // namespace jnsn
#endif // JNSN_JS_IR_CONSTRUCTION_INTERNAL_H
