#ifndef JNSN_IR_BUILDER_INTERNAL_H
#define JNSN_IR_BUILDER_INTERNAL_H
#include "jnsn/ast_walker.h"
#include "jnsn/ir/ir_builder.h"
#include "jnsn/ir/module.h"
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

struct ir_builder {
  using result = std::variant<semantic_error, std::unique_ptr<module>>;
  ir_context &ctx;
  ast_ir_mappings mappings;
  module *mod;
  ir_builder(ir_context &ctx) : ctx(ctx) {}
  result build(const module_node &ast);
  c_str_val *get_c_str_val(std::string str);
  basic_block *make_block(function &F);
  call_inst *cast_to_number(basic_block &IP, value &val);
  call_inst *cast_to_primitive(basic_block &IP, value &val);
  call_inst *cast_to_string(basic_block &IP, value &val);
  call_inst *test_is_string(basic_block &IP, value &val);
  call_inst *concat_strings(basic_block &IP, value &lhs, value &rhs);
  function *get_intrinsic(intrinsic i) { return ctx.get_intrinsic(i); }

  template <class ty> ty *insert_inst(basic_block &bb) {
    assert(mod);
    auto *inst = ctx.make_inst<ty>();
    ctx.insert_inst_into(bb, *inst);
    return inst;
  }
  template <class Inst>
  void set_inst_arg(Inst &inst, typename Inst::arguments arg, value &val) {
    ctx.set_inst_arg(inst, arg, val);
  }
  template <class Iterator>
  instruction *prepare_call_args(basic_block &IP, Iterator begin,
                                 Iterator end) {
    auto *args = insert_inst<alloc_object_inst>(IP);
    unsigned idx = 0;
    for (auto It = begin; It != end; ++It) {
      auto *idx_val = get_c_str_val(std::to_string(idx));
      auto *addr = insert_inst<def_prop_inst>(IP);
      set_inst_arg(*addr, def_prop_inst::arguments::address, *args);
      set_inst_arg(*addr, def_prop_inst::arguments::prop, *idx_val);
      auto *store = insert_inst<store_inst>(IP);
      set_inst_arg(*store, store_inst::arguments::address, *addr);
      set_inst_arg(*store, store_inst::arguments::value, **It);
    }
    return args;
  }
};

struct hoisted_codegen : public ast_walker<hoisted_codegen> {
  ir_builder &builder;
  basic_block &bb;
  hoisted_codegen(ir_builder &builder, basic_block &bb)
      : builder(builder), bb(bb) {}
  bool on_enter(const function_expr_node &node) { return false; }
  bool on_enter(const function_stmt_node &node) {
    // TODO
    return false;
  }
  bool on_enter(const arrow_function_node &node) { return false; }
  bool on_enter(const class_func_node &node) { return false; }
  bool on_enter(const var_decl_node &node) override;
};

using inst_result = std::variant<ir_error, value *>;
struct inst_creator : public const_ast_node_visitor<inst_result> {
  using result = inst_result;
  ir_builder &builder;
  basic_block *IP;
  inst_creator(ir_builder &builder, basic_block *IP)
      : builder(builder), IP(IP) {}
  result accept(const statement_node &) override {
    return ir_error{"Encountered abstract class statement_node", {}};
  }
  result accept(const module_node &node) override;
  result accept(const expression_node &) override {
    return ir_error{"Encountered abstract class expression_node", {}};
  }
  result accept(const param_list_node &) override;
  result accept(const block_node &node) override;
  result accept(const function_expr_node &) override;
  result accept(const class_func_node &) override;
  result accept(const class_expr_node &) override;
  result accept(const arrow_function_node &) override;
  result accept(const identifier_expr_node &node) override;

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
  result accept(const template_string_node &node) override;
  result accept(const template_literal_node &node) override;
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
  result accept(const unary_expr_node &) override {
    return ir_error{"Encountered abstract class unary_expr_node", {}};
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
  // FIXME these are not yet correctly modeled
  result accept(const import_stmt_node &) override;
  result accept(const export_stmt_node &) override;
  result accept(const import_wildcard_node &) override;
  result accept(const export_wildcard_node &) override;
};

/// Helper to get the address value corresponding to an ast_node.
// Makes use of inst_creator to produce code if necessary
class load_adress_gen : public ast_walker<load_adress_gen> {
  inst_creator &inster;
  value *result = nullptr;
  load_adress_gen(inst_creator &inster) : inster(inster) {}
  bool on_enter(const number_literal_node &node) override;
  bool on_enter(const int_literal_node &node) override;
  bool on_enter(const float_literal_node &node) override;
  bool on_enter(const hex_literal_node &node) override;
  bool on_enter(const oct_literal_node &node) override;
  bool on_enter(const bin_literal_node &node) override;
  bool on_enter(const string_literal_node &node) override;
  bool on_enter(const regex_literal_node &node) override;
  bool on_enter(const template_string_node &node) override;

public:
  static value &get_address(ast_node &node, inst_creator &inster) {
    load_adress_gen addrgen(inster);
    addrgen.visit(node);
    assert(addrgen.result);
    return *addrgen.result;
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

} // namespace jnsn
#endif // JNSN_IR_BUILDER_INTERNAL_H
