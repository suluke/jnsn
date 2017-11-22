#include "parsing/ast_ops.h"
#include "parsing/ast_walker.h"
#include "parsing/ir/ir_builder.h"
#include "parsing/ir/module.h"
#include "parsing/util.h"
#include <map>

namespace parsing {

struct ir_error {
  std::string msg;
  source_location loc;
};

using inst_result = std::variant<ir_error, value *>;

struct inst_creator : public const_ast_node_visitor<inst_result> {
  using result = inst_result;

  result accept(const statement_node &) override {
    return ir_error{"Encountered abstract class statement_node", {}};
  }
  result accept(const module_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const expression_node &) override {
    return ir_error{"Encountered abstract class expression_node", {}};
  }
  result accept(const param_list_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const block_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const function_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const class_func_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const class_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const arrow_function_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const identifier_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }

  result accept(const number_literal_node &) override {
    return ir_error{"Encountered abstract class number_literal", {}};
  }
  result accept(const int_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const float_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const hex_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const oct_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const bin_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const string_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const regex_literal_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const template_string_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const template_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const array_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const object_entry_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const object_literal_node &) override {
    return ir_error{"Not implemented", {}};
  }

  result accept(const member_access_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const computed_member_access_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const argument_list_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const call_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const spread_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const new_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const new_target_node &) override {
    return ir_error{"Not implemented", {}};
  }
  // Unary expressions
  result accept(const unary_expr_node &) override {
    return ir_error{"Encountered abstract class unary_expr_node", {}};
  }
  result accept(const postfix_increment_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const postfix_decrement_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const prefix_increment_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const prefix_decrement_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const prefix_plus_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const prefix_minus_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const not_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const binverse_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const typeof_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const void_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const delete_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const bin_op_expr_node &) override {
    return ir_error{"Encountered abstract class bin_op_expr_node", {}};
  }
  // arithmetic binops
  result accept(const add_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const subtract_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const multiply_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const divide_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const pow_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const modulo_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  // comparison binops
  result accept(const less_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const less_eq_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const greater_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const greater_eq_expr_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const equals_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const strong_equals_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const not_equals_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const strong_not_equals_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const log_and_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const log_or_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  // bitwise binops
  result accept(const lshift_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const rshift_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const log_rshift_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const bitwise_and_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const bitwise_or_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const bitwise_xor_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  // assignment binops
  result accept(const assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const add_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const subtract_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const multiply_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const divide_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const modulo_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const pow_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const lshift_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const rshift_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const log_rshift_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const and_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const or_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const xor_assign_node &) override {
    return ir_error{"Not implemented", {}};
  }
  // other binops
  result accept(const comma_operator_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const ternary_operator_node &) override {
    return ir_error{"Not implemented", {}};
  }
  // keyword binops
  result accept(const in_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const instanceof_expr_node &) override {
    return ir_error{"Not implemented", {}};
  }

  result accept(const function_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const class_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const label_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const var_decl_part_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const empty_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const var_decl_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const if_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const do_while_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const while_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const for_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const for_in_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const for_of_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const switch_clause_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const case_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const switch_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const break_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const continue_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const return_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const throw_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const catch_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const try_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  // FIXME these are not yet correctly modeled
  result accept(const import_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const export_stmt_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const import_wildcard_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const export_wildcard_node &) override {
    return ir_error{"Not implemented", {}};
  }
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
  template <class ty> ty &insert_inst(basic_block &bb) {
    assert(mod);
    auto *inst = ctx.make_inst<ty>();
    ctx.insert_inst_into(&bb, inst);
    return *inst;
  }
  template <class Inst>
  void set_inst_arg(Inst &inst, typename Inst::arguments arg, value &val) {
    ctx.set_inst_arg(inst, arg, val);
  }
  std::string get_unique_id(value &v) {
    return ctx.get_unique_id(v);
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

static std::string to_str(const string_table_entry &s) {
  return std::string{s.data(), s.size()};
}

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
  bool on_enter(const var_decl_node &node) {
    if (node.keyword == "var") {
      for (auto *part : node.parts) {
        auto &define = builder.insert_inst<define_inst>(bb);
        auto &name = builder.ctx.get_c_str_val(to_str(part->name));
        builder.set_inst_arg(define, define_inst::arguments::name, name);
      }
    }
    return false;
  }
};

static const statement_node *get_function_body(const ast_node *node) {
  if (isa<function_expr_node>(node)) {
    return static_cast<const function_expr_node *>(node)->body;
  } else if (isa<function_stmt_node>(node)) {
    return static_cast<const function_stmt_node *>(node)->body;
  } else if (isa<arrow_function_node>(node)) {
    return static_cast<const arrow_function_node *>(node)->body;
  } else if (isa<class_func_node>(node)) {
    return static_cast<const class_func_node *>(node)->body;
  }
  unreachable("Given ast node is not a function");
}

ir_builder::result ir_builder::build(const module_node &ast) {
  auto mod = std::unique_ptr<module>(new module(ctx));
  this->mod = mod.get();
  ast_ir_mappings mappings;
  auto *entry = ctx.make_function();
  ctx.insert_function_into(mod.get(), entry);
  ctx.insert_block_into(entry, ctx.make_block());
  entry->set_name("__module_entry__");
  hoisted_codegen(*this, *entry->get_entry()).visit(ast);
  function_collector fcollect;
  fcollect.visit(ast);
  for (const auto *ast_func : fcollect.funcs) {
    auto *F = ctx.make_function();
    if (isa<function_stmt_node>(ast_func)) {
      const auto as_stmt = static_cast<const function_stmt_node *>(ast_func);
      F->set_name(to_str(as_stmt->name));
    }
    ctx.insert_block_into(F, ctx.make_block());
    mappings.funcs.emplace(ast_func, F);
    const auto *body = get_function_body(ast_func);
    hoisted_codegen(*this, *F->get_entry()).visit(*body);
    ctx.insert_function_into(mod.get(), F);
  }
  return std::move(mod);
}

ir_builder::result ast_to_ir(const module_node &ast, ir_context &ctx) {
  return ir_builder(ctx).build(ast);
}
} // namespace parsing