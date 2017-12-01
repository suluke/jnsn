#include "jnsn/ast_ops.h"
#include "jnsn/ast_walker.h"
#include "jnsn/ir/intrinsics.h"
#include "jnsn/ir/ir_builder.h"
#include "jnsn/ir/module.h"
#include "jnsn/util.h"
#include <array>
#include <cstdlib>
#include <map>
#include <string>

namespace jnsn {

struct ir_error {
  std::string msg;
  source_location loc;
};

struct ast_ir_mappings {
  std::map<const ast_node *, function *> funcs;
};

static std::string to_str(const string_table_entry &s) {
  return std::string{s.data(), s.size()};
}

struct ir_builder {
  using result = std::variant<semantic_error, std::unique_ptr<module>>;
  ir_context &ctx;
  ast_ir_mappings mappings;
  module *mod;
  ir_builder(ir_context &ctx) : ctx(ctx) {}
  result build(const module_node &ast);
  c_str_val *get_c_str_val(std::string str) {
    auto *val = ctx.get_c_str_val(std::move(str));
    mod->add_string_constant(*val);
    return val;
  }
  basic_block *make_block(function &F) {
    auto *bb = ctx.make_block();
    ctx.insert_block_into(F, *bb);
    return bb;
  }
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
  call_inst *cast_to_number(basic_block &IP, value &val) {
    std::array<value *, 1> args = {&val};
    auto *to_num = get_intrinsic(intrinsic::to_number);
    auto *args_list = prepare_call_args(IP, args.begin(), args.end());
    auto *num = insert_inst<call_inst>(IP);
    set_inst_arg(*num, call_inst::arguments::callee, *to_num);
    set_inst_arg(*num, call_inst::arguments::this_val, *ctx.get_undefined());
    set_inst_arg(*num, call_inst::arguments::arguments, *args_list);
    return num;
  }
  call_inst *cast_to_primitive(basic_block &IP, value &val) {
    std::array<value *, 1> args = {&val};
    auto *to_prim = get_intrinsic(intrinsic::to_primitive);
    auto *args_list = prepare_call_args(IP, args.begin(), args.end());
    auto *prim = insert_inst<call_inst>(IP);
    set_inst_arg(*prim, call_inst::arguments::callee, *to_prim);
    set_inst_arg(*prim, call_inst::arguments::this_val, *ctx.get_undefined());
    set_inst_arg(*prim, call_inst::arguments::arguments, *args_list);
    return prim;
  }
  call_inst *cast_to_string(basic_block &IP, value &val) {
    std::array<value *, 1> args = {&val};
    auto *to_str = get_intrinsic(intrinsic::to_string);
    auto *args_list = prepare_call_args(IP, args.begin(), args.end());
    auto *str = insert_inst<call_inst>(IP);
    set_inst_arg(*str, call_inst::arguments::callee, *to_str);
    set_inst_arg(*str, call_inst::arguments::this_val, *ctx.get_undefined());
    set_inst_arg(*str, call_inst::arguments::arguments, *args_list);
    return str;
  }
  call_inst *test_is_string(basic_block &IP, value &val) {
    std::array<value *, 1> args = {&val};
    auto *is_str = get_intrinsic(intrinsic::is_string);
    auto *args_list = prepare_call_args(IP, args.begin(), args.end());
    auto *res = insert_inst<call_inst>(IP);
    set_inst_arg(*res, call_inst::arguments::callee, *is_str);
    set_inst_arg(*res, call_inst::arguments::this_val, *ctx.get_undefined());
    set_inst_arg(*res, call_inst::arguments::arguments, *args_list);
    return res;
  }
  call_inst *concat_strings(basic_block &IP, value &lhs, value &rhs) {
    std::array<value *, 2> args = {&lhs, &rhs};
    auto *concat = get_intrinsic(intrinsic::concat);
    auto *args_list = prepare_call_args(IP, args.begin(), args.end());
    auto *res = insert_inst<call_inst>(IP);
    set_inst_arg(*res, call_inst::arguments::callee, *concat);
    set_inst_arg(*res, call_inst::arguments::this_val, *ctx.get_undefined());
    set_inst_arg(*res, call_inst::arguments::arguments, *args_list);
    return res;
  }
  function *get_intrinsic(intrinsic i) { return ctx.get_intrinsic(i); }
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
  result accept(const module_node &node) override {
    for (auto *stmt : node.stmts) {
      auto res = visit(*stmt);
      if (std::holds_alternative<ir_error>(res)) {
        return std::get<ir_error>(res);
      }
    }
    return nullptr;
  }
  result accept(const expression_node &) override {
    return ir_error{"Encountered abstract class expression_node", {}};
  }
  result accept(const param_list_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const block_node &node) override {
    for (auto *child : node.stmts) {
      auto res = visit(*child);
      if (std::holds_alternative<ir_error>(res)) {
        return std::get<ir_error>(res);
      }
    }
    return nullptr;
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
    return builder.ctx.get_c_num_val(
        static_cast<double>(std::strtol(node.val.data(), nullptr, 10)));
  }
  result accept(const float_literal_node &node) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const hex_literal_node &node) override {
    return builder.ctx.get_c_num_val(
        static_cast<double>(std::strtol(node.val.data() + 2, nullptr, 16)));
  }
  result accept(const oct_literal_node &node) override {
    return builder.ctx.get_c_num_val(
        static_cast<double>(std::strtol(node.val.data() + 2, nullptr, 8)));
  }
  result accept(const bin_literal_node &node) override {
    return builder.ctx.get_c_num_val(
        static_cast<double>(std::strtol(node.val.data() + 2, nullptr, 2)));
  }
  result accept(const string_literal_node &node) override {
    return builder.get_c_str_val(
        std::string{node.val.data() + 1, node.val.size() - 2});
  }
  result accept(const regex_literal_node &) override {
    return ir_error{"Not implemented", {}};
  }
  result accept(const template_string_node &node) override {
    return builder.get_c_str_val(
        std::string{node.val.data() + 1, node.val.size() - 2});
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
#define LOAD_BINOP_ARGS(lhs, rhs)                                              \
  do {                                                                         \
    auto lhs_err = visit(*node.lhs);                                           \
    if (std::holds_alternative<ir_error>(lhs_err)) {                           \
      return lhs;                                                              \
    }                                                                          \
    auto rhs_err = visit(*node.rhs);                                           \
    if (std::holds_alternative<ir_error>(rhs_err)) {                           \
      return rhs;                                                              \
    }                                                                          \
    lhs = std::get<value *>(lhs_err);                                          \
    rhs = std::get<value *>(rhs_err);                                          \
  } while (false)

  result accept(const add_node &node) override {
    // See dmitripavlutin.com/javascriptss-addition-operator-demystified/
    assert(IP);
    auto &start = *IP;
    assert(start.get_parent());
    auto &F = *IP->get_parent();

    value *lhs, *rhs;
    LOAD_BINOP_ARGS(lhs, rhs);

    // convert lhs to primitive
    call_inst *lhs_prim = builder.cast_to_primitive(start, *lhs);
    call_inst *rhs_prim = builder.cast_to_primitive(start, *rhs);
    // see if lhs primitive is string
    call_inst *lhs_is_str = builder.test_is_string(start, *lhs_prim);
    call_inst *rhs_is_str = builder.test_is_string(start, *rhs_prim);

    // define some branch targets for later use
    auto *on_concat = builder.make_block(F);
    on_concat->set_name("concat");
    auto *on_add = builder.make_block(F);
    on_add->set_name("add");
    auto *val_unification = builder.make_block(F);
    val_unification->set_name("plus.phi");

    // dispatch either to concatenation or addition
    {
      // test if either one is string
      auto *lhs_or_rhs_str = builder.insert_inst<log_or_inst>(*IP);
      builder.set_inst_arg(*lhs_or_rhs_str, log_or_inst::arguments::lhs,
                           *lhs_is_str);
      builder.set_inst_arg(*lhs_or_rhs_str, log_or_inst::arguments::rhs,
                           *rhs_is_str);

      auto *br = builder.insert_inst<cbr_inst>(*IP);
      builder.set_inst_arg(*br, cbr_inst::arguments::cond, *lhs_or_rhs_str);
      builder.set_inst_arg(*br, cbr_inst::arguments::true_target, *on_concat);
      builder.set_inst_arg(*br, cbr_inst::arguments::false_target, *on_add);
    }

    // codegen for concat
    call_inst *concat = nullptr;
    {
      auto *lhs_str = builder.cast_to_string(*on_concat, *lhs_prim);
      auto *rhs_str = builder.cast_to_string(*on_concat, *rhs_prim);

      concat = builder.concat_strings(*on_concat, *lhs_str, *rhs_str);

      auto *concat_br = builder.insert_inst<br_inst>(*on_concat);
      builder.set_inst_arg(*concat_br, br_inst::arguments::target,
                           *val_unification);
    }

    // codegen for addition
    add_inst *add = nullptr;
    {
      auto *lhs_num = builder.cast_to_number(*on_add, *lhs);
      auto *rhs_num = builder.cast_to_number(*on_add, *rhs);

      add = builder.insert_inst<add_inst>(*on_add);
      builder.set_inst_arg(*add, add_inst::arguments::lhs, *lhs_num);
      builder.set_inst_arg(*add, add_inst::arguments::rhs, *rhs_num);

      auto *add_br = builder.insert_inst<br_inst>(*on_add);
      builder.set_inst_arg(*add_br, br_inst::arguments::target,
                           *val_unification);
    }

    // now unify results
    auto *val = builder.insert_inst<phi_inst>(*val_unification);
    builder.set_inst_arg(*val, phi_inst::arguments::in1, *on_concat);
    builder.set_inst_arg(*val, phi_inst::arguments::val1, *concat);
    builder.set_inst_arg(*val, phi_inst::arguments::in2, *on_add);
    builder.set_inst_arg(*val, phi_inst::arguments::val2, *add);

    IP = val_unification;
    return val;
  }
  result accept(const subtract_node &node) override {
    value *lhs, *rhs;
    LOAD_BINOP_ARGS(lhs, rhs);
    std::array<value *, 1> args;
    auto *to_num = builder.get_intrinsic(intrinsic::to_number);
    // turn lhs into a number
    args[0] = lhs;
    auto *lhs_args = builder.prepare_call_args(*IP, args.begin(), args.end());
    auto *lhs_num = builder.insert_inst<call_inst>(*IP);
    builder.set_inst_arg(*lhs_num, call_inst::arguments::callee, *to_num);
    builder.set_inst_arg(*lhs_num, call_inst::arguments::this_val,
                         *builder.ctx.get_undefined());
    builder.set_inst_arg(*lhs_num, call_inst::arguments::arguments, *lhs_args);
    // turn rhs into a number
    args[0] = rhs;
    auto *rhs_args = builder.prepare_call_args(*IP, args.begin(), args.end());
    auto *rhs_num = builder.insert_inst<call_inst>(*IP);
    builder.set_inst_arg(*rhs_num, call_inst::arguments::callee, *to_num);
    builder.set_inst_arg(*rhs_num, call_inst::arguments::this_val,
                         *builder.ctx.get_undefined());
    builder.set_inst_arg(*rhs_num, call_inst::arguments::arguments, *rhs_args);

    //
    auto *res = builder.insert_inst<sub_inst>(*IP);
    builder.set_inst_arg(*res, sub_inst::arguments::lhs, *lhs_num);
    builder.set_inst_arg(*res, sub_inst::arguments::rhs, *rhs_num);
    return res;
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

/// Collects all functions in post order
struct function_collector : public ast_walker<function_collector> {
  std::vector<const ast_node *> funcs;
  void on_leave(const function_expr_node &node) { funcs.emplace_back(&node); }
  void on_leave(const function_stmt_node &node) { funcs.emplace_back(&node); }
  void on_leave(const arrow_function_node &node) { funcs.emplace_back(&node); }
  void on_leave(const class_func_node &node) { funcs.emplace_back(&node); }
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
  bool on_enter(const var_decl_node &node) {
    if (node.keyword == "var") {
      for (auto *part : node.parts) {
        auto *define = builder.insert_inst<define_inst>(bb);
        auto *name = builder.get_c_str_val(to_str(part->name));
        builder.set_inst_arg(*define, define_inst::arguments::name, *name);
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
  ctx.insert_function_into(*mod, *entry);
  ctx.insert_block_into(*entry, *ctx.make_block());
  entry->set_name("__module_entry__");
  hoisted_codegen(*this, *entry->get_entry()).visit(ast);
  auto res = inst_creator(*this, entry->get_entry()).visit(ast);
  if (std::holds_alternative<ir_error>(res)) {
    auto &err = std::get<ir_error>(res);
    return semantic_error(err.msg, err.loc);
  }

  function_collector fcollect;
  fcollect.visit(ast);
  for (const auto *ast_func : fcollect.funcs) {
    auto *F = ctx.make_function();
    if (isa<function_stmt_node>(ast_func)) {
      const auto as_stmt = static_cast<const function_stmt_node *>(ast_func);
      F->set_name(to_str(as_stmt->name));
    }
    ctx.insert_block_into(*F, *ctx.make_block());
    mappings.funcs.emplace(ast_func, F);
    const auto *body = get_function_body(ast_func);
    hoisted_codegen(*this, *F->get_entry()).visit(*body);
    ctx.insert_function_into(*mod, *F);
    auto res = inst_creator(*this, F->get_entry()).visit(*body);
    if (std::holds_alternative<ir_error>(res)) {
      auto &err = std::get<ir_error>(res);
      return semantic_error(err.msg, err.loc);
    }
  }
  return std::move(mod);
}

ir_builder::result ast_to_ir(const module_node &ast, ir_context &ctx) {
  return ir_builder(ctx).build(ast);
}
} // namespace jnsn
