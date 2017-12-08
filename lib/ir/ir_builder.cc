#include "ir_builder_internal.h"
#include "jnsn/ast_ops.h" // isa<> on ast nodes
#include "jnsn/util.h"    // unreachable
#include <cstdlib>        // number parsing

namespace jnsn {

static std::string to_str(const string_table_entry &s) {
  return std::string{s.data(), s.size()};
}

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

// ============ ir_builder impl ============
c_str_val *ir_builder::get_c_str_val(std::string str) {
  auto *val = ctx.get_c_str_val(std::move(str));
  mod->add_string_constant(*val);
  return val;
}
basic_block *ir_builder::make_block(function &F) {
  auto *bb = ctx.make_block();
  ctx.insert_block_into(F, *bb);
  return bb;
}
call_inst *ir_builder::cast_to_number(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *to_num = get_intrinsic(intrinsic::to_number);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *num = insert_inst<call_inst>(IP);
  set_inst_arg(*num, call_inst::arguments::callee, *to_num);
  set_inst_arg(*num, call_inst::arguments::this_val, *ctx.get_undefined());
  set_inst_arg(*num, call_inst::arguments::arguments, *args_list);
  return num;
}
call_inst *ir_builder::cast_to_primitive(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *to_prim = get_intrinsic(intrinsic::to_primitive);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *prim = insert_inst<call_inst>(IP);
  set_inst_arg(*prim, call_inst::arguments::callee, *to_prim);
  set_inst_arg(*prim, call_inst::arguments::this_val, *ctx.get_undefined());
  set_inst_arg(*prim, call_inst::arguments::arguments, *args_list);
  return prim;
}
call_inst *ir_builder::cast_to_string(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *to_str = get_intrinsic(intrinsic::to_string);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *str = insert_inst<call_inst>(IP);
  set_inst_arg(*str, call_inst::arguments::callee, *to_str);
  set_inst_arg(*str, call_inst::arguments::this_val, *ctx.get_undefined());
  set_inst_arg(*str, call_inst::arguments::arguments, *args_list);
  return str;
}
call_inst *ir_builder::test_is_string(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *is_str = get_intrinsic(intrinsic::is_string);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *res = insert_inst<call_inst>(IP);
  set_inst_arg(*res, call_inst::arguments::callee, *is_str);
  set_inst_arg(*res, call_inst::arguments::this_val, *ctx.get_undefined());
  set_inst_arg(*res, call_inst::arguments::arguments, *args_list);
  return res;
}
call_inst *ir_builder::concat_strings(basic_block &IP, value &lhs, value &rhs) {
  std::array<value *, 2> args = {&lhs, &rhs};
  auto *concat = get_intrinsic(intrinsic::concat);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *res = insert_inst<call_inst>(IP);
  set_inst_arg(*res, call_inst::arguments::callee, *concat);
  set_inst_arg(*res, call_inst::arguments::this_val, *ctx.get_undefined());
  set_inst_arg(*res, call_inst::arguments::arguments, *args_list);
  return res;
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
// =========================================

bool load_adress_gen::on_enter(const number_literal_node &node) {
  return false;
}
bool load_adress_gen::on_enter(const int_literal_node &node) { return false; }
bool load_adress_gen::on_enter(const float_literal_node &node) { return false; }
bool load_adress_gen::on_enter(const hex_literal_node &node) { return false; }
bool load_adress_gen::on_enter(const oct_literal_node &node) { return false; }
bool load_adress_gen::on_enter(const bin_literal_node &node) { return false; }
bool load_adress_gen::on_enter(const string_literal_node &node) {
  return false;
}
bool load_adress_gen::on_enter(const regex_literal_node &node) { return false; }
bool load_adress_gen::on_enter(const template_string_node &node) {
  return false;
}

bool hoisted_codegen::on_enter(const var_decl_node &node) {
  if (node.keyword == "var") {
    for (auto *part : node.parts) {
      auto *define = builder.insert_inst<define_inst>(bb);
      auto *name = builder.get_c_str_val(to_str(part->name));
      builder.set_inst_arg(*define, define_inst::arguments::name, *name);
    }
  }
  return false;
}

// ============ inst_creator impl ============
inst_creator::result inst_creator::accept(const module_node &node) {
  for (auto *stmt : node.stmts) {
    auto res = visit(*stmt);
    if (std::holds_alternative<ir_error>(res)) {
      return std::get<ir_error>(res);
    }
  }
  return nullptr;
}
inst_creator::result inst_creator::accept(const param_list_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const block_node &node) {
  for (auto *child : node.stmts) {
    auto res = visit(*child);
    if (std::holds_alternative<ir_error>(res)) {
      return std::get<ir_error>(res);
    }
  }
  return nullptr;
}
inst_creator::result inst_creator::accept(const function_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const class_func_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const class_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const arrow_function_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const identifier_expr_node &node) {
  return ir_error{"Not implemented", {}};
}

inst_creator::result inst_creator::accept(const int_literal_node &node) {
  return builder.ctx.get_c_num_val(
      static_cast<double>(std::strtol(node.val.data(), nullptr, 10)));
}
inst_creator::result inst_creator::accept(const float_literal_node &node) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const hex_literal_node &node) {
  return builder.ctx.get_c_num_val(
      static_cast<double>(std::strtol(node.val.data() + 2, nullptr, 16)));
}
inst_creator::result inst_creator::accept(const oct_literal_node &node) {
  return builder.ctx.get_c_num_val(
      static_cast<double>(std::strtol(node.val.data() + 2, nullptr, 8)));
}
inst_creator::result inst_creator::accept(const bin_literal_node &node) {
  return builder.ctx.get_c_num_val(
      static_cast<double>(std::strtol(node.val.data() + 2, nullptr, 2)));
}
inst_creator::result inst_creator::accept(const string_literal_node &node) {
  return builder.get_c_str_val(
      std::string{node.val.data() + 1, node.val.size() - 2});
}
inst_creator::result inst_creator::accept(const regex_literal_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const template_string_node &node) {
  return builder.get_c_str_val(
      std::string{node.val.data() + 1, node.val.size() - 2});
}
inst_creator::result inst_creator::accept(const template_literal_node &node) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const array_literal_node &node) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const object_entry_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const object_literal_node &) {
  return ir_error{"Not implemented", {}};
}

inst_creator::result inst_creator::accept(const member_access_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const computed_member_access_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const argument_list_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const call_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const spread_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const new_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const new_target_node &) {
  return ir_error{"Not implemented", {}};
}
// Unary expressions
inst_creator::result inst_creator::accept(const postfix_increment_node &node) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const postfix_decrement_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const prefix_increment_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const prefix_decrement_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const prefix_plus_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const prefix_minus_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const not_expr_node &node) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const binverse_expr_node &node) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const typeof_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const void_expr_node &node) {
  visit(*node.value);
  return builder.ctx.get_undefined();
}
inst_creator::result inst_creator::accept(const delete_expr_node &) {
  return ir_error{"Not implemented", {}};
}

// arithmetic binops
template <class AstNodeTy>
std::optional<ir_error>
inst_creator::load_binop_args(const AstNodeTy &node, value **lhs, value **rhs) {
  auto lhs_err = visit(*node.lhs);
  if (std::holds_alternative<ir_error>(lhs_err)) {
    return std::get<ir_error>(lhs_err);
  }
  auto rhs_err = visit(*node.rhs);
  if (std::holds_alternative<ir_error>(rhs_err)) {
    return std::get<ir_error>(rhs_err);
  }
  *lhs = std::get<value *>(lhs_err);
  *rhs = std::get<value *>(rhs_err);
  return std::nullopt;
}
template <class AstNodeTy, class InstTy>
std::variant<ir_error, InstTy *>
inst_creator::arithmetic_binop_codegen(const AstNodeTy &node) {
  value *lhs, *rhs;
  if (auto err = load_binop_args<AstNodeTy>(node, &lhs, &rhs)) {
    return *err;
  }
  std::array<value *, 1> args;
  auto *to_num = builder.get_intrinsic(intrinsic::to_number);
  /* turn lhs into a number */
  args[0] = lhs;
  auto *lhs_args = builder.prepare_call_args(*IP, args.begin(), args.end());
  auto *lhs_num = builder.insert_inst<call_inst>(*IP);
  builder.set_inst_arg(*lhs_num, call_inst::arguments::callee, *to_num);
  builder.set_inst_arg(*lhs_num, call_inst::arguments::this_val,
                       *builder.ctx.get_undefined());
  builder.set_inst_arg(*lhs_num, call_inst::arguments::arguments, *lhs_args);
  /* turn rhs into a number */
  args[0] = rhs;
  auto *rhs_args = builder.prepare_call_args(*IP, args.begin(), args.end());
  auto *rhs_num = builder.insert_inst<call_inst>(*IP);
  builder.set_inst_arg(*rhs_num, call_inst::arguments::callee, *to_num);
  builder.set_inst_arg(*rhs_num, call_inst::arguments::this_val,
                       *builder.ctx.get_undefined());
  builder.set_inst_arg(*rhs_num, call_inst::arguments::arguments, *rhs_args);

  auto *res = builder.insert_inst<InstTy>(*IP);
  builder.set_inst_arg(*res, InstTy::arguments::lhs, *lhs_num);
  builder.set_inst_arg(*res, InstTy::arguments::rhs, *rhs_num);
  return res;
}

inst_creator::result inst_creator::accept(const add_node &node) {
  // See dmitripavlutin.com/javascriptss-addition-operator-demystified/
  assert(IP);
  auto &start = *IP;
  assert(start.get_parent());
  auto &F = *IP->get_parent();

  value *lhs, *rhs;
  if (auto err = load_binop_args<add_node>(node, &lhs, &rhs))
    return *err;

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
    builder.set_inst_arg(*add_br, br_inst::arguments::target, *val_unification);
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

template <class InstTy>
inst_creator::result to_inster_result(std::variant<ir_error, InstTy *> res) {
  if (std::holds_alternative<ir_error>(res))
    return std::get<ir_error>(res);
  return static_cast<value *>(std::get<InstTy *>(res));
}

inst_creator::result inst_creator::accept(const subtract_node &node) {
  return to_inster_result(
      std::move(arithmetic_binop_codegen<subtract_node, sub_inst>(node)));
}
inst_creator::result inst_creator::accept(const multiply_node &node) {
  return to_inster_result(
      std::move(arithmetic_binop_codegen<multiply_node, mul_inst>(node)));
}
inst_creator::result inst_creator::accept(const divide_node &node) {
  return to_inster_result(
      std::move(arithmetic_binop_codegen<divide_node, div_inst>(node)));
}
inst_creator::result inst_creator::accept(const pow_expr_node &node) {
  return to_inster_result(
      std::move(arithmetic_binop_codegen<pow_expr_node, pow_inst>(node)));
}
inst_creator::result inst_creator::accept(const modulo_expr_node &node) {
  return to_inster_result(
      std::move(arithmetic_binop_codegen<modulo_expr_node, mod_inst>(node)));
}
// comparison binops
inst_creator::result inst_creator::accept(const less_expr_node &node) {
  auto cmp_or_err = arithmetic_binop_codegen<less_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::lt);
  return cmp;
}
inst_creator::result inst_creator::accept(const less_eq_expr_node &node) {
  auto cmp_or_err = arithmetic_binop_codegen<less_eq_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::leq);
  return cmp;
}
inst_creator::result inst_creator::accept(const greater_expr_node &node) {
  auto cmp_or_err = arithmetic_binop_codegen<greater_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::gt);
  return cmp;
}
inst_creator::result inst_creator::accept(const greater_eq_expr_node &node) {
  auto cmp_or_err =
      arithmetic_binop_codegen<greater_eq_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::geq);
  return cmp;
}
inst_creator::result inst_creator::accept(const equals_expr_node &node) {
  auto cmp_or_err = arithmetic_binop_codegen<equals_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::eq);
  return cmp;
}
inst_creator::result inst_creator::accept(const strong_equals_expr_node &node) {
  auto cmp_or_err =
      arithmetic_binop_codegen<strong_equals_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::eqeq);
  return cmp;
}
inst_creator::result inst_creator::accept(const not_equals_expr_node &node) {
  auto cmp_or_err =
      arithmetic_binop_codegen<not_equals_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::neq);
  return cmp;
}
inst_creator::result
inst_creator::accept(const strong_not_equals_expr_node &node) {
  auto cmp_or_err =
      arithmetic_binop_codegen<strong_not_equals_expr_node, cmp_inst>(node);
  if (std::holds_alternative<ir_error>(cmp_or_err))
    return std::get<ir_error>(cmp_or_err);
  auto *cmp = std::get<cmp_inst *>(cmp_or_err);
  cmp->set_op(cmp_operator::neqeq);
  return cmp;
}
inst_creator::result inst_creator::accept(const log_and_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const log_or_expr_node &) {
  return ir_error{"Not implemented", {}};
}
// bitwise binops
inst_creator::result inst_creator::accept(const lshift_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const rshift_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const log_rshift_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const bitwise_and_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const bitwise_or_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const bitwise_xor_expr_node &) {
  return ir_error{"Not implemented", {}};
}
// assignment binops
inst_creator::result inst_creator::accept(const assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const add_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const subtract_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const multiply_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const divide_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const modulo_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const pow_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const lshift_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const rshift_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const log_rshift_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const and_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const or_assign_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const xor_assign_node &) {
  return ir_error{"Not implemented", {}};
}
// other binops
inst_creator::result inst_creator::accept(const comma_operator_node &node) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const ternary_operator_node &) {
  return ir_error{"Not implemented", {}};
}
// keyword binops
inst_creator::result inst_creator::accept(const in_expr_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const instanceof_expr_node &) {
  return ir_error{"Not implemented", {}};
}

inst_creator::result inst_creator::accept(const function_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const class_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const label_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const var_decl_part_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const empty_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const var_decl_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const if_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const do_while_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const while_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const for_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const for_in_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const for_of_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const switch_clause_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const case_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const switch_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const break_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const continue_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const return_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const throw_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const catch_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const try_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
// FIXME these are not yet correctly modeled
inst_creator::result inst_creator::accept(const import_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const export_stmt_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const import_wildcard_node &) {
  return ir_error{"Not implemented", {}};
}
inst_creator::result inst_creator::accept(const export_wildcard_node &) {
  return ir_error{"Not implemented", {}};
}
// ===========================================
} // namespace jnsn
