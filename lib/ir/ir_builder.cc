#include "jnsn/ir/ir_builder.h"
#include "jnsn/ir/module.h"

using namespace jnsn;
ir_builder::ir_builder(module &mod) : mod(mod), ctx(mod.get_context()) {}

function *ir_builder::get_intrinsic(intrinsic i) {
  return ctx.get_intrinsic(i);
}
str_val *ir_builder::get_str_val(std::string str) {
  return mod.get_str_val(std::move(str));
}
basic_block *ir_builder::make_block(function &F) {
  auto *bb = ctx.make_block();
  ctx.insert_block_into(F, *bb);
  return bb;
}
function *ir_builder::make_function() {
  auto *F = ctx.make_function();
  ctx.insert_function_into(mod, *F);
  return F;
}
call_inst *ir_builder::cast_to_number(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *to_num = get_intrinsic(intrinsic::to_number);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *num = insert_inst<call_inst>(IP);
  set_inst_arg(*num, call_inst::arguments::callee, *to_num);
  set_inst_arg(*num, call_inst::arguments::arguments, *args_list);
  return num;
}
call_inst *ir_builder::cast_to_primitive(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *to_prim = get_intrinsic(intrinsic::to_primitive);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *prim = insert_inst<call_inst>(IP);
  set_inst_arg(*prim, call_inst::arguments::callee, *to_prim);
  set_inst_arg(*prim, call_inst::arguments::arguments, *args_list);
  return prim;
}
call_inst *ir_builder::cast_to_bool(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *to_bool = get_intrinsic(intrinsic::to_bool);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *as_bool = insert_inst<call_inst>(IP);
  set_inst_arg(*as_bool, call_inst::arguments::callee, *to_bool);
  set_inst_arg(*as_bool, call_inst::arguments::arguments, *args_list);
  return as_bool;
}
call_inst *ir_builder::cast_to_string(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *to_str = get_intrinsic(intrinsic::to_string);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *str = insert_inst<call_inst>(IP);
  set_inst_arg(*str, call_inst::arguments::callee, *to_str);
  set_inst_arg(*str, call_inst::arguments::arguments, *args_list);
  return str;
}
call_inst *ir_builder::test_is_string(basic_block &IP, value &val) {
  std::array<value *, 1> args = {&val};
  auto *is_str = get_intrinsic(intrinsic::is_string);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *res = insert_inst<call_inst>(IP);
  set_inst_arg(*res, call_inst::arguments::callee, *is_str);
  set_inst_arg(*res, call_inst::arguments::arguments, *args_list);
  return res;
}
call_inst *ir_builder::concat_strings(basic_block &IP, value &lhs, value &rhs) {
  std::array<value *, 2> args = {&lhs, &rhs};
  auto *concat = get_intrinsic(intrinsic::concat);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *res = insert_inst<call_inst>(IP);
  set_inst_arg(*res, call_inst::arguments::callee, *concat);
  set_inst_arg(*res, call_inst::arguments::arguments, *args_list);
  return res;
}
call_inst *ir_builder::load_or_undefined(basic_block &IP, value &addr,
                                         str_val &prop) {
  std::array<value *, 2> args = {&addr, &prop};
  auto *lou = get_intrinsic(intrinsic::load_or_undefined);
  auto *args_list = prepare_call_args(IP, args.begin(), args.end());
  auto *res = insert_inst<call_inst>(IP);
  set_inst_arg(*res, call_inst::arguments::callee, *lou);
  set_inst_arg(*res, call_inst::arguments::arguments, *args_list);
  return res;
}
