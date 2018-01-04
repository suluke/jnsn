#include "jnsn/ir/ir_context.h"
#include "jnsn/ir/module.h"
#include "jnsn/util.h"
#include <cassert>
#include <sstream>

using namespace jnsn;

ir_context::ir_context(){
#define INTRINSIC(NAME, ARGS, RET)                                             \
  {                                                                            \
    functions.emplace_back(function{*this, true});                             \
    auto *I = &functions.back();                                               \
    I->set_name(#NAME);                                                        \
    intrinsics.emplace(intrinsic::NAME, I);                                    \
  }
#include "jnsn/ir/intrinsics.def"
}

c_num_val *ir_context::get_c_num_val(double d) {
  auto I = nums.find(d);
  if (I == nums.end()) {
    I = nums.emplace(d, c_num_val{d, *this}).first;
  }
  return &I->second;
}
string_table_entry ir_context::internalize_string(std::string s) {
  return str_table.get_handle(std::move(s));
}
str_val *ir_context::make_str_val(std::string val) {
  auto h = str_table.get_handle(std::move(val));
  strs.emplace_back(str_val(h, *this));
  return &strs.back();
}

function *ir_context::make_function() {
  functions.emplace_back(function{*this});
  return &functions.back();
}
basic_block *ir_context::make_block() {
  blocks.emplace_back(basic_block{*this});
  return &blocks.back();
}
void ir_context::insert_function_into(module &M, function &F) {
  F.parent = &M;
  M.functions.emplace(&F);
}
void ir_context::insert_block_into(function &F, basic_block &BB) {
  BB.parent = &F;
  F.blocks.emplace_back(&BB);
}
void ir_context::insert_inst_into(basic_block &BB, instruction &Inst) {
  Inst.parent = &BB;
  BB.instructions.emplace_back(&Inst);
}

function *ir_context::get_intrinsic(intrinsic i) {
  assert(intrinsics.count(i));
  return intrinsics[i];
}
