#include "jnsn/ir/ir_context.h"
#include "jnsn/ir/module.h"
#include "jnsn/util.h"
#include <cassert>
#include <sstream>

using namespace jnsn;

ir_context::ir_context(){
#define INTRINSIC(NAME, ARGS, RET)                                             \
  {                                                                            \
    auto *i = make_function();                                                 \
    i->set_name("!" #NAME);                                                    \
    intrinsics.emplace(intrinsic::NAME, i);                                    \
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
c_str_val *ir_context::get_c_str_val(std::string s) {
  auto h = str_table.get_handle(s);
  auto I = strs.find(h);
  if (I == strs.end()) {
    I = strs.emplace(h, c_str_val{h, *this}).first;
  }
  return &I->second;
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
