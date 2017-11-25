#include "parsing/ir/ir_context.h"
#include "parsing/ir/module.h"
#include "parsing/util.h"
#include <cassert>
#include <sstream>

using namespace parsing;

ir_context::ir_context(){
#define INTRINSIC(NAME, ARGS, RET)                                             \
  {                                                                            \
    auto *i = make_function();                                                 \
    i->set_name("!" #NAME);                                                    \
    intrinsics.emplace(intrinsic::NAME, i);                                    \
  }
#include "parsing/ir/intrinsics.def"
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

// FIXME this is of course WIP and eventually needs a proper impl
std::string ir_context::get_unique_id(const value &val) const {
  if (isa<constant>(val)) {
    return get_unique_id(static_cast<const constant &>(val));
  } else if (isa<instruction>(val)) {
    return get_unique_id(static_cast<const instruction &>(val));
  } else if (isa<basic_block>(val)) {
    return get_unique_id(static_cast<const basic_block &>(val));
  } else if (isa<function>(val)) {
    return get_unique_id(static_cast<const function &>(val));
  }
  unreachable("Unknown value type");
}
std::string ir_context::get_unique_id(const constant &c) const {
  if (isa<c_str_val>(c)) {
    return "%str";
  }
  std::stringstream ss;
  c.print(ss);
  return ss.str();
}
std::string ir_context::get_unique_id(const instruction &) const { return "%"; }
std::string ir_context::get_unique_id(const basic_block &) const {
  return "label";
}
std::string ir_context::get_unique_id(const function &F) const {
  return F.get_name();
}