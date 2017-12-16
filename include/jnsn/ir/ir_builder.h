#ifndef JNSN_IR_BUILDER_H
#define JNSN_IR_BUILDER_H
#include "jnsn/ir/intrinsics.h"
#include "jnsn/ir/ir_context.h"
#include <string>

namespace jnsn {
class module;

struct ir_builder {
  module &mod;
  ir_context &ctx;
  ir_builder(module &mod);
  str_val *get_str_val(std::string str);
  basic_block *make_block(function &F);
  function *make_function();
  call_inst *cast_to_number(basic_block &IP, value &val);
  call_inst *cast_to_primitive(basic_block &IP, value &val);
  call_inst *cast_to_string(basic_block &IP, value &val);
  call_inst *cast_to_bool(basic_block &IP, value &val);
  call_inst *test_is_string(basic_block &IP, value &val);
  call_inst *concat_strings(basic_block &IP, value &lhs, value &rhs);
  call_inst *load_or_undefined(basic_block &IP, value &address, str_val &prop);
  function *get_intrinsic(intrinsic i);

  template <class ty> ty *insert_inst(basic_block &bb) {
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
      auto *idx_val = get_str_val(std::to_string(idx));
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
} // namespace jnsn
#endif // JNSN_IR_BUILDER_H
