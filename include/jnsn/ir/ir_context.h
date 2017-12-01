#ifndef JNSN_IR_CONTEXT_H
#define JNSN_IR_CONTEXT_H
#include "jnsn/ir/ir.h"
#include "jnsn/ir/intrinsics.h"
#include <deque>
#include <map>
#include <variant>

namespace jnsn {
class module;

class ir_context {
  friend struct ir_builder;
  friend class module;
  friend class function;
  friend class basic_block;
  friend class value;
  friend class constant;
  friend class c_bool_val;
  friend class c_num_val;
  friend class c_str_val;
  friend class undefined_val;
  friend class null_val;
  friend class instruction;
#define INSTRUCTION(NAME, ARGUMENTS, PROPS, RET) friend class NAME##_inst;
#include "jnsn/ir/instructions.def"

  std::deque<basic_block> blocks;
  std::deque<function> functions;
  std::map<intrinsic, function *> intrinsics;
  /// union of instructions for unified storage
  using inst = std::variant<
#define INSTRUCTION(NAME, ARGUMENTS, PROPS, RET) NAME##_inst,
#include "jnsn/ir/instructions.def"
      std::nullptr_t // FIXME maybe we can omit this somehow
      >;
  std::deque<inst> insts;
  // literals
  undefined_val undef_v = {*this};
  null_val null_v = {*this};
  c_bool_val true_v = {true, *this};
  c_bool_val false_v = {false, *this};
  std::map<double, c_num_val> nums;
  string_table str_table;
  std::map<std::string_view, c_str_val> strs;

  function *make_function();
  basic_block *make_block();
  template <class ty> ty *make_inst() {
    insts.emplace_back(ty(*this));
    return &std::get<ty>(insts.back());
  }
  void insert_function_into(module &M, function &F);
  void insert_block_into(function &F, basic_block &BB);
  void insert_inst_into(basic_block &BB, instruction &Inst);

  template <class Inst>
  void set_inst_arg(Inst &inst, typename Inst::arguments arg, value &val) {
    // TODO this indirection is intended to allow for user-tracking of
    // val
    inst.args[static_cast<std::underlying_type_t<typename Inst::arguments>>(
        arg)] = &val;
  }

  function *get_intrinsic(intrinsic);

public:
  ir_context();
  c_num_val *get_c_num_val(double d);
  c_str_val *get_c_str_val(std::string s);
  undefined_val *get_undefined() { return &undef_v; }
  null_val *get_null() { return &null_v; }
  c_bool_val *get_true() { return &true_v; }
  c_bool_val *get_false() { return &false_v; }
};

} // namespace jnsn
#endif // JNSN_IR_CONTEXT_H
