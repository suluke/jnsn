#ifndef JNSN_IR_VALUE_H
#define JNSN_IR_VALUE_H
#include "jnsn/ir/types.h"
#include <string>

namespace jnsn {

enum class ir_value_kind {
  value_kind,
  global_value_kind,
  function_kind,
  basic_block_kind,
  instruction_kind,
  constant_kind,
  const_bool_kind,
  const_num_kind,
  const_str_kind,
  undefined_kind,
  null_kind,
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET) NAME##_inst_kind,
#include "jnsn/ir/instructions.def"
};

class ir_context;

/// base class of all values that make up an IR module
// Therefore, it has both a ir_value_kind (for isa<> support) as well as a
// runtime type (for static analysis / static constraint verification)
class value {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::value_kind;
  ir_context &ctx;
  const ir_value_kind dyn_kind;
  const type ty;
  std::string name;

protected:
  value(ir_context &ctx, ir_value_kind dyn_kind, type ty) : ctx(ctx), dyn_kind(dyn_kind), ty(ty) {}
  ir_context &get_ctx() { return ctx; }
  const ir_context &get_ctx() const { return ctx; }

public:
  ir_value_kind get_kind() const { return dyn_kind; }
  type get_type() const { return ty; }
  const std::string &get_name() const { return name; }
  bool has_name() const { return name != ""; }
  void set_name(std::string name) { this->name = std::move(name); }
};

} // namespace jnsn
#endif // JNSN_IR_VALUE_H
