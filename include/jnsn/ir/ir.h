#ifndef JNSN_IR_H
#define JNSN_IR_H
#include "jnsn/ir/instructions.h"
#include "jnsn/string_table.h"
#include <algorithm>
#include <cassert>
#include <vector>

namespace jnsn {
/// Forward-declare containers (for `parent` pointers)
class module;
class function;

/// ir constants/immediate values
struct constant : public value {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::constant_kind;

protected:
  constant(ir_context &ctx, ir_value_kind k, type ty) : value(ctx, k, ty) {}
};

class c_bool_val : public constant {
  template <class ty> friend bool isa(const value &);
  friend class ir_context;
  static constexpr ir_value_kind kind = ir_value_kind::const_bool_kind;
  bool val;

  c_bool_val(bool val, ir_context &ctx)
      : constant(ctx, ir_value_kind::const_bool_kind, c_bool_type::create()),
        val(val) {}

public:
  bool get_value() const { return val; }
};

class c_num_val : public constant {
  template <class ty> friend bool isa(const value &);
  friend class ir_context;
  static constexpr ir_value_kind kind = ir_value_kind::const_num_kind;
  double val;

  c_num_val(double val, ir_context &ctx)
      : constant(ctx, ir_value_kind::const_num_kind, c_num_type::create()),
        val(val) {}

public:
  double get_value() const { return val; }
};

class undefined_val : public constant {
  template <class ty> friend bool isa(const value &);
  friend class ir_context;
  static constexpr ir_value_kind kind = ir_value_kind::undefined_kind;

  undefined_val(ir_context &ctx)
      : constant(ctx, ir_value_kind::undefined_kind, undefined_type::create()) {
  }
};

class null_val : public constant {
  template <class ty> friend bool isa(const value &);
  friend class ir_context;
  static constexpr ir_value_kind kind = ir_value_kind::null_kind;

  null_val(ir_context &ctx)
      : constant(ctx, ir_value_kind::null_kind, null_type::create()) {}
};

class global_value : public value {
  friend class ir_context;
  friend class module;
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::global_value_kind;
  module *parent = nullptr;

protected:
  global_value(ir_context &ctx, ir_value_kind k, type ty) : value(ctx, k, ty) {}

public:
  bool has_parent() const { return parent; }
  module *get_parent() const { return parent; }
};

class str_val : public global_value {
  template <class ty> friend bool isa(const value &);
  friend class ir_context;
  friend class module;
  static constexpr ir_value_kind kind = ir_value_kind::const_str_kind;
  string_table_entry val;

  str_val(string_table_entry val, ir_context &ctx)
      : global_value(ctx, ir_value_kind::const_str_kind, c_str_type::create()),
        val(val) {}

public:
  std::string_view get_value() const { return val; }
};

/// BasicBlock
class basic_block : public value {
  friend class ir_context;
  friend class instruction;
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::basic_block_kind;
  using inst_list = std::vector<instruction *>;
  inst_list instructions;
  function *parent = nullptr;

  basic_block(ir_context &ctx)
      : value(ctx, ir_value_kind::basic_block_kind,
              basic_block_type::create()) {}

public:
  basic_block(const basic_block &) = default;
  basic_block(basic_block &&) = default;
  bool has_parent() const { return parent; }
  function *get_parent() const { return parent; }
  bool contains(const instruction &inst) const {
    return std::find(begin(), end(), &inst) != end();
  }
  size_t size() const { return instructions.size(); }
  inst_list::const_iterator begin() const { return instructions.begin(); }
  inst_list::const_iterator end() const { return instructions.end(); }
  bool has_terminator();
};

/// Function
class function : public global_value {
  friend class ir_context;
  friend class basic_block;
  friend class instruction;
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::function_kind;
  using block_list = std::vector<basic_block *>;
  block_list blocks;
  bool intrinsic = false;
  function(ir_context &ctx, bool intrinsic = false)
      : global_value(ctx, ir_value_kind::function_kind,
                     function_type::create()),
        intrinsic(intrinsic) {}

public:
  function(const function &) = default;
  function(function &&) = default;
  basic_block *get_entry() const {
    assert(!blocks.empty());
    return blocks.front();
  }
  bool is_intrinsic() { return intrinsic; }
  block_list::const_iterator begin() const { return blocks.begin(); }
  block_list::const_iterator end() const { return blocks.end(); }
};

/// type introspection support for values
template <class ty> bool isa(const value &val) {
  static_assert(std::is_base_of_v<value, ty>);
  if constexpr (std::is_same_v<ty, value>)
    return true;
  else if constexpr (std::is_same_v<ty, global_value>) {
    switch (val.get_kind()) {
    case ir_value_kind::function_kind:
    case ir_value_kind::const_str_kind:
      return true;
    default:
      return false;
    }
  } else if constexpr (std::is_same_v<ty, constant>) {
    switch (val.get_kind()) {
    case ir_value_kind::const_bool_kind:
    case ir_value_kind::const_num_kind:
    case ir_value_kind::undefined_kind:
    case ir_value_kind::null_kind:
      return true;
    default:
      return false;
    }
  } else if constexpr (std::is_same_v<ty, instruction>) {
    switch (val.get_kind()) {
#define INSTRUCTION(NAME, ARG, PROP, RET) case ir_value_kind::NAME##_inst_kind:
#include "jnsn/ir/instructions.def"
      return true;
    default:
      return false;
    }
  }
  return ty::kind == val.get_kind();
}
template <class ty> bool isa(const value *val) {
  assert(val);
  return isa<ty>(*val);
}
} // namespace jnsn
#endif // JNSN_IR_H
