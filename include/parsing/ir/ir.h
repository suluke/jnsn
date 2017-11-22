#ifndef PARSING_IR_H
#define PARSING_IR_H
#include "parsing/ir/instructions.h"
#include "parsing/string_table.h"
#include <vector>

namespace parsing {
/// ir constants/immediate values
struct constant : public value {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::constant_kind;

protected:
  constant(ir_context &ctx, ir_value_kind k, type ty) : value(ctx, k, ty) {}

public:
  void print(std::ostream &stream, unsigned indent = 0) const;
};
class c_bool_val : public constant {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::const_bool_kind;
  bool val;

public:
  c_bool_val(bool val, ir_context &ctx)
      : constant(ctx, ir_value_kind::const_bool_kind, c_bool_type::create()),
        val(val) {}
  void print(std::ostream &stream, unsigned indent = 0) const;
};
class c_num_val : public constant {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::const_num_kind;
  double val;

public:
  c_num_val(double val, ir_context &ctx)
      : constant(ctx, ir_value_kind::const_num_kind, c_num_type::create()),
        val(val) {}
  void print(std::ostream &stream, unsigned indent = 0) const;
};
class c_str_val : public constant {
  template <class ty> friend bool isa(const value &);
  friend class ir_context;
  static constexpr ir_value_kind kind = ir_value_kind::const_str_kind;
  string_table_entry val;

public:
  c_str_val(string_table_entry val, ir_context &ctx)
      : constant(ctx, ir_value_kind::const_str_kind, c_str_type::create()),
        val(val) {}
  void print(std::ostream &stream, unsigned indent = 0) const;
};
class undefined_val : public constant {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::undefined_kind;

public:
  undefined_val(ir_context &ctx)
      : constant(ctx, ir_value_kind::undefined_kind, undefined_type::create()) {
  }
  void print(std::ostream &stream, unsigned indent = 0) const;
};
class null_val : public constant {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::null_kind;

public:
  null_val(ir_context &ctx)
      : constant(ctx, ir_value_kind::null_kind, null_type::create()) {}
  void print(std::ostream &stream, unsigned indent = 0) const;
};

/// Forward-declare containers (for `parent` pointers)
class module;
class function;

/// BasicBlock
class basic_block : public value {
  friend class ir_context;
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::basic_block_kind;
  std::vector<instruction *> instructions;
  function *parent;

public:
  basic_block(ir_context &ctx)
      : value(ctx, ir_value_kind::basic_block_kind,
              basic_block_type::create()) {}
  void print(std::ostream &stream, unsigned indent = 0) const;
};

/// Function
class function : public value {
  friend class ir_context;
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::function_kind;
  std::vector<basic_block *> blocks;
  module *parent;
  function(ir_context &ctx)
      : value(ctx, ir_value_kind::function_kind, function_type::create()) {}

public:
  function(const function &) = default;
  function(function &&) = default;
  basic_block *get_entry() const { return blocks.front(); }
  void print(std::ostream &stream, unsigned indent = 0) const;
};

/// type introspection support for values
template <class ty> bool isa(const value &val) {
  static_assert(std::is_base_of_v<value, ty>);
  if constexpr (std::is_same_v<ty, value>)
    return true;
  else if constexpr (std::is_same_v<ty, basic_block>)
    return val.get_kind() == ir_value_kind::basic_block_kind;
  else if constexpr (std::is_same_v<ty, function>)
    return val.get_kind() == ir_value_kind::function_kind;
  else if constexpr (std::is_same_v<ty, constant>) {
    switch (val.get_kind()) {
    case ir_value_kind::const_bool_kind:
    case ir_value_kind::const_num_kind:
    case ir_value_kind::const_str_kind:
      return true;
    default:
      return false;
    }
  } else if constexpr (std::is_same_v<ty, instruction>) {
    switch (val.get_kind()) {
#define INSTRUCTION(NAME, ARG, PROP, RET) case ir_value_kind::NAME##_inst_kind:
#include "parsing/ir/instructions.def"
      return true;
    default:
      return false;
    }
  }
  return ty::kind == val.get_kind();
}
} // namespace parsing
#endif // PARSING_IR_H