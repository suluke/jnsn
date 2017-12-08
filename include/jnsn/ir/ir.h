#ifndef JNSN_IR_H
#define JNSN_IR_H
#include "jnsn/ir/instructions.h"
#include "jnsn/string_table.h"
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

public:
  void print(std::ostream &stream, unsigned indent = 0) const;
  std::string str() const;
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
  std::string str() const;
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
  std::string str() const;
};
class undefined_val : public constant {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::undefined_kind;

public:
  undefined_val(ir_context &ctx)
      : constant(ctx, ir_value_kind::undefined_kind, undefined_type::create()) {
  }
  void print(std::ostream &stream, unsigned indent = 0) const;
  std::string str() const;
};
class null_val : public constant {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::null_kind;

public:
  null_val(ir_context &ctx)
      : constant(ctx, ir_value_kind::null_kind, null_type::create()) {}
  void print(std::ostream &stream, unsigned indent = 0) const;
  std::string str() const;
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
  std::string get_unique_id() const;
};
// FIXME drop the c_ prefix because strings are different from usual constants
class c_str_val : public global_value {
  template <class ty> friend bool isa(const value &);
  friend class ir_context;
  friend class module;
  static constexpr ir_value_kind kind = ir_value_kind::const_str_kind;
  string_table_entry val;

  using global_value::get_unique_id;
  c_str_val(string_table_entry val, ir_context &ctx)
      : global_value(ctx, ir_value_kind::const_str_kind, c_str_type::create()),
        val(val) {}
public:
  void print(std::ostream &stream, unsigned indent = 0) const;
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
  std::string get_unique_id(const instruction &) const;
  std::string get_unique_id() const;

public:
  basic_block(ir_context &ctx)
      : value(ctx, ir_value_kind::basic_block_kind,
              basic_block_type::create()) {}
  bool has_parent() const { return parent; }
  function *get_parent() const { return parent; }
  void print(std::ostream &stream, unsigned indent = 0) const;
  size_t size() const { return instructions.size(); }
  inst_list::const_iterator begin() const { return instructions.begin(); }
  inst_list::const_iterator end() const { return instructions.end(); }
};

/// Function
class function : public global_value {
  friend class ir_context;
  friend class basic_block;
  friend class instruction;
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::function_kind;
  std::vector<basic_block *> blocks;
  function(ir_context &ctx)
      : global_value(ctx, ir_value_kind::function_kind,
                     function_type::create()) {}

  std::string get_unique_id(const instruction &) const;
  std::string get_unique_id(const basic_block &) const;
  using global_value::get_unique_id;

public:
  function(const function &) = default;
  function(function &&) = default;
  basic_block *get_entry() const { return blocks.front(); }
  bool is_intrinsic() {
    auto &name = get_name();
    if (name.empty())
      return false;
    return name[0] == '!';
  }
  void print(std::ostream &stream, unsigned indent = 0) const;
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
} // namespace jnsn
#endif // JNSN_IR_H
