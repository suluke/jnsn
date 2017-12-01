#ifndef JNSN_IR_INSTRUCTIONS_H
#define JNSN_IR_INSTRUCTIONS_H
#include "jnsn/ir/value.h"
#include <array>
#include <type_traits>

namespace jnsn {

class basic_block;

/// base class of all instructions
class instruction : public value {
  friend class ir_context;
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::instruction_kind;
  basic_block *parent = nullptr;
protected:
  std::string get_unique_id() const;
  /// Used for printing instruction's arguments
  std::string get_unique_id(const value &val) const;

public:
  instruction(ir_context &ctx, ir_value_kind kind, type ty)
      : value(ctx, kind, ty) {}
  bool has_parent() const { return parent; }
  basic_block *get_parent() const { return parent; }
  void print(std::ostream &stream, unsigned indent = 0) const;
};

/// instruction properties
enum class cmp_operator {
#define CMP_OP(OP) OP,
#include "jnsn/ir/instructions.def"
};

/// instruction generation
#define RETURN(RET) RET##_type::create()
#define ARG(NAME, TY) NAME,
#define ARGS(...) enum class arguments { __VA_ARGS__ _ARGC_ };
#define PROP(NAME, TY)                                                         \
private:                                                                       \
  TY NAME;                                                                     \
public:                                                                        \
  TY get_##NAME() { return NAME; }                                             \
  void set_##NAME(TY val) { NAME = val; }
#define PROPS(...) __VA_ARGS__
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  class NAME##_inst : public instruction {                                     \
    template <class ty> friend bool isa(const value &);                        \
    friend class ir_context;                                                   \
    static constexpr ir_value_kind kind = ir_value_kind::NAME##_inst_kind;     \
                                                                               \
  public:                                                                      \
    NAME##_inst(ir_context &ctx)                                               \
        : instruction(ctx, ir_value_kind::NAME##_inst_kind, RET) {}            \
    ARGUMENTS                                                                  \
    void print(std::ostream &stream, unsigned indent = 0) const;               \
    PROPERTIES                                                                 \
                                                                               \
  private:                                                                     \
    std::array<value *, static_cast<std::underlying_type_t<arguments>>(        \
                            arguments::_ARGC_)>                                \
        args;                                                                  \
  };
#include "jnsn/ir/instructions.def"

} // namespace jnsn
#endif // JNSN_IR_INSTRUCTIONS_H
