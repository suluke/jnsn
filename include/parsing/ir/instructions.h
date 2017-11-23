#ifndef PARSING_IR_INSTRUCTIONS_H
#define PARSING_IR_INSTRUCTIONS_H
#include "parsing/ir/value.h"
#include <array>
#include <type_traits>

namespace parsing {

class basic_block;

/// base class of all instructions
class instruction : public value {
  template <class ty> friend bool isa(const value &);
  static constexpr ir_value_kind kind = ir_value_kind::instruction_kind;

public:
  basic_block *parent;
  instruction(ir_context &ctx, ir_value_kind kind, type ty)
      : value(ctx, kind, ty) {}
  void print(std::ostream &stream, unsigned indent = 0) const;
};

/// instruction properties
enum class cmp_operator {
#define CMP_OP(OP) OP,
#include "parsing/ir/instructions.def"
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
#include "parsing/ir/instructions.def"

} // namespace parsing
#endif // PARSING_IR_INSTRUCTIONS_H