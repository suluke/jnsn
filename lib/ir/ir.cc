#include "parsing/ir/ir.h"
#include "parsing/ir/ir_context.h"
#include "parsing/util.h"
#include <iomanip>
#include <iostream>

using namespace parsing;

#define INDENT_HELPER()                                                        \
  do {                                                                         \
    if (indent) {                                                              \
      auto flags = stream.flags();                                             \
      stream << std::right << std::setfill(' ') << std::setw(indent) << ' ';   \
      stream.flags(flags);                                                     \
    }                                                                          \
  } while (false)

void constant::print(std::ostream &stream, unsigned indent) const {
  if (isa<c_bool_val>(*this)) {
    static_cast<const c_bool_val *>(this)->print(stream, indent);
  } else if (isa<c_num_val>(*this)) {
    static_cast<const c_num_val *>(this)->print(stream, indent);
  } else if (isa<c_str_val>(*this)) {
    static_cast<const c_str_val *>(this)->print(stream, indent);
  } else if (isa<undefined_val>(*this)) {
    static_cast<const undefined_val *>(this)->print(stream, indent);
  } else if (isa<null_val>(*this)) {
    static_cast<const null_val *>(this)->print(stream, indent);
  }
  unreachable("Unknown constant type encountered");
}
void c_bool_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << (val ? "true" : "false");
}
void c_num_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << val;
}
void c_str_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << val;
}
void undefined_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << "undefined";
}
void null_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << "null";
}

/// Instruction visitor
// FIXME move this somewhere public (i.e. header)
struct inst_visitor_base {
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  void accept(const NAME##_inst &) {}
#include "parsing/ir/instructions.def"
};
template <class impl> struct inst_visitor : public inst_visitor_base {
  void visit(const instruction &inst) {
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  if (isa<NAME##_inst>(inst)) {                                                \
    static_cast<impl *>(this)->accept(static_cast<const NAME##_inst &>(inst)); \
  }
#include "parsing/ir/instructions.def"
  }
};

void instruction::print(std::ostream &stream, unsigned indent) const {
  struct print_dispatcher : public inst_visitor<print_dispatcher> {
    std::ostream &stream;
    unsigned indent;
    print_dispatcher(std::ostream &stream, unsigned indent)
        : stream(stream), indent(indent) {}
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  void accept(const NAME##_inst &inst) { inst.print(stream, indent); }
#include "parsing/ir/instructions.def"
  };
  auto print = print_dispatcher(stream, indent);
  print.visit(*this);
}
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  void NAME##_inst::print(std::ostream &stream, unsigned indent) const {       \
    INDENT_HELPER();                                                           \
    if (!isa<void_type>(get_type())) {                                         \
      stream << get_ctx().get_unique_id(*this) << " = ";                       \
    }                                                                          \
    stream << #NAME << " ";                                                    \
    for (auto *arg : args) {                                                   \
      stream << get_ctx().get_unique_id(*arg) << ", ";                         \
    }                                                                          \
  }
#include "parsing/ir/instructions.def"
void basic_block::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << get_ctx().get_unique_id(*this) << ":\n";
  for (const auto *Inst : instructions) {
    Inst->print(stream, indent + 2);
    stream << "\n";
  }
}
void function::print(std::ostream &stream, unsigned indent) const {
  stream << "function " << get_ctx().get_unique_id(*this) << " {\n";
  for (auto *block : blocks) {
    block->print(stream, indent + 2);
    stream << "\n";
  }
  stream << "}\n";
}