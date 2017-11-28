#include "parsing/ir/ir.h"
#include "parsing/ir/ir_context.h"
#include "parsing/ir/module.h"
#include "parsing/util.h"
#include <algorithm>
#include <cassert>
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
  } else if (isa<undefined_val>(*this)) {
    static_cast<const undefined_val *>(this)->print(stream, indent);
  } else if (isa<null_val>(*this)) {
    static_cast<const null_val *>(this)->print(stream, indent);
  } else {
    unreachable("Unknown constant type encountered");
  }
}

std::string constant::str() const {
  if (isa<c_bool_val>(*this)) {
    return static_cast<const c_bool_val *>(this)->str();
  } else if (isa<c_num_val>(*this)) {
    return static_cast<const c_num_val *>(this)->str();
  } else if (isa<undefined_val>(*this)) {
    return static_cast<const undefined_val *>(this)->str();
  } else if (isa<null_val>(*this)) {
    return static_cast<const null_val *>(this)->str();
  }
  unreachable("Unknown constant type encountered");
}



std::string c_bool_val::str() const { return val ? "true" : "false"; }
void c_bool_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << str();
}
std::string c_num_val::str() const { return std::to_string(val); }
void c_num_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << val;
}
std::string undefined_val::str() const { return "undefined"; }
void undefined_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << str();
}
std::string null_val::str() const { return "null"; }
void null_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << str();
}

void c_str_val::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << val;
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

std::string instruction::get_unique_id(const value &val) const {
  if (isa<instruction>(val))
    return static_cast<const instruction &>(val).get_unique_id();
  else if (isa<basic_block>(val))
    return static_cast<const basic_block &>(val).get_unique_id();
  else if (isa<constant>(val))
    return static_cast<const constant &>(val).str();
  else if (isa<global_value>(val))
    return static_cast<const global_value &>(val).get_unique_id();
  // TODO
  return "%";
}
std::string instruction::get_unique_id() const {
  if (has_parent())
    return get_parent()->get_unique_id(*this);
  return "%?";
}

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
      stream << get_unique_id() << " = ";                                      \
    }                                                                          \
    stream << #NAME << " ";                                                    \
    if (!args.empty()) {                                                       \
      auto It = args.begin();                                                  \
      stream << get_unique_id(**It);                                           \
      while (++It != args.end()) {                                             \
        stream << ", " << get_unique_id(**It);                                 \
      }                                                                        \
    }                                                                          \
  }
#include "parsing/ir/instructions.def"

std::string basic_block::get_unique_id() const {
  if (!has_parent()) {
    if (!has_name())
      return "label";
    else
      return get_name();
  } else {
    return get_parent()->get_unique_id(*this);
  }
}

std::string basic_block::get_unique_id(const instruction &inst) const {
  assert(inst.get_parent() == this);
  if (has_parent())
    return get_parent()->get_unique_id(inst);
  auto It = std::find(instructions.begin(), instructions.end(), &inst);
  assert(It != instructions.end());
  auto pos = std::distance(instructions.begin(), It);
  if (inst.has_name())
    return "%" + inst.get_name() + std::to_string(pos);
  return "%" + std::to_string(pos);
}

void basic_block::print(std::ostream &stream, unsigned indent) const {
  INDENT_HELPER();
  stream << get_unique_id() << ":\n";
  for (const auto *Inst : instructions) {
    Inst->print(stream, indent + 2);
    stream << "\n";
  }
}

std::string global_value::get_unique_id() const {
  // TODO
  if (!has_parent()) {
    if (!has_name())
      return "anonymous";
    else
      return get_name();
  } else {
    return get_parent()->get_unique_id(*this);
  }
}

std::string function::get_unique_id(const basic_block &bb) const {
  assert(bb.has_parent() && bb.get_parent() == this);
  auto It = std::find(blocks.begin(), blocks.end(), &bb);
  assert(It != blocks.end());
  auto pos = std::distance(blocks.begin(), It);
  if (bb.has_name())
    return bb.get_name() + std::to_string(pos);
  return "label" + std::to_string(pos);
}
std::string function::get_unique_id(const instruction &inst) const {
  assert(inst.has_parent());
  auto *bb = inst.get_parent();
  assert(bb->has_parent() && bb->get_parent() == this);
  auto bbIt = std::find(blocks.begin(), blocks.end(), bb);
  assert(bbIt != blocks.end());
  auto bbPos = std::distance(blocks.begin(), bbIt);
  assert(bbPos >= 0);
  size_t idx = 0;
  for (size_t i = 0u; i < (size_t) bbPos; ++i) {
    idx += blocks[i]->size();
  }
  auto iIt = std::find(bb->begin(), bb->end(), &inst);
  assert(iIt != bb->end());
  idx += std::distance(bb->begin(), iIt);
  if (inst.has_name())
    return "%" + inst.get_name() + std::to_string(idx);
  return "%" + std::to_string(idx);
}

void function::print(std::ostream &stream, unsigned indent) const {
  stream << "function " << get_unique_id() << " {\n";
  for (auto *block : blocks) {
    block->print(stream, indent + 2);
    stream << "\n";
  }
  stream << "}\n";
}