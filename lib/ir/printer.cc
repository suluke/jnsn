#include "jnsn/ir/printer.h"
#include "jnsn/ir/module.h"
#include "jnsn/util.h"
#include <iomanip>

using namespace jnsn;

/// Helper function to perform correct indentation on a stream according
/// to a given ir_print_policy
static inline void indent(std::ostream &stream, const ir_print_policy policy) {
  if (policy.indent_begin > 0) {
    char ic = ' ';
    if (policy.indent_type == ir_print_policy::TABS) {
      ic = '\t';
    }
    auto flags = stream.flags();
    stream << std::right << std::setfill(ic) << std::setw(policy.indent_begin)
           << ic;
    stream.flags(flags);
  }
}

template <class T, class C>
static std::string get_numbered_name(const T &elm, const C &container,
                                     const char *defaultName) {
  std::string name(defaultName);
  if (elm.has_name()) {
    name = elm.get_name();
  }
  unsigned same_name = 0;
  if (name == "")
    same_name += 1;
  for (const T *O : container) {
    if (O == &elm) {
      if (same_name == 0) {
        return name;
      } else {
        return name + std::to_string(same_name - 1);
      }
    }
    if (O->get_name() ==
        elm.get_name()) { // important: use elm.get_name because local variable
                          // "name" may be "defaultName" if name is empty string
      same_name += 1;
    }
  }
  unreachable("Container does not contain element to get numbered name for");
}

static std::string to_string(const constant &c) {
  if (isa<c_bool_val>(c)) {
    auto &as_bool = static_cast<const c_bool_val &>(c);
    return as_bool.get_value() ? "true" : "false";
  } else if (isa<c_num_val>(c)) {
    auto &as_num = static_cast<const c_num_val &>(c);
    std::stringstream ss;
    ss << as_num.get_value();
    return ss.str();
  } else if (isa<undefined_val>(c)) {
    return "undefined";
  } else if (isa<null_val>(c)) {
    return "null";
  }
  unreachable("Unknown constant type encountered");
}

static std::string get_unique_id(const str_val &str) {
  if (str.has_parent()) {
    return get_numbered_name(str, str.get_parent()->get_strs(), "str");
  } else if (str.has_name()) {
    return str.get_name();
  } else {
    return "str";
  }
}

static std::string get_unique_id(const function &F) {
  if (F.has_parent()) {
    return get_numbered_name(F, F.get_parent()->get_functions(), "anonymous");
  } else if (F.has_name()) {
    return F.get_name();
  } else {
    return "anonymous";
  }
}

static std::string get_unique_id(const global_value &G) {
  if (isa<str_val>(G)) {
    return get_unique_id(static_cast<const str_val &>(G));
  } else {
    assert(isa<function>(G));
    return get_unique_id(static_cast<const function &>(G));
  }
}

static std::string get_unique_id(const basic_block &bb) {
  if (bb.has_parent()) {
    return get_numbered_name(bb, *bb.get_parent(), "label");
  } else if (bb.has_name()) {
    return bb.get_name();
  } else {
    return "label";
  }
}

static std::string get_unique_id(const instruction &inst) {
  if (inst.has_parent()) {
    if (inst.get_parent()->has_parent()) {
      unsigned same_name = 0;
      if (!inst.has_name())
        same_name += 1;
      const auto &F = *inst.get_parent()->get_parent();
      for (const auto *bb : F) {
        if (bb == inst.get_parent())
          break;
        for (const auto *oinst : *bb) {
          if (oinst->get_name() == inst.get_name())
            same_name += 1;
        }
      }
      assert(inst.get_parent()->contains(inst));
      for (const auto *oinst : *inst.get_parent()) {
        if (oinst == &inst)
          break;
        if (oinst->get_name() == inst.get_name())
          same_name += 1;
      }
      if (same_name == 0)
        return "%" + inst.get_name();
      return "%" + inst.get_name() + std::to_string(same_name - 1);
    } else {
      return "%" + get_numbered_name(inst, *inst.get_parent(), "");
    }
  } else if (inst.has_name()) {
    return "%" + inst.get_name();
  } else {
    return "%";
  }
}

static std::string get_unique_id(const value &val) {
  if (isa<instruction>(val))
    return get_unique_id(static_cast<const instruction &>(val));
  else if (isa<basic_block>(val))
    return get_unique_id(static_cast<const basic_block &>(val));
  else if (isa<constant>(val))
    return to_string(static_cast<const constant &>(val));
  else if (isa<global_value>(val))
    return get_unique_id(static_cast<const global_value &>(val));
  unreachable("Encountered unsupported value type");
}

void ir_printer::print(std::ostream &stream, const value &val,
                       const ir_print_policy policy) {
  if (isa<instruction>(val))
    print(stream, static_cast<const instruction &>(val), policy);
  else if (isa<basic_block>(val))
    print(stream, static_cast<const basic_block &>(val), policy);
  else if (isa<constant>(val))
    print(stream, static_cast<const constant &>(val), policy);
  else if (isa<global_value>(val))
    print(stream, static_cast<const global_value &>(val), policy);
  unreachable("Encountered unsupported value type");
}
void ir_printer::print(std::ostream &stream, const module &mod,
                       const ir_print_policy policy) {
  indent(stream, policy);
  stream << "; global values\n";
  for (const auto *str : mod.get_strs()) {
    print(stream, *str, policy);
  }
  indent(stream, policy);
  stream << "\n; functions\n";
  for (auto *F : mod.get_functions()) {
    print(stream, *F, policy);
  }
}
void ir_printer::print(std::ostream &stream, const constant &c,
                       const ir_print_policy policy) {
  indent(stream, policy);
  stream << to_string(c);
}
void ir_printer::print(std::ostream &stream, const global_value &G,
                       const ir_print_policy policy) {
  if (isa<str_val>(G)) {
    return print(stream, static_cast<const str_val &>(G), policy);
  } else {
    assert(isa<function>(G));
    return print(stream, static_cast<const function &>(G), policy);
  }
}
void ir_printer::print(std::ostream &stream, const function &fun,
                       const ir_print_policy policy) {
  indent(stream, policy);
  stream << "function " << get_unique_id(fun) << " {\n";
  for (auto *block : fun) {
    print(stream, *block, policy.one_level_deeper());
  }
  indent(stream, policy);
  stream << "}\n";
}
void ir_printer::print(std::ostream &stream, const str_val &str,
                       const ir_print_policy policy) {
  stream << "string " << get_unique_id(str) << ": \"" << str.get_value()
         << "\"\n";
}
void ir_printer::print(std::ostream &stream, const basic_block &bb,
                       const ir_print_policy policy) {
  indent(stream, policy);
  stream << get_unique_id(bb) << ":\n";
  for (const auto *inst : bb) {
    print(stream, *inst, policy.one_level_deeper());
  }
}

/// Instruction visitor
// FIXME move this somewhere public (i.e. header)
struct inst_visitor_base {
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  void accept(const NAME##_inst &) {}
#include "jnsn/ir/instructions.def"
};

template <class impl> struct inst_visitor : public inst_visitor_base {
  void visit(const instruction &inst) {
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  if (isa<NAME##_inst>(inst)) {                                                \
    static_cast<impl *>(this)->accept(static_cast<const NAME##_inst &>(inst)); \
  }
#include "jnsn/ir/instructions.def"
  }
};

void ir_printer::print(std::ostream &stream, const instruction &inst,
                       const ir_print_policy policy) {
  struct inst_printer : public inst_visitor<inst_printer> {
    std::ostream &stream;
    const ir_print_policy &policy;
    inst_printer(std::ostream &stream, const ir_print_policy &policy)
        : stream(stream), policy(policy) {}
#define INSTRUCTION(NAME, ARGUMENTS, PROPERTIES, RET)                          \
  void accept(const NAME##_inst &inst) {                                       \
    indent(stream, policy);                                                    \
    if (!isa<void_type>(inst.get_type())) {                                    \
      stream << get_unique_id(inst) << " = ";                                  \
    }                                                                          \
    stream << #NAME << " ";                                                    \
    if (inst.arg_begin() != inst.arg_end()) {                                  \
      auto It = inst.arg_begin();                                              \
      stream << get_unique_id(**It);                                           \
      while (++It != inst.arg_end()) {                                         \
        stream << ", " << get_unique_id(**It);                                 \
      }                                                                        \
    }                                                                          \
    stream << "\n";                                                            \
  }
#include "jnsn/ir/instructions.def"
  };
  auto print = inst_printer(stream, policy);
  print.visit(inst);
}
