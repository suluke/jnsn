#ifndef JNSN_IR_MODULE_H
#define JNSN_IR_MODULE_H
#include "jnsn/ir/ir_context.h"
#include <set>

namespace jnsn {

class module {
  friend struct ir_context;
  friend class global_value;
  ir_context &ctx;
  function *const entry;
  using func_set = std::set<function *>;
  func_set functions;
  struct str_val_less {
    bool operator()(const str_val *s1, const str_val *s2) const {
      return std::less<std::string_view>()(s1->val, s2->val);
    }
  };
  using str_set = std::set<str_val *, str_val_less>;
  str_set strs;

public:
  module(ir_context &ctx) : ctx(ctx), entry(ctx.make_function()) {
    entry->set_name("!__module_entry__"); // exclamation mark signals special
                                          // compiler-generated function
    ctx.insert_function_into(*this, *entry);
  }
  friend std::ostream &operator<<(std::ostream &stream, const module &mod);
  function *get_entry() { return entry; }
  function *get_function_by_name(std::string name);
  ir_context &get_context() { return ctx; }
  str_val *get_str_val(std::string val);
  const str_set &get_strs() const { return strs; }
  const func_set &get_functions() const { return functions; }
};

} // namespace jnsn
#endif // JNSN_IR_MODULE_H
