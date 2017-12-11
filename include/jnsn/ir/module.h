#ifndef JNSN_IR_MODULE_H
#define JNSN_IR_MODULE_H
#include "jnsn/ir/ir_context.h"
#include <set>

namespace jnsn {

class module {
  friend struct ir_context;
  friend class global_value;
  ir_context &ctx;
  function *entry;
  std::set<function *> functions;
  struct str_val_less {
    bool operator()(const str_val *s1, const str_val *s2) const {
      return std::less<std::string_view>()(s1->val, s2->val);
    }
  };
  std::set<str_val *, str_val_less> strs;

  // unique identifier support
  std::map<const global_value *, std::string> global_names;
  std::string get_unique_id(const global_value &);

public:
  module(ir_context &ctx) : ctx(ctx), entry(ctx.make_function()) {
    entry->set_name("!__module_entry__");
    ctx.insert_function_into(*this, *entry);
  }
  void print(std::ostream &);
  friend std::ostream &operator<<(std::ostream &stream, module &mod);
  function *get_entry() { return entry; }
  function *get_function_by_name(std::string name);

  str_val *get_str_val(std::string val) {
    auto handle = ctx.internalize_string(val);
    str_val tester(handle, ctx);
    if (strs.count(&tester)) {
      return *strs.find(&tester);
    }
    auto *str = ctx.make_str_val(std::move(val));
    str->parent = this;
    strs.emplace(str);
    return str;
  }
};

} // namespace jnsn
#endif // JNSN_IR_MODULE_H
