#ifndef JNSN_IR_MODULE_H
#define JNSN_IR_MODULE_H
#include "jnsn/ir/ir_context.h"
#include <set>

namespace jnsn {

class module {
  friend struct ir_context;
  friend class global_value;
  std::set<function *> functions;
  std::set<c_str_val *> strs;
  ir_context &ctx;
  // unique identifier support
  std::map<const global_value *, std::string> global_names;
  std::string get_unique_id(const global_value &);

public:
  module(ir_context &ctx) : ctx(ctx) {}
  void print(std::ostream &);
  friend std::ostream &operator<<(std::ostream &stream, module &mod);

  void add_string_constant(c_str_val &str) { strs.emplace(&str); }
};

} // namespace jnsn
#endif // JNSN_IR_MODULE_H
