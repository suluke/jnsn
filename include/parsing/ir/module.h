#ifndef PARSING_IR_MODULE_H
#define PARSING_IR_MODULE_H
#include "parsing/ir/ir_context.h"
#include <set>

namespace parsing {

class module {
  friend struct ir_context;
  std::set<function *> functions;
  std::set<c_str_val *> strs;
  ir_context &ctx;

public:
  module(ir_context &ctx) : ctx(ctx) {}
  void print(std::ostream &) const;
  friend std::ostream &operator<<(std::ostream &stream, const module &mod);
};

} // namespace parsing
#endif // PARSING_IR_MODULE_H