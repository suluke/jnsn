#include "jnsn/ir/module.h"
#include "jnsn/ir/printer.h"
#include "jnsn/util.h"
#include <cassert>
#include <iostream>
#include <sstream>

using namespace jnsn;

function *module::get_function_by_name(std::string name) {
  for (auto *F : functions) {
    if (F->get_name() == name) {
      return F;
    }
  }
  return nullptr;
}

str_val *module::get_str_val(std::string val) {
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

namespace jnsn {
std::ostream &operator<<(std::ostream &stream, const module &mod) {
  ir_printer::print(stream, mod);
  return stream;
}

} // namespace jnsn
