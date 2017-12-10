#include "jnsn/ir/module.h"
#include "jnsn/util.h"
#include <sstream>
#include <iostream>

using namespace jnsn;

function *module::get_function_by_name(std::string name) {
  for (auto *F : functions) {
    if (F->get_name() == name) {
      return F;
    }
  }
  return nullptr;
}

void module::print(std::ostream &stream) {
  stream << "; global values\n";
  for (auto *str : strs) {
    stream << "string " << get_unique_id(*str) << ": \"";
    str->print(stream);
    stream << "\"\n";
  }
  stream << "\n; functions\n";
  for (auto *F : functions) {
    F->print(stream);
  }
}
std::string module::get_unique_id(const global_value &G) {
  if (global_names.count(&G)) {
    return global_names[&G];
  }
  //~ // TODO this still needs improvements
  if (isa<c_str_val>(G)) {
    return "str";
  }
  return G.get_name();
}

namespace jnsn {
std::ostream &operator<<(std::ostream &stream, module &mod) {
  mod.print(stream);
  return stream;
}
} // namespace jnsn
