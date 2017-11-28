#include "parsing/ir/module.h"
#include "parsing/util.h"
#include <sstream>
#include <iostream>

using namespace parsing;

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

namespace parsing {
std::ostream &operator<<(std::ostream &stream, module &mod) {
  mod.print(stream);
  return stream;
}
} // namespace parsing