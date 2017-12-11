#include "jnsn/ir/module.h"
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

void module::print(std::ostream &stream) {
  stream << "; global values\n";
  for (const auto *str : strs) {
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
  assert(G.get_parent() == this);
  if (global_names.count(&G)) {
    return global_names[&G];
  }
  //~ // TODO this still needs improvements
  if (isa<str_val>(G)) {
    const auto *as_cstr = static_cast<const str_val *>(&G);
    // Jeez, I'm so desperate, I'm using const_cast :(
    auto *as_str = const_cast<str_val *>(as_cstr);
    return "str" +
           std::to_string(std::distance(strs.begin(), strs.find(as_str)));
  }
  return G.get_name();
}

namespace jnsn {
std::ostream &operator<<(std::ostream &stream, module &mod) {
  mod.print(stream);
  return stream;
}
} // namespace jnsn
