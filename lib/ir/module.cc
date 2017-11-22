#include "parsing/ir/module.h"
#include <iostream>

namespace parsing {
void module::print(std::ostream &stream) const {
  stream << "; global values\n";
  for (auto *str : strs) {
    stream << "string " << ctx.get_unique_id(*str) << ": \"";
    str->print(stream);
    stream << "\"\n";
  }
  stream << "\n; functions\n";
  for (auto *F : functions) {
    F->print(stream);
  }
}

std::ostream &operator<<(std::ostream &stream, const module &mod) {
  mod.print(stream);
  return stream;
}

} // namespace parsing