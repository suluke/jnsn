#include "jnsn/ir/ir.h"

using namespace jnsn;

bool basic_block::has_terminator() {
  if (instructions.empty())
    return false;
  auto *lastInst = instructions.back();
  if (isa<ret_inst>(lastInst) || isa<br_inst>(lastInst) ||
      isa<cbr_inst>(lastInst) || isa<throw_inst>(lastInst)) {
    return true;
  }
  return false;
}
