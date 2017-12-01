#ifndef JNSN_IR_INTRINSICS_H
#define JNSN_IR_INTRINSICS_H
namespace jnsn {

enum class intrinsic {
#define INTRINSIC(NAME, ARGUMENTS, RET) NAME,
#include "jnsn/ir/intrinsics.def"
};

} // namespace jnsn
#endif // JNSN_IR_INTRINSICS_H
