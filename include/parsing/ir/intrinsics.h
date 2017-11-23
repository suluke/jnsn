#ifndef PARSING_IR_INTRINSICS_H
#define PARSING_IR_INTRINSICS_H
namespace parsing {

enum class intrinsic {
#define INTRINSIC(NAME, ARGUMENTS, RET) NAME,
#include "parsing/ir/intrinsics.def"
};

}
#endif // PARSING_IR_INTRINSICS_H