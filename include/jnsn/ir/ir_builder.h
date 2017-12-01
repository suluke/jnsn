#ifndef JNSN_IR_BUILDER_H
#define JNSN_IR_BUILDER_H
#include "jnsn/source_location.h"
#include <memory>
namespace jnsn {

struct module_node;
class module;
class ir_context;
struct semantic_error {
  std::string msg;
  source_location loc;
  semantic_error(std::string msg, source_location loc)
      : msg(std::move(msg)), loc(loc) {}
  friend std::ostream &operator<<(std::ostream &stream,
                                  const semantic_error &err) {
    return stream << err.msg;
  }
};
std::variant<semantic_error, std::unique_ptr<module>>
ast_to_ir(const module_node &, ir_context &ctx);

} // namespace jnsn
#endif // JNSN_IR_BUILDER_H
