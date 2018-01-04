#ifndef JNSN_IR_PRINTER_H
#define JNSN_IR_PRINTER_H
#include <ostream>
namespace jnsn {

class value;
class module;
class global_value;
class function;
class str_val;

struct constant;
class c_bool_val;
class c_num_val;
class undefined_val;
class null_val;

class basic_block;
class instruction;

// TODO we can add constexpr to a lot of member functions here
struct ir_print_policy {
  enum INDENT_TYPE { TABS, SPACES } indent_type = SPACES;
  unsigned indent_begin = 0;
  unsigned indent_width = 2;

  ir_print_policy one_level_deeper() const {
    return ir_print_policy{indent_type, indent_begin + indent_width,
                           indent_width};
  }
};

class ir_printer {
public:
  static void print(std::ostream &, const value &val,
                    const ir_print_policy = {});
  static void print(std::ostream &, const module &mod,
                    const ir_print_policy = {});
  static void print(std::ostream &, const global_value &val,
                    const ir_print_policy = {});
  static void print(std::ostream &, const function &fun,
                    const ir_print_policy = {});
  static void print(std::ostream &, const str_val &str,
                    const ir_print_policy = {});
  static void print(std::ostream &, const constant &c,
                    const ir_print_policy = {});
  static void print(std::ostream &, const basic_block &bb,
                    const ir_print_policy = {});
  static void print(std::ostream &, const instruction &inst,
                    const ir_print_policy = {});
};

} // namespace jnsn
#endif // JNSN_IR_PRINTER_H
