#ifndef PARSING_AST_EXECUTOR_H
#define PARSING_AST_EXECUTOR_H
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace parsing {

struct module_node;
struct exec_value_base {
  virtual ~exec_value_base() = default;
  virtual void print(std::ostream &) = 0;
  friend std::ostream &operator<<(std::ostream &stream, exec_value_base &val) {
    val.print(stream);
    return stream;
  }
};
struct undefined_value : public exec_value_base {
  void print(std::ostream &stream) override { stream << "undefined"; };
};
struct null_value : public exec_value_base {
  void print(std::ostream &stream) override { stream << "null"; };
};
struct true_value : public exec_value_base {
  void print(std::ostream &stream) override { stream << "true"; };
};
struct false_value : public exec_value_base {
  void print(std::ostream &stream) override { stream << "false"; };
};
struct number_value : public exec_value_base {
  double value;
  number_value(double value) : value(value) {}
  void print(std::ostream &stream) override { stream << value; }
};
struct string_value : public exec_value_base {
  std::string_view value;
  string_value(std::string_view value) : value(value) {}
  void print(std::ostream &stream) override { stream << "\"" << value << "\""; }
};

class exec_value;
struct array_value : public exec_value_base {
  // This ain't even UB! https://stackoverflow.com/a/31047542/1468532
  std::vector<exec_value> elms;
  array_value();
  void print(std::ostream &stream) override;
};

class exec_value : public exec_value_base {
  using val_t =
      std::variant<undefined_value, null_value, true_value, false_value,
                   number_value, string_value, array_value>;
  val_t val;
  exec_value_base &upcast_content();

public:
  exec_value(undefined_value val) : val(val) {}
  exec_value(null_value val) : val(val) {}
  exec_value(true_value val) : val(val) {}
  exec_value(false_value val) : val(val) {}
  exec_value(number_value val) : val(val) {}
  exec_value(string_value val) : val(val) {}
  exec_value(array_value val) : val(val) {}

  exec_value(const exec_value &) = default;
  exec_value(exec_value &&) = default;
  exec_value &operator=(const exec_value &) = default;
  exec_value &operator=(exec_value &&) = default;

  void print(std::ostream &) override;
};

struct exec_error {
  std::string msg;
  friend std::ostream &operator<<(std::ostream &, const exec_error);
};

class exec_vm {
public:
  exec_value lookup(std::string_view identifier);
};

class ast_executor {
  exec_vm vm;

public:
  using result = std::variant<exec_error, exec_value>;
  result execute(module_node &ast);
};

} // namespace parsing
#endif // PARSING_AST_EXECUTOR_H