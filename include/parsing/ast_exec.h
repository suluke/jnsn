#ifndef PARSING_AST_EXECUTOR_H
#define PARSING_AST_EXECUTOR_H
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

namespace parsing {

struct module_node;
struct exec_value_base {
  virtual ~exec_value_base() = default;
  virtual void print(std::ostream &) const = 0;
  friend std::ostream &operator<<(std::ostream &stream, exec_value_base &val) {
    val.print(stream);
    return stream;
  }
};
struct undefined_value : public exec_value_base {
  void print(std::ostream &stream) const override { stream << "undefined"; };
};
struct null_value : public exec_value_base {
  void print(std::ostream &stream) const override { stream << "null"; };
};
struct bool_value : public exec_value_base {
  /*const*/ bool value;
  bool_value(bool value) : value(value) {}
  void print(std::ostream &stream) const override {
    stream << (value ? "true" : "false");
  };
};
struct number_value : public exec_value_base {
  /*const*/ double value;
  number_value(double value) : value(value) {}
  void print(std::ostream &stream) const override { stream << value; }
};
struct string_value : public exec_value_base {
  /*const*/ std::string_view value;
  string_value(std::string_view value) : value(value) {}
  void print(std::ostream &stream) const override {
    stream << "\"" << value << "\"";
  }
};
/// Heap values are values that are only passed by reference and need to
/// be garbage collected
struct heap_value_base : public exec_value_base {
  bool reachable = false;
};
struct ref_value : public exec_value_base {
  heap_value_base *value /*const*/;
  ref_value(heap_value_base *value) : value(value) {}
  void print(std::ostream &stream) const override { value->print(stream); }
};

class exec_value : public exec_value_base {
  using val_t = std::variant<undefined_value, null_value, bool_value,
                             number_value, string_value, ref_value>;
  val_t val;
  const exec_value_base &upcast_content() const {
    return std::visit(
        [](const auto &val) -> const exec_value_base & { return val; }, val);
  }

public:
  exec_value(undefined_value val) : val(val) {}
  exec_value(null_value val) : val(val) {}
  exec_value(bool_value val) : val(val) {}
  exec_value(number_value val) : val(val) {}
  exec_value(string_value val) : val(val) {}
  exec_value(ref_value val) : val(val) {}

  exec_value(const exec_value &) = default;
  exec_value(exec_value &&) = default;
  exec_value &operator=(const exec_value &) = default;

  void print(std::ostream &stream) const override { upcast_content().print(stream); }
  template <typename T> T &get() { return std::get<T>(val); }
  template <typename T> friend bool isa(const exec_value &);
};

struct array_value : public heap_value_base {
  std::vector<exec_value> elms;
  void print(std::ostream &stream) const override;
};
struct object_value : public heap_value_base {
  std::unordered_map<std::string, exec_value> entries;
  void print(std::ostream &stream) const override;
};
struct scope_value : public heap_value_base {
  scope_value *parent;
  enum scope_kind { FUNCTION, BLOCK } kind;
  std::unordered_map<std::string, exec_value> entries;
  scope_value(scope_value *parent = nullptr, scope_kind kind = FUNCTION) : parent(parent), kind(kind) {}
  void print(std::ostream &stream) const override { stream << "<scope>"; };
};
struct function_value : public heap_value_base {
  scope_value *outer_scope;
  enum scope_kind { FUNCTION, BLOCK } kind;
  std::unordered_map<std::string, exec_value> entries;
  void print(std::ostream &stream) const override { stream << "<scope>"; };
};
using heap_value = std::variant<array_value, object_value>;

template <typename T> bool isa(const exec_value &val) {
  return std::holds_alternative<T>(val.val);
}

struct exec_error {
  std::string msg;
  friend std::ostream &operator<<(std::ostream &, const exec_error);
};

class exec_vm {
  std::vector<heap_value> heap;
  std::vector<std::vector<heap_value>::size_type> free_pool;
  heap_value &find_slot();
public:
  exec_value lookup(std::string_view identifier);
  array_value &alloc_array();
  object_value &alloc_object();
};

class ast_executor {
  exec_vm vm;

public:
  using result = std::variant<exec_error, exec_value>;
  result execute(module_node &ast);
};

} // namespace parsing
#endif // PARSING_AST_EXECUTOR_H