#ifndef JNSN_JS_AST_NAME_ANALYSIS_H
#define JNSN_JS_AST_NAME_ANALYSIS_H
#include "jnsn/js/ast.h"

#include <map>
#include <set>
#include <variant>

namespace jnsn {
enum class ast_name_origin {
  UNKNOWN = 0,
  BUILTIN,
  DECLARATION,
  PARAM,
  FUNCTION_STMT,
  CLASS
};

struct ast_name_analysis_result {
  std::map<const ast_node *, std::pair<ast_name_origin, const ast_node *>>
      declarations;
};

class ast_name_analysis_base {
  using result = ast_name_analysis_result;
  virtual bool is_builtin(string_table_entry name) = 0;

protected:
  ast_name_analysis_base() = default;
  result run(const module_node *node);
  result run(const function_stmt_node *node);
  result run(const function_expr_node *node);
  result run(const arrow_function_node *node);
  result run(const class_func_node *node);
};

struct default_builtins {
  static bool contains(string_table_entry name);
};

template <class builtins = default_builtins>
class ast_name_analysis : public ast_name_analysis_base {
  bool is_builtin(string_table_entry name) override {
    return builtins::contains(name);
  }

public:
  using result = ast_name_analysis_result;
  static result run(const module_node *node) {
    return ast_name_analysis().ast_name_analysis_base::run(node);
  }
  static result run(const function_stmt_node *node) {
    return ast_name_analysis().ast_name_analysis_base::run(node);
  }
  static result run(const function_expr_node *node) {
    return ast_name_analysis().ast_name_analysis_base::run(node);
  }
  static result run(const arrow_function_node *node) {
    return ast_name_analysis().ast_name_analysis_base::run(node);
  }
  static result run(const class_func_node *node) {
    return ast_name_analysis().ast_name_analysis_base::run(node);
  }
};

} // namespace jnsn
#endif // JNSN_JS_AST_NAME_ANALYSIS_H
