#ifndef JNSN_JS_AST_ANALYSIS_H
#define JNSN_JS_AST_ANALYSIS_H
#include "jnsn/source_location.h"
#include <string>
#include <vector>

namespace jnsn {
struct ast_node;

struct ast_error {
  ast_error(std::string msg, source_location loc)
      : msg(std::move(msg)), loc(std::move(loc)) {}
  std::string msg;
  source_location loc;
  friend std::ostream &operator<<(std::ostream &stream, const ast_error &error);
};
struct ast_analysis_report {
  std::vector<ast_error> errors;
  operator bool() const { return !errors.empty(); }
  friend std::ostream &operator<<(std::ostream &stream,
                                  const ast_analysis_report &report);
};

ast_analysis_report analyze_js_ast(ast_node &ast);
} // namespace jnsn
#endif // JNSN_JS_AST_ANALYSIS_H
