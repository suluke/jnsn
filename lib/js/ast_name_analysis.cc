#include "jnsn/js/ast_name_analysis.h"

using namespace jnsn;

bool default_builtins::contains(string_table_entry name) {
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects
  static std::set<std::string_view> builtins{
      "Array",
      "ArrayBuffer",
      "AsyncFunction",
      "Atomics",
      "Boolean",
      "DataView",
      "Date",
      "decodeURI",
      "decodeURIComponent",
      "encodeURI",
      "encodeURIComponent",
      "Error",
      "escape",
      "eval",
      "EvalError",
      "Float32Array",
      "Float64Array",
      "Function",
      "Generator",
      "GeneratorFunction",
      "Infinity",
      "Int16Array",
      "Int32Array",
      "Int8Array",
      "InternalError",
      "Intl",
      "isFinite",
      "isNaN",
      "JSON",
      "Map",
      "Math",
      "NaN",
      "null",
      "Number",
      "Object",
      "parseFloat",
      "parseInt",
      "Promise",
      "Proxy",
      "RangeError",
      "ReferenceError",
      "Reflect",
      "RegExp",
      "Set",
      "SharedArrayBuffer",
      "SIMD",
      "String",
      "Symbol",
      "SyntaxError",
      "TypeError",
      "Uint16Array",
      "Uint32Array",
      "Uint8Array",
      "Uint8ClampedArray",
      "undefined",
      "unescape",
      "URIError",
      "WeakMap",
      "WeakSet",
      "WebAssembly",
  };
  return builtins.count(name);
}

ast_name_analysis_base::result
ast_name_analysis_base::run(const module_node *node) {
  return {};
}
ast_name_analysis_base::result
ast_name_analysis_base::run(const function_stmt_node *node) {
  return {};
}
ast_name_analysis_base::result
ast_name_analysis_base::run(const function_expr_node *node) {
  return {};
}
ast_name_analysis_base::result
ast_name_analysis_base::run(const arrow_function_node *node) {
  return {};
}
ast_name_analysis_base::result
ast_name_analysis_base::run(const class_func_node *node) {
  return {};
}
