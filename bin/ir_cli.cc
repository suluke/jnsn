#include "jnsn/ir/module.h"
#include "jnsn/ir/ir_builder.h"
#include "jnsn/parser.h"

using namespace std;
using namespace jnsn;

void ir_cli() {
  while (true) {
    cout << "Enter code:\n";
    cin_line_parser parser;
    auto res = parser.parse();
    if (std::holds_alternative<module_node *>(res)) {
      auto *mod = std::get<module_node *>(res);
      ir_context ctx;
      auto res = build_ir_from_ast(*mod, ctx);
      if (std::holds_alternative<semantic_error>(res)) {
        cout << "SEMANTIC ERROR: " << std::get<semantic_error>(res) << "\n";
        break;
      } else {
        auto &ir = std::get<std::unique_ptr<module>>(res);
        cout << "; module\n";
        cout << *ir << "\n";
      }
    } else if (std::holds_alternative<parser_error>(res)) {
      auto err = std::get<parser_error>(res);
      cout << "PARSER ERROR: " << err << '\n';
      break;
    }
  }
}

int main(int argc, char **argv) {
  ir_cli();
  return 0;
}
