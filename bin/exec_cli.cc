#include "parsing/ast_exec.h"
#include "parsing/parser.h"

using namespace std;
using namespace parsing;

void exec_cli() {
  ast_executor exec;
  while (true) {
    cout << "Enter text:\n";
    cin_line_parser parser;
    parser_base::result res = parser.parse();
    if (std::holds_alternative<module_node *>(res)) {
      auto *mod = std::get<module_node *>(res);
      auto res = exec.execute(*mod);
      if (std::holds_alternative<exec_error>(res)) {
        cout << std::get<exec_error>(res) << "\n";
        break;
      } else {
        cout << std::get<exec_value>(res) << "\n";
      }
    } else if (std::holds_alternative<parser_error>(res)) {
      auto err = std::get<parser_error>(res);
      cout << "ERROR: " << err << '\n';
      break;
    }
  }
}

int main(int argc, char **argv) {
  exec_cli();
  return 0;
}