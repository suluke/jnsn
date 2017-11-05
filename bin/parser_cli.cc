#include "parsing/parser.h"

using namespace std;
using namespace parsing;

static void parser_cli() {
  using ast_root = parser_base::ast_root;
  bool error = false;
  do {
    cout << "Enter text:\n";
    cin_line_parser parser;
    parser_base::result res = parser.parse();
    if (std::holds_alternative<ast_root>(res)) {
      auto mod = std::get<ast_root>(res);
      cout << mod;
    } else if (std::holds_alternative<parser_error>(res)) {
      auto err = std::get<parser_error>(res);
      cout << "ERROR: " << err << '\n';
      error = true;
    }
  } while(!error);
}

int main(int argc, char **argv) {
  parser_cli();
  return 0;
}
