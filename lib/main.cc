#include "parsing/parser.h"

using namespace std;
using namespace parsing;

static void lexer_cli() {
  struct lex_visitor {
    void operator()(lexer_error err) {
      cout << err.msg << ' ' << '(' << err.loc << ')' << '\n';
    }
    void operator()(std::monostate eof) {
      cout << "EOF\n";
    }
    void operator()(token T) {
      cout << T << '\n';
    }
  } visitor;
  bool error = false;
  do {
    cout << "Enter text:\n";
    cin_line_lexer lexer;
    lexer_base::result res;
    do {
      res = lexer.next();
      std::visit(visitor, res);
      if (std::holds_alternative<lexer_error>(res)) {
        error = true;
        break;
      }
    } while (!std::holds_alternative<lexer_base::eof_t>(res));
  } while(!error);
}

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
      cout << "ERROR: " << err.msg << '\n';
      error = true;
    }
  } while(!error);
}

int main(int argc, char **argv) {
  parser_cli();
  return 0;
}
