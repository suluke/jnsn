#include "parsing/lexer.h"

using namespace std;
using namespace parsing;

int main(int argc, char **argv) {
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
    } while (!std::holds_alternative<std::monostate>(res));
  } while(!error);
  return 0;
}
