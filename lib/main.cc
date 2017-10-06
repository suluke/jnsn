#include "parsing/lexer.h"

using namespace std;
using namespace parsing;

int main(int argc, char **argv) {
  struct lex_visitor {
    bool operator()(lexer_error err) {
      cout << err.msg << ' ' << '(' << err.loc << ')' << '\n';
      return false;
    }
    bool operator()(std::monostate eof) {
      cout << "EOF\n";
      return false;
    }
    bool operator()(token T) {
      cout << "Token\n";
      if (!T.text.empty()) {
        cout << T.text << '\n';
      }
      return true;
    }
  } visitor;
  do {
    cout << "Enter text:\n";
    cin_line_lexer lexer;
    if (!std::visit(visitor, lexer.next())) {
      break;
    }
  } while(true);
  return 0;
}
