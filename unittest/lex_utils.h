#include "parsing/lexer.h"
///
///
class constant_string_lexer : public parsing::lexer_base {
  const char *it;
  read_t read_unit() override {
    read_t res;
    if (*it != '\0') {
      res = {*it};
      ++it;
    }
    return res;
  }

public:
  void set_text(const char *text) {
    reset();
    it = text;
  }
};