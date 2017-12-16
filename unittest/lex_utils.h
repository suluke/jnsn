#ifndef JNSN_UNITTEST_LEX_UTILS_H
#define JNSN_UNITTEST_LEX_UTILS_H
#include "jnsn/js/lexer.h"
namespace jnsn {
///
///
class constant_string_lexer : public lexer_base {
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
} // namespace jnsn
#endif // JNSN_UNITTEST_LEX_UTILS_H
