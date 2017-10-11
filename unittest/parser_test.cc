#include "gtest/gtest.h"

using namespace parsing;

class parser_test : public ::testing::Test {
protected:
  constant_string_lexer lexer;
};