#include "parsing/parser.h"

using namespace parsing;

parser_base::result parser_base::parse() {
  nodes.clear();
  module = nodes.make_module();

  return &*module;
}
