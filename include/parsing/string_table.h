#ifndef PARSING_STRING_TABLE_H
#define PARSING_STRING_TABLE_H
#include <string_view>
#include <unordered_set>

namespace parsing {

class string_table {
  std::unordered_set<std::string> table;
public:
  using entry = std::string_view;
  entry get_handle(std::string s) {
    auto it_ins = table.insert(std::move(s));
    auto it = it_ins.first;
    return { it->data(), it->size() };
  }
};

} // namespace parsing

#endif
