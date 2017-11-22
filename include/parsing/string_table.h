#ifndef PARSING_STRING_TABLE_H
#define PARSING_STRING_TABLE_H
#include <string>
#include <string_view>
#include <set> // don't use unordered_set because elements might relocate

namespace parsing {

class string_table;
class string_table_entry {
  friend class string_table;
  std::string_view text;
  string_table_entry(std::string_view text) : text(std::move(text)) {}

public:
  string_table_entry() = default;
  string_table_entry(const string_table_entry &) = default;
  string_table_entry(string_table_entry &&) = default;
  string_table_entry &operator=(const string_table_entry &) = default;
  string_table_entry &operator=(string_table_entry &&) = default;
  operator std::string_view() const { return text; }
  bool empty() const { return text.empty(); }
  const char *data() const { return text.data(); }
  std::string_view::size_type size() const noexcept { return text.size(); }
  std::string_view::iterator begin() const { return text.begin(); }
  std::string_view::iterator end() const { return text.end(); }
  bool operator==(const string_table_entry &o) const { return text == o.text; }
  const std::string_view *operator->() const { return &text; }
  friend std::ostream &operator<<(std::ostream &stream,
                                  const string_table_entry &entry) {
    return stream << entry.text;
  }
  friend bool operator==(const string_table_entry &entry,
                         const std::string_view &view) {
    return entry.text == view;
  }
  friend bool operator==(const std::string_view &view,
                         const string_table_entry &entry) {
    return entry.text == view;
  }
};

class string_table {
private:
  using container = std::set<std::string>;
  container table;

public:
  using entry = string_table_entry;
  using iterator = container::iterator;
  using const_iterator = container::const_iterator;
  entry get_handle(std::string s);
  iterator begin() { return table.begin(); }
  const_iterator begin() const { return table.begin(); }
  iterator end() { return table.end(); }
  const_iterator end() const { return table.end(); }
};

} // namespace parsing
#endif // PARSING_STRING_TABLE_H