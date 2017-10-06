#ifndef PARSING_SOURCE_LOCATION_H
#define PARSING_SOURCE_LOCATION_H
#include <cctype>
#include <sstream>

namespace parsing {

/// FIXME duplicate type alias
using unit_t = char;

class source_location {
  size_t row, col, next_row, next_col;
public:
  source_location() : row(0), col(0), next_row(0), next_col(0) {}
  source_location(const source_location &) = default;
  source_location(source_location &&) = default;
  source_location &operator=(const source_location &) = default;
  source_location &operator=(source_location &&) = default;

  size_t get_row() { return row; }
  size_t get_col() { return col; }

  void advance(unit_t u) {
    row = next_row;
    col = next_col;
    if (u == '\n') {
      ++next_row;
      next_col = 0;
    } else if (u == '\r') {
      // do nothing
    } else {
      ++next_col;
    }
  }
  /// FIXME this will not correctly restore the col value
  void rewind(unit_t u) {
    next_row = row;
    next_col = col;
    if (u == '\n') {
      --row;
    } else if (u == '\r') {
      // do nothing
    } else {
      --col;
    }
  }

  friend std::ostream &operator<<(std::ostream &stream, const source_location &loc) {
    std::stringstream ss;
    ss << "line: " << loc.row << ", column: " << loc.col;
    stream << ss.str();
    return stream;
  }
};

} // namespace parsing

#endif
