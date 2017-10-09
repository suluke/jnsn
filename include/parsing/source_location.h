#ifndef PARSING_SOURCE_LOCATION_H
#define PARSING_SOURCE_LOCATION_H
#include <cctype>
#include <sstream>

namespace parsing {

/// FIXME duplicate type alias
using unit_t = char;

class source_location {
  size_t row, col;
public:
  /// Why does row begin at 0? To allow filling the lexer's sliding window
  /// with some initial content plus a newline so it isn't regarded as EOF
  /// and the source_location is still at the correct position after the
  /// sliding window's initial content has been consumed
  source_location() : row(0), col(1) {}
  source_location(const source_location &) = default;
  source_location(source_location &&) = default;
  source_location &operator=(const source_location &) = default;
  source_location &operator=(source_location &&) = default;

  size_t get_row() { return row; }
  size_t get_col() { return col; }

  void advance(unit_t u) {
    if (u == '\n') {
      ++row;
      col = 1;
    } else if (u == '\r') {
      // do nothing
    } else {
      ++col;
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

#endif // PARSING_SOURCE_LOCATION_H
