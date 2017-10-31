#include <cstdlib>
#include <iostream>

namespace parsing {
[[noreturn]] inline void unreachable_internal(const char *msg = nullptr,
                                              const char *file = nullptr,
                                              unsigned line = 0) {
  if (msg) {
    std::cerr << msg << "\n";
  }
  std::cerr << "UNREACHABLE executed";
  if (file) {
    std::cerr << " at " << file << ":" << line;
  }
  std::cerr << "!\n";
  std::abort();
}
} // namespace parsing

#ifndef NDEBUG
#define unreachable(msg)                                                       \
  ::parsing::unreachable_internal(msg, __FILE__, __LINE__)
#else
#define unreachable(msg) ::parsing::unreachable_internal()
#endif