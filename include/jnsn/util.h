#ifndef JNSN_UTIL_H
#define JNSN_UTIL_H
#include <cstdlib>
#include <iostream>

namespace jnsn {
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
} // namespace jnsn

#ifndef NDEBUG
#define unreachable(msg) ::jnsn::unreachable_internal(msg, __FILE__, __LINE__)
#else
#define unreachable(msg) ::jnsn::unreachable_internal()
#endif // NDEBUG
#endif // JNSN_UTIL_H
