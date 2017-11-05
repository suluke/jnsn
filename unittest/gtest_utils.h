// stackoverflow.com/questions/16491675/how-to-send-custom-message-in-google-c-testing-framework
#include <sstream>

namespace testing {
namespace internal {
enum GTestColor { COLOR_DEFAULT, COLOR_RED, COLOR_GREEN, COLOR_YELLOW };

extern void ColoredPrintf(GTestColor color, const char *fmt, ...);
} // namespace internal
} // namespace testing