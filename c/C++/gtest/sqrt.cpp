#include <cmath>

constexpr double squareRoot(const double a) {
  return a < 0.0 ? -1.0 : std::sqrt(a);
}
