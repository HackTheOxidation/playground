#include <algorithm>
#include <concepts>
#include <functional>
#include <iostream>
#include <numeric>
#include <ranges>
#include <type_traits>


template<typename Integer>
requires std::integral<Integer>
constexpr auto fib(const Integer n) noexcept {
  const auto numbers = std::views::iota(1, n + 1);
  return std::reduce(numbers.begin(), numbers.end(), 1, std::multiplies{});
}

int main(void) {
  constexpr auto result = fib(5);

  std::cout << result << '\n';

  return 0;
}
