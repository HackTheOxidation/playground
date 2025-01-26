#include <algorithm>
#include <compare>
#include <concepts>
#include <cstdlib>
#include <functional>
#include <numeric>
#include <ranges>
#include <vector>
#include <type_traits>

template<typename Collection>
constexpr auto qsort(const Collection collection) noexcept {
  if (std::ranges::empty(collection)) {
    return collection;
  } else {
    const auto pivot = std::ranges::views::take(collection, 1);
    const auto smaller = std::ranges::views::filter(collection, [&](auto& n) { return n <= pivot.front(); });
    const auto larger = std::ranges::views::filter(collection, [&](auto& n) { return n > pivot.front(); });
    return std::ranges::views::concat(qsort(smaller), pivot, qsort(larger));
  }
}

int main(void) {
  std::vector<int> numbers = {9, 3, 7, 1, 5 };
  const auto sorted_numbers = qsort(numbers);

  return 0;
}
