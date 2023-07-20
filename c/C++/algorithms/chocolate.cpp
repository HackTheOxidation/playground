#include <algorithm>
#include <array>
#include <functional>
#include <iostream>
#include <map>
#include <optional>
#include <utility>

using Coord = std::pair<std::size_t, std::size_t>;
using Profits = std::map<Coord, int>;
using ProfitFun = std::function<int(std::size_t, std::size_t)>;
template<std::size_t N> using Pricelist = const std::array<int, N>;

auto maximum_profit(const Profits &profits) -> Coord {
  auto [k, _] = *std::max_element(profits.begin(), profits.end());
  return k;
}

template<std::size_t N>
auto chocolate_profit(const std::size_t x, Pricelist<N> s, Pricelist<N> b) -> Coord {
  ProfitFun profit = [&, s, b](const std::size_t i, const std::size_t j) {
    return b[j] - s[i] - (int(j) - int(i)) * 100;
  };

  Profits profits;

  std::function<void(std::size_t, std::size_t)> recurse =
    [&, x](const std::size_t i, const std::size_t j) mutable {
      if (j == N - 1) {
        profits[std::pair(i, j)] = profit(i, j);
      } else if (i == x) {
        profits[std::pair(i, j)] = profit(i, j);
        recurse(i, j + 1);
      } else {
        recurse(i + 1, j);
        profits[std::pair(i, j)] = profit(i, j);
        recurse(i, j + 1);
      }
  };

  recurse(0, x + 1);

  return maximum_profit(profits);
}

auto main() -> int {
  constexpr Pricelist<5> s = {600, 350, 400, 400, 500};
  constexpr Pricelist<5> b = {500, 450, 300, 400, 600};
  constexpr std::size_t x = 2;

  static_assert(x < s.size());

  auto [i, j] = chocolate_profit(x, s, b);
  std::cout << "(" << i << ", " << j << ")\n";

  return 0;
}
