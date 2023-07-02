#include <array>
#include <functional>
#include <iostream>
#include <map>
#include <optional>
#include <vector>
#include <utility>


using Coord = std::pair<std::size_t, std::size_t>;
using Profits = std::map<Coord, int>;
using ProfitFun = std::function<int(std::size_t, std::size_t)>;
template<std::size_t N> using Pricelist = std::array<int, N>;

auto maximum_profit(const Profits &profits) -> std::optional<Coord> {
  std::optional<std::pair<Coord, int>> max = std::nullopt;

  for (auto [k, v] : profits) {
    if (!max.has_value()) {
      max = std::make_pair(k, v);
      continue;
    }

    if (max.has_value() && v > max.value().second)
      max = std::make_pair(k, v);
  }

  return max ? std::make_optional(max.value().first) : std::nullopt;
}

template<std::size_t N>
auto chocolate_profit(std::size_t x, Pricelist<N> s, Pricelist<N> b) -> std::optional<Coord> {
  if (x >= N)
    return std::nullopt;

  ProfitFun profit = [&, s, b](std::size_t i, std::size_t j) {
    return b[j] - s[i] - (int(j) - int(i)) * 100;
  };

  Profits profits;

  std::function<void(std::size_t, std::size_t)> recurse =
    [&, x](std::size_t i, std::size_t j) mutable {
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
  Pricelist<5> s = {600, 350, 400, 400, 500};
  Pricelist<5> b = {500, 450, 300, 400, 600};

  auto best = chocolate_profit(2, s, b);

  if (best.has_value()) {
    auto [i, j] = best.value();
    std::cout << "(" << i << ", " << j << ")\n";
  } else {
    std::cout << "Maximum profit could not be determined.\n";
  }

  return 0;
}
