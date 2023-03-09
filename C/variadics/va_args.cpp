#include <concepts>
#include <iostream>
#include <type_traits>

template<typename T, typename... Args>
requires std::same_as<T, int>
constexpr int sum(const T a, const Args... args) {
  if constexpr(sizeof...(args) > 0)
    return a + sum(args...);
  else
    return a;
}

int main(void) {
  int total = sum(1, 2, 3, 4, 5);
  std::cout << "The sum is: " << total << '\n';
  return 0;
}
