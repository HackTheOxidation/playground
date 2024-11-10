#include "reference_counting.hpp"
#include <iostream>

using namespace std;

int main() {
  int* my_int = new int(1);
  RC<int> ref(my_int);
  RC<int> copy(ref);

  auto from_static = RC<int>::make_rc(1);

  return 0;
}
