#include <iostream>
#include "reference_counting.hpp"

using namespace std;

int main() {
  int* my_int = new int(1);
  RC<int> ref(my_int);
  RC<int> copy(ref);

  return 0;
}
