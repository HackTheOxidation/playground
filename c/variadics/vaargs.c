#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

int sum(int argc, ...) {
  va_list args;
  va_start(args, argc);

  int s = 0;
  for (int i = 0; i < argc; ++i) {
    s += va_arg(args, int);
  }

  va_end(args);

  return s;
}

int main(void) {
  int total = sum(3, 1, 2, 3);
  printf("The sum is: %d\n", total);

  return 0;
}
