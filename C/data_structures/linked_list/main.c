#include "linkedlist.h"
#include <stdio.h>

int main() {
  node *n = create_linkedlist(1);

  insert_head(&n, 2);
  insert_head(&n, 3);
  print_linkedlist(n);

  remove_head(&n);
  print_linkedlist(n);

  deallocate(&n);
  return 0;
}
