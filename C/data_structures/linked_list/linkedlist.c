#include "linkedlist.h"
#include <stdio.h>
#include <stdlib.h>

node *create_linkedlist(int value) {
  node *n = NULL;
  insert_head(&n, value);
  return n;
}

void insert_head(node **n, int value) {
  if (*n != NULL) {
    node *temp = malloc(sizeof(node));
    temp->value = value;
    temp->n = *n;
    *n = temp;
  } else {
    *n = malloc(sizeof(node));
    (*n)->value = value;
  }
}

void deallocate(node **n) {
  while (remove_head(n) != 1)
    ;
}

int remove_head(node **n) {
  if (*n != NULL) {
    node *head = *n;
    *n = (*n)->n;
    free(head);
    return 0;
  } else
    return 1;
}

void print_linkedlist(node *n) {
  for (node *curr = n; curr != NULL; curr = curr->n) {
    printf("%d ", curr->value);
  }
  printf("\n");
}
