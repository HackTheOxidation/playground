#ifndef LINKEDLIST_H_
#define LINKEDLIST_H_

typedef struct node {
  int value;
  struct node* n;
} node;

node* create_linkedlist(int value);
void insert_head(node** n, int value);
int remove_head(node** n);
void print_linkedlist(node* n);
void deallocate(node** n);

#endif
