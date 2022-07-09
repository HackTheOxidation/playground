#ifndef weak_alias
#define weak_alias(old, new)                                                   \
  extern __typeof(old) new __attribute__((__weak__, __alias__(#old)))
#endif

#define _GNU_SOURCE
#include <fcntl.h>

int creat(const char *filename, mode_t mode) {
  return open(filename, O_CREAT | O_WRONLY | O_TRUNC, mode);
}

weak_alias(creat, creat64);
