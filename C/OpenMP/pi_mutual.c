#include <stdio.h>
#include <omp.h>

#define NUM_THREADS 4

static long num_steps = 100000;

int main() {
  double time_start = omp_get_wtime();

  double sum = 0.0;
  double step = 1.0 / (double) num_steps;

  long local_steps = num_steps / NUM_THREADS;
  omp_set_num_threads(NUM_THREADS);

  #pragma omp parallel
  {
    int id = omp_get_thread_num();
    int i = id * local_steps;
    int limit = (id + 1) * local_steps;

    for (; i < limit; i++) {
      double x = (i + 0.5) * step;
      #pragma omp critical
      sum += 4.0 / (1.0 + x * x);
    }
  }

  double pi = step * sum;

  double duration = omp_get_wtime() - time_start;

  printf("Computed pi = %f in %f seconds.\n", pi, duration);

  return 0;
}
