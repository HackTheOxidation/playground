#include <stdio.h>
#include <omp.h>

#define NUM_THREADS 4

static long num_steps = 100000;

// NOTE: This implementation sucks, because the sum array cause 'false sharing'
// due to the fact that it sits on the same cacheline.

int main() {
  int i, nthreads;
  double pi, time_start, sum[NUM_THREADS];
  double step = 1.0 / (double) num_steps;

  omp_set_num_threads(NUM_THREADS);
  time_start = omp_get_wtime();
  #pragma omp parallel
  {
    int i;
    int id = omp_get_thread_num();
    double x;
    int nthrds = omp_get_num_threads();
    if (id == 0)
      nthreads = nthrds;

    for (i = id, sum[id] = 0.0; i < num_steps; i = i + nthrds) {
      x = (i + 0.5) * step;
      sum[id] += 4.0 / (1.0 + x * x);
    }
  }

  for (i = 0, pi = 0.0; i < nthreads; i++) {
    pi += sum[i] * step;
  }

  double duration = omp_get_wtime() - time_start;

  printf("Computed pi = %f in %f seconds.\n", pi, duration);
  return 0;
}
