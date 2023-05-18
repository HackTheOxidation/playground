

void gauss_elimitation(double *A[], double* b, int N) {
  for (int i = 0; i < N-1; i++) {
    for (int j = i; j < N; j++) {
      double ratio = A[j][i]/A[i][i];
      for (int k = i; k < N; k++) {
	A[j][k] -= (ratio * A[i][k]);
	b[j] -= (ratio * b[i]);
      }
    }
  }
}
