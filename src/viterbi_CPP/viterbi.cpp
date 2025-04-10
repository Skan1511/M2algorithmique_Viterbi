#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector viterbi_cpp(IntegerVector obs, NumericVector pi, NumericMatrix A, NumericMatrix B) {
  int T = obs.size();      // Longueur des observations
  int N = pi.size();       // Nombre d'états
  
  NumericMatrix delta(T, N);
  IntegerMatrix psi(T, N);
  
  // Initialisation
  for (int i = 0; i < N; i++) {
    delta(0, i) = pi[i] * B(i, obs[0] - 1);
  }
  
  // Récurrence
  for (int t = 1; t < T; t++) {
    for (int j = 0; j < N; j++) {
      double max_val = 0.0;
      int max_state = 0;
      for (int i = 0; i < N; i++) {
        double val = delta(t - 1, i) * A(i, j);
        if (val > max_val) {
          max_val = val;
          max_state = i;
        }
      }
      delta(t, j) = max_val * B(j, obs[t] - 1);
      psi(t, j) = max_state;
    }
  }
  
  // Backtracking
  IntegerVector path(T);
  int last_state = 0;
  double max_prob = 0.0;
  for (int i = 0; i < N; i++) {
    if (delta(T - 1, i) > max_prob) {
      max_prob = delta(T - 1, i);
      last_state = i;
    }
  }
  
  path[T - 1] = last_state;
  for (int t = T - 2; t >= 0; t--) {
    path[t] = psi(t + 1, path[t + 1]);
  }
  
  return path + 1; // conversion vers index R
}
