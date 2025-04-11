#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector viterbi_bio_cpp(IntegerVector obs, NumericVector pi, NumericMatrix A, NumericMatrix B) {
  int T = obs.size();  // Length of the observation sequence
  int N = pi.size();   // Number of hidden states
  
  // Initialize matrices for delta and psi (used for backtracking)
  NumericMatrix delta(T, N);
  IntegerMatrix psi(T, N);
  
  // Initialization
  for (int j = 0; j < N; ++j) {
    delta(0, j) = pi[j] * B(j, obs[0] - 1);
  }
  
  // Recurrence
  for (int t = 1; t < T; ++t) {
    for (int j = 0; j < N; ++j) {
      double max_prob = R_NegInf;
      int max_state = 0;
      for (int i = 0; i < N; ++i) {
        double current_prob = delta(t - 1, i) * A(i, j);
        if (current_prob > max_prob) {
          max_prob = current_prob;
          max_state = i;
        }
      }
      delta(t, j) = max_prob * B(j, obs[t] - 1);
      psi(t, j) = max_state;
    }
  }
  
  // Backtracking to get the optimal path
  IntegerVector sequence(T);
  sequence(T - 1) = which_max(delta(T - 1, _));
  
  for (int t = T - 2; t >= 0; --t) {
    sequence(t) = psi(t + 1, sequence(t + 1));
  }
  
  return sequence;
}
