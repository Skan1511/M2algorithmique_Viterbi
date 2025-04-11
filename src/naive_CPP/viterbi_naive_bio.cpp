#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector naive_viterbi_bio_cpp(IntegerVector obs, NumericVector pi, NumericMatrix A, NumericMatrix B) {
  int T = obs.size();  // Longueur de la séquence d'observations
  int N = pi.size();   // Nombre d'états cachés
  
  NumericMatrix prob(T, N);
  IntegerMatrix path(T, N);
  
  // Initialisation des premières probabilités
  for (int j = 0; j < N; ++j) {
    prob(0, j) = pi[j] * B(j, obs[0] - 1);  // "obs[0] - 1" pour l'indexation correcte
  }
  
  // Calcul des probabilités et chemins pour chaque étape
  for (int t = 1; t < T; ++t) {
    for (int j = 0; j < N; ++j) {
      double max_prob = R_NegInf;
      int max_state = 0;
      for (int i = 0; i < N; ++i) {
        double current_prob = prob(t - 1, i) * A(i, j);
        if (current_prob > max_prob) {
          max_prob = current_prob;
          max_state = i;
        }
      }
      prob(t, j) = max_prob * B(j, obs[t] - 1);
      path(t, j) = max_state;
    }
  }
  
  // Backtracking pour retrouver le chemin optimal
  IntegerVector sequence(T);
  sequence(T - 1) = which_max(prob(T - 1, _));
  
  for (int t = T - 2; t >= 0; --t) {
    sequence(t) = path(t + 1, sequence(t + 1));
  }
  
  return sequence;
}
