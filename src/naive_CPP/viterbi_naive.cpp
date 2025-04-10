#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List naive_viterbi_cpp(IntegerVector obs, NumericVector pi, NumericMatrix A, NumericMatrix B) {
  int T = obs.size();      // longueur de la séquence
  int N = pi.size();       // nombre d'états
  
  int total = pow(N, T);   // nombre total de combinaisons
  IntegerMatrix comb(total, T);
  
  // Génération de toutes les combinaisons possibles
  for (int i = 0; i < total; i++) {
    int val = i;
    for (int t = T - 1; t >= 0; t--) {
      comb(i, t) = val % N;
      val = val / N;
    }
  }
  
  double max_prob = -1.0;
  IntegerVector best_seq(T);
  
  for (int i = 0; i < total; i++) {
    IntegerVector path = comb(i, _);
    double prob = pi[path[0]] * B(path[0], obs[0] - 1);
    
    for (int t = 1; t < T; t++) {
      prob *= A(path[t - 1], path[t]) * B(path[t], obs[t] - 1);
    }
    
    if (prob > max_prob) {
      max_prob = prob;
      best_seq = path;
    }
  }
  
  return List::create(Named("path") = best_seq + 1,
                      Named("prob") = max_prob);
}
