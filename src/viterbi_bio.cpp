#include <Rcpp.h>
using namespace Rcpp;

// Fonction Viterbi (Optimisée pour les matrices de probabilités d'émission et de transition)
// [[Rcpp::export]]
NumericVector viterbi_bio_cpp(NumericVector obs, NumericVector pi, NumericMatrix A, NumericMatrix B) {
  
  int n = obs.size();  // Nombre d'observations
  int m = A.nrow();    // Nombre d'états
  int T = B.ncol();    // Nombre de symboles d'observation
  
  // Vecteurs pour stocker les probabilités maximales et les chemins
  NumericMatrix delta(n, m);   // Probabilité maximale à chaque étape
  IntegerMatrix psi(n, m);     // Pointeurs vers l'état précédent
  
  // Initialisation de la première étape
  for (int i = 0; i < m; ++i) {
    delta(0, i) = pi[i] * B(i, obs[0] - 1); // Observation décalée de 1 pour correspondre aux indices de R
    psi(0, i) = 0;
  }
  
  // Calcul des probabilités maximales pour chaque étape suivante
  for (int t = 1; t < n; ++t) {
    for (int j = 0; j < m; ++j) {
      double max_prob = -1e10;
      int max_state = 0;
      
      for (int i = 0; i < m; ++i) {
        double prob = delta(t-1, i) * A(i, j) * B(j, obs[t] - 1);
        if (prob > max_prob) {
          max_prob = prob;
          max_state = i;
        }
      }
      
      delta(t, j) = max_prob;
      psi(t, j) = max_state;
    }
  }
  
  // Reconstructer le chemin optimal en remontant à partir de la dernière étape
  NumericVector path(n);
  int last_state = which_max(delta(n-1, _));  // L'état final avec la probabilité maximale
  path(n-1) = last_state;
  
  for (int t = n - 2; t >= 0; --t) {
    last_state = psi(t+1, last_state);
    path(t) = last_state;
  }
  
  return path + 1;  // Ajouter 1 pour que les états commencent à 1 (indices R)
}
