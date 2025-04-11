#' Algorithme naïf pour séquence ADN
#'
#' Applique la méthode exhaustive pour trouver la meilleure séquence d'états cachés
#' @param obs Séquence d'observations (codée en entiers correspondant aux bases A, T, C, G)
#' @param states États possibles (vecteur de chaînes, ex: c("C", "N"))
#' @param pi Vecteur de probabilité initiale
#' @param A Matrice de transition
#' @param B Matrice d’émission
#' @return Meilleure séquence d’états (vecteur de caractères)
#' @export
trouver_sequence_naive_bio <- function(obs, states, pi, A, B) {
  T <- length(obs)
  N <- length(states)
  all_paths <- gtools::permutations(n = N, r = T, v = states, repeats.allowed = TRUE)
  
  best_prob <- -Inf
  best_path <- NULL
  
  for (i in 1:nrow(all_paths)) {
    path <- all_paths[i, ]
    prob <- pi[which(states == path[1])] * B[which(states == path[1]), obs[1]]
    
    for (t in 2:T) {
      from <- which(states == path[t - 1])
      to <- which(states == path[t])
      obs_t <- obs[t]
      prob <- prob * A[from, to] * B[to, obs_t]
    }
    
    if (prob > best_prob) {
      best_prob <- prob
      best_path <- path
    }
  }
  return(best_path)
}
