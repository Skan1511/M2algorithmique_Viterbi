# Algorithme naïf de Viterbi pour la séquence d'ADN

trouver_sequence_naive_bio <- function(obs, states, pi, A, B) {
  T <- length(obs)  # Longueur de la séquence observée
  N <- length(states)  # Nombre d'états cachés (C, N)
  
  # Initialisation de la matrice des probabilités
  prob <- matrix(0, nrow = T, ncol = N)
  path <- matrix(0, nrow = T, ncol = N)
  
  # Initialisation des premières probabilités
  prob[1, ] <- pi * B[, obs[1]]
  
  # Récurrence pour les autres états
  for (t in 2:T) {
    for (j in 1:N) {
      prob[t, j] <- max(prob[t-1, ] * A[, j]) * B[j, obs[t]]
      path[t, j] <- which.max(prob[t-1, ] * A[, j])
    }
  }
  
  # Backtracking pour retrouver le chemin optimal
  final_state <- which.max(prob[T, ])
  sequence <- numeric(T)
  sequence[T] <- final_state
  
  for (t in (T-1):1) {
    sequence[t] <- path[t+1, sequence[t+1]]
  }
  
  return(states[sequence])
}
