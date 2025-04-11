#' Algorithme de Viterbi
#'
#' @param obs Séquence d'observations (entiers).
#' @param pi Vecteur de probabilités initiales.
#' @param A Matrice de transition.
#' @param B Matrice d'émission.
#'
#' @return Une liste contenant la probabilité maximale et le chemin optimal.
#' @export
viterbi_bio_optimise <- function(obs, pi, A, B) {
  n_states <- length(pi)
  n_observations <- length(obs)
  delta <- matrix(0, nrow = n_states, ncol = n_observations)
  psi <- matrix(0, nrow = n_states, ncol = n_observations)
  delta[, 1] <- pi * B[, obs[1]]
  for (t in 2:n_observations) {
    for (s in 1:n_states) {
      prob <- delta[, t - 1] * A[, s] * B[s, obs[t]]
      delta[s, t] <- max(prob)
      psi[s, t] <- which.max(prob)
    }
  }
  path <- integer(n_observations)
  path[n_observations] <- which.max(delta[, n_observations])
  for (t in (n_observations - 1):1) {
    path[t] <- psi[path[t + 1], t + 1]
  }
  list(prob = max(delta[, n_observations]), path = path)
}
#' @export 
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

