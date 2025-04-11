viterbi_bio_optimise <- function(obs, pi, A, B) {
  # Nombre d'états et d'observations
  n_states <- length(pi)
  n_observations <- length(obs)
  
  # Initialisation des matrices
  delta <- matrix(0, nrow = n_states, ncol = n_observations)
  psi <- matrix(0, nrow = n_states, ncol = n_observations)
  
  # Initialisation de la première colonne de delta
  delta[, 1] <- pi * B[, obs[1]]
  
  # Remplissage des autres colonnes
  for (t in 2:n_observations) {
    for (s in 1:n_states) {
      prob <- delta[, t - 1] * A[, s] * B[s, obs[t]]
      delta[s, t] <- max(prob)
      psi[s, t] <- which.max(prob)
    }
  }
  
  # Recherche de la probabilité optimale
  path <- integer(n_observations)
  path[n_observations] <- which.max(delta[, n_observations])
  
  for (t in (n_observations - 1):1) {
    path[t] <- psi[path[t + 1], t + 1]
  }
  
  # Retourner la probabilité maximale et le chemin optimal
  list(prob = max(delta[, n_observations]), path = path)
}
