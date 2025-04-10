# Algorithme de Viterbi en R (version optimisée par programmation dynamique)
viterbi <- function(obs, states, pi, A, B) {
  
  T <- length(obs)    # longueur de la séquence observée
  N <- length(states) # nombre d'états cachés
  
  delta <- matrix(0, T, N)
  psi <- matrix(0, T, N)
  
  # Initialisation
  delta[1, ] <- pi * B[, obs[1]]
  
  # Récurrence
  for (t in 2:T) {
    for (j in 1:N) {
      prob <- delta[t-1, ] * A[, j]
      delta[t, j] <- max(prob) * B[j, obs[t]]
      psi[t, j] <- which.max(prob)
    }
  }
  
  # Backtracking pour trouver la séquence optimale
  path <- numeric(T)
  path[T] <- which.max(delta[T, ])
  for (t in (T-1):1) {
    path[t] <- psi[t+1, path[t+1]]
  }
  
  # Renvoie la séquence optimale trouvée
  return(states[path])
}

# Petit test simple pour vérifier l'algorithme de Viterbi :

# Etats cachés
states <- c("Pluie", "Soleil")

# Observations (parapluie = 1, pas parapluie = 2)
obs <- c(1, 2, 1)

# Probabilités initiales
pi <- c(0.6, 0.4)

# Matrice de transition (Pluie/Soleil vers Pluie/Soleil)
A <- matrix(c(0.7, 0.3,
              0.4, 0.6), byrow = TRUE, nrow = 2)

# Matrice d'émission (probabilité d'observer parapluie ou pas)
B <- matrix(c(0.9, 0.1,  # Pluie
              0.2, 0.8), # Soleil
            byrow = TRUE, nrow = 2)

# Test de la fonction
sequence_opt <- viterbi(obs, states, pi, A, B)

# Affichage résultat
print(sequence_opt)
