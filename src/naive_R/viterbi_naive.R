install.packages("gtools")
# Premier test : affichage simple
print("Mon premier script Viterbi (version naive)")

# Solution naïve (force brute) en R pour Viterbi (exemple à 2 états)

# Génère toutes les séquences possibles d'états
library(gtools) # pour permutations avec répétitions (install.packages("gtools") si nécessaire)

trouver_sequence_naive <- function(obs, states, pi, A, B) {
  T <- length(obs)
  N <- length(states)
  
  # Toutes les combinaisons possibles d'états cachés
  combinaisons <- permutations(n = N, r = T, v = states, repeats.allowed = TRUE)
  
  max_prob <- 0
  meilleur_chemin <- NULL
  
  for(i in 1:nrow(combinaisons)) {
    seq <- combinaisons[i, ]
    prob <- pi[which(states == seq[1])] * B[which(states == seq[1]), obs[1]]
    if (T > 1) {
      for (t in 2:T) {
        etat_prec <- which(states == seq[t-1])
        etat_courant <- which(states == seq[t])
        prob <- prob * A[etat_prec, etat_courant] * B[etat_courant, obs[t]]
      }
    }
    
    if(prob > max_prob) {
      max_prob <- prob
      meilleur_chemin <- seq
    }
  }
  
  return(meilleur_chemin)
}

# Test rapide avec les mêmes données que précédemment
states <- c("Pluie", "Soleil")
obs <- c(1, 2, 1)
pi <- c(0.6, 0.4)
A <- matrix(c(0.7, 0.3,
              0.4, 0.6), byrow = TRUE, nrow = 2)
B <- matrix(c(0.9, 0.1,
              0.2, 0.8), byrow = TRUE, nrow = 2)

# Exécute la solution naïve
sequence_naive <- trouver_sequence_naive(obs, states, pi, A, B)

print(sequence_naive)
