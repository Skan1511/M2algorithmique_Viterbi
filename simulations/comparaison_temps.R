# Comparaison simple des temps entre Viterbi naïf et optimisé

# Chargement des scripts
source("src/naive_R/viterbi_naive.R")
source("src/viterbi_R/viterbi_algo.R")


# Données communes pour les tests
states <- c("Pluie", "Soleil")
pi <- c(0.6, 0.4)
A <- matrix(c(0.7, 0.3,
              0.4, 0.6), byrow = TRUE, nrow = 2)
B <- matrix(c(0.9, 0.1,
              0.2, 0.8), byrow = TRUE, nrow = 2)

# Observation générée artificiellement (exemple simple)
set.seed(123)
obs <- sample(1:2, size = 10, replace = TRUE)

# Temps pour la méthode naïve
temps_naif <- system.time({
  sequence_naive <- trouver_sequence_naive(obs, states, pi, A, B)
})

# Temps pour la méthode Viterbi optimisée
temps_viterbi <- system.time({
  sequence_viterbi <- viterbi(obs, states, pi, A, B)
})

# Affichage clair des résultats
cat("Séquence (naïf) :", sequence_naive, "\n")
cat("Temps (naïf) :", temps_naif["elapsed"], "secondes\n\n")

cat("Séquence (Viterbi) :", sequence_viterbi, "\n")
cat("Temps (Viterbi) :", temps_viterbi["elapsed"], "secondes\n\n")

cat("Le rapport entre les temps (naïf/Viterbi) est de :",
    round(temps_naif["elapsed"] / temps_viterbi["elapsed"], 2), "\n")

