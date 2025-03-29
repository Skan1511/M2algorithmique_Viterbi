# Benchmark sur différentes tailles de séquences

# Chargement des fonctions
source("src/naive_R/viterbi_naive.R")
source("src/viterbi_R/viterbi_algo.R")

# Paramètres communs
states <- c("Pluie", "Soleil")
pi <- c(0.6, 0.4)
A <- matrix(c(0.7, 0.3,
              0.4, 0.6), byrow = TRUE, nrow = 2)
B <- matrix(c(0.9, 0.1,
              0.2, 0.8), byrow = TRUE, nrow = 2)

# Tailles de séquences à tester
tailles <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)

# Vecteurs pour stocker les temps
temps_naif <- numeric(length(tailles))
temps_viterbi <- numeric(length(tailles))

# Benchmark
for (i in seq_along(tailles)) {
  set.seed(123)  # reproductible
  obs <- sample(1:2, tailles[i], replace = TRUE)
  
  # Temps naïf
  t1 <- system.time({
    trouver_sequence_naive(obs, states, pi, A, B)
  })["elapsed"]
  
  # Temps Viterbi
  t2 <- system.time({
    viterbi(obs, states, pi, A, B)
  })["elapsed"]
  
  temps_naif[i] <- t1
  temps_viterbi[i] <- t2
}

# Affichage simple des résultats
df <- data.frame(
  Taille = tailles,
  Naif = temps_naif,
  Viterbi = temps_viterbi
)

print(df)
install.packages("ggplot2")
# Charger ggplot2
library(ggplot2)

# Transformer le tableau pour ggplot (long format)
df_long <- reshape2::melt(df, id.vars = "Taille", variable.name = "Méthode", value.name = "Temps")

# Tracer le graphique
ggplot(df_long, aes(x = Taille, y = Temps, color = Méthode)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_log10()

  labs(
    title = "Temps d'exécution selon la taille de la séquence",
    x = "Taille de la séquence observée",
    y = "Temps (secondes)",
    color = "Méthode"
  ) +
  theme_minimal(base_size = 14)
ggplot(df_long, aes(x = Taille, y = Temps, color = Méthode)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Temps d'exécution en fonction de la taille de la séquence",
    x = "Taille de la séquence observée",
    y = "Temps (secondes)",
    color = "Méthode"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # ajoute un peu d'espace en haut
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),  # centre le titre
    legend.position = "bottom"              # déplace la légende en bas
  )
