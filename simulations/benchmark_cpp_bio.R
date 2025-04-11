# Benchmark C++ vs R : version optimisée et version naïve

library(Rcpp)
library(ggplot2)
library(microbenchmark)

# Charger les matrices depuis le fichier viterbi_brca1.R
source("C:/Users/gharb/Documents/M2algo_viterbi/viterbiM2/simulations/viterbi_brca1.R")

# Charger les fonctions R pour les versions naïve et optimisée
source("C:/Users/gharb/Documents/M2algo_viterbi/viterbiM2/R/viterbi_bio_naive.R")
source("C:/Users/gharb/Documents/M2algo_viterbi/viterbiM2/src/viterbi_R/viterbi_bio.R")

# Charger les versions C++ via Rcpp
sourceCpp("C:/Users/gharb/Documents/M2algo_viterbi/viterbiM2/src/viterbi_bio.cpp")
sourceCpp("C:/Users/gharb/Documents/M2algo_viterbi/viterbiM2/src/naive_CPP/viterbi_naive_bio.cpp")

# Paramètres communs
states <- c("C", "N")  # Codant et Non-Codant
pi <- pi  # Probabilités initiales (chargées depuis viterbi_brca1.R)
A <- A  # Matrice de transition (chargée depuis viterbi_brca1.R)
B <- B  # Matrice d'émission (chargée depuis viterbi_brca1.R)

# Séquence d'observations courte (pour tester la version naïve)
obs_small <- sample(1:2, 6, replace = TRUE)

# Séquence d'observations plus longue (pour tester la version optimisée)
obs_long <- sample(1:2, 50, replace = TRUE)

# --- BENCHMARK NAIF ---
bm_naif <- microbenchmark(
  naive_R = trouver_sequence_naive_bio(obs_small, states, pi, A, B),
  naive_CPP = naive_viterbi_bio_cpp(obs_small, pi, A, B),
  times = 10L
)

# --- BENCHMARK VITERBI ---
bm_viterbi <- microbenchmark(
  viterbi_R = viterbi_bio_optimise(obs_long, pi, A, B),
  viterbi_CPP = viterbi_bio_cpp(obs_long, pi, A, B),
  times = 10L
)

# Affichage des résultats
print(bm_naif)
print(bm_viterbi)

# --- Graphique de Benchmark ---
library(reshape2)

df_naif <- as.data.frame(bm_naif)
df_vit <- as.data.frame(bm_viterbi)

df_naif$Type <- "Naïf"
df_vit$Type <- "Viterbi"

df_all <- rbind(df_naif, df_vit)
df_all$expr <- factor(df_all$expr, levels = unique(df_all$expr))

# Bar plot des temps moyens
ggplot(df_all, aes(x = expr, y = time / 1e6, fill = Type)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.7) +
  labs(title = "Comparaison des temps d'exécution R vs C++",
       x = "Méthode",
       y = "Temps (ms)",
       fill = "Approche") +
  theme_minimal(base_size = 14)
