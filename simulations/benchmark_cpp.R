# Benchmark C++ vs R : version optimisée et version naïve

library(Rcpp)
library(ggplot2)
library(microbenchmark)

# Charger les fonctions R
source("C:/Users/gharb/Documents/M2algo_viterbi/M2algorithmique_Viterbi/src/naive_R/viterbi_naive.R")
source("C:/Users/gharb/Documents/M2algo_viterbi/M2algorithmique_Viterbi/src/viterbi_R/viterbi_algo.R")


# Charger les versions C++
sourceCpp("C:/Users/gharb/Documents/M2algo_viterbi/M2algorithmique_Viterbi/src/viterbi_CPP/viterbi.cpp")
sourceCpp("C:/Users/gharb/Documents/M2algo_viterbi/M2algorithmique_Viterbi/src/naive_CPP/viterbi_naive.cpp")

# Paramètres communs
states <- c("Pluie", "Soleil")
pi <- c(0.6, 0.4)
A <- matrix(c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE)
B <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)

# Observation courte (pour naïf)
obs_small <- sample(1:2, 6, replace = TRUE)

# Observation plus longue (pour viterbi uniquement)
obs_long <- sample(1:2, 50, replace = TRUE)

cat("Observation (naïf):", obs_small, "\n")
cat("Observation (optimisé):", obs_long[1:10], "... (total:", length(obs_long), ")\n\n")

# --- BENCHMARK NAIF ---

bm_naif <- microbenchmark(
  naive_R = trouver_sequence_naive(obs_small, states, pi, A, B),
  naive_CPP = naive_viterbi_cpp(obs_small, pi, A, B),
  times = 10L
)

# --- BENCHMARK VITERBI ---

bm_viterbi <- microbenchmark(
  viterbi_R = viterbi(obs_long, states, pi, A, B),
  viterbi_CPP = viterbi_cpp(obs_long, pi, A, B),
  times = 10L
)

print(bm_naif)
print(bm_viterbi)

# --- GRAPHIQUE COMPARATIF ---

library(reshape2)

df_naif <- as.data.frame(bm_naif)
df_vit <- as.data.frame(bm_viterbi)

df_naif$Type <- "Naïf"
df_vit$Type <- "Viterbi"

df_all <- rbind(df_naif, df_vit)
df_all$expr <- factor(df_all$expr, levels = unique(df_all$expr))

ggplot(df_all, aes(x = expr, y = time / 1e6, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparaison des temps d'exécution R vs C++",
       x = "Méthode",
       y = "Temps (ms)",
       fill = "Approche") +
  theme_minimal(base_size = 14)
