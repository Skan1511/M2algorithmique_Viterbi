# Exemple bioinformatique : prédiction de zones codantes dans une séquence ADN

# États : C = Codant, N = Non-codant
states <- c("C", "N")

# Probabilités initiales (on commence souvent en non-codant)
pi <- c(0.2, 0.8)

# Matrice de transition entre états
A <- matrix(c(
  0.8, 0.2,
  0.4, 0.6
), nrow = 2, byrow = TRUE)


# Matrice d'émission : probas de A/T/C/G selon C/N
B <- matrix(c(
  0.4, 0.1, 0.4, 0.1,    # Codant favorise A et C
  0.1, 0.4, 0.1, 0.4     # Non-codant favorise T et G
), nrow = 2, byrow = TRUE)

colnames(B) <- c("A", "T", "C", "G")

# Génération d'une séquence ADN observée
set.seed(123)
obs_raw <- sample(c("A", "T", "C", "G"), size = 50, replace = TRUE)

# Conversion en indices pour l'algo (1-based)
obs <- match(obs_raw, c("A", "T", "C", "G"))

# Application de l'algo de Viterbi (en R ou C++)
sequence_predite <- viterbi(obs, states, pi, A, B)

# Résultat
data.frame(Base = obs_raw, État = sequence_predite)
