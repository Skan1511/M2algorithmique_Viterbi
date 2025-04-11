# Charger les bibliothèques nécessaires
library(Biostrings)

# 1. Générer une séquence d'ADN aléatoire de 500 bases
set.seed(42)  # Pour la reproductibilité
seq_length <- 500  # Longueur de la séquence
bases <- c("A", "T", "C", "G")
seq <- paste(sample(bases, seq_length, replace = TRUE), collapse = "")

# Convertir la séquence en un objet DNAString
seq <- DNAString(seq)

# 2. Définir des régions codantes et non codantes (simulées)
states <- rep("N", seq_length)  # Par défaut, toute la séquence est non codante

# Définir les indices des CDS (exons)
cds_start <- sample(1:(seq_length - 100), 10, replace = TRUE)  # 10 exons, longueur de 100 bases
cds_end <- cds_start + 100  # Exons de longueur 100

# Marquer les régions codantes dans le vecteur `states`
for (i in 1:length(cds_start)) {
  states[cds_start[i]:cds_end[i]] <- "C"
}

# 3. Générer les matrices de transition et d'émission
# - Matrice de transition (A) : P(C -> C), P(C -> N), P(N -> C), P(N -> N)
count_CC <- 0  # Transition C -> C
count_CN <- 0  # Transition C -> N
count_NC <- 0  # Transition N -> C
count_NN <- 0  # Transition N -> N

# - Matrice d'émission (B) : P(A|C), P(T|C), P(C|C), P(G|C), P(A|N), P(T|N), P(C|N), P(G|N)
count_A_C <- 0  # A dans les codants
count_T_C <- 0  # T dans les codants
count_C_C <- 0  # C dans les codants
count_G_C <- 0  # G dans les codants
count_A_N <- 0  # A dans les non codants
count_T_N <- 0  # T dans les non codants
count_C_N <- 0  # C dans les non codants
count_G_N <- 0  # G dans les non codants

# Parcourir la séquence et calculer les transitions et les émissions
for (i in 2:seq_length) {
  # Calcul des transitions
  if (states[i-1] == "C" && states[i] == "C") {
    count_CC <- count_CC + 1
  } else if (states[i-1] == "C" && states[i] == "N") {
    count_CN <- count_CN + 1
  } else if (states[i-1] == "N" && states[i] == "C") {
    count_NC <- count_NC + 1
  } else if (states[i-1] == "N" && states[i] == "N") {
    count_NN <- count_NN + 1
  }
  
  # Calcul des émissions
  base <- substring(as.character(seq), i, i)
  if (states[i] == "C") {
    if (base == "A") count_A_C <- count_A_C + 1
    if (base == "T") count_T_C <- count_T_C + 1
    if (base == "C") count_C_C <- count_C_C + 1
    if (base == "G") count_G_C <- count_G_C + 1
  } else if (states[i] == "N") {
    if (base == "A") count_A_N <- count_A_N + 1
    if (base == "T") count_T_N <- count_T_N + 1
    if (base == "C") count_C_N <- count_C_N + 1
    if (base == "G") count_G_N <- count_G_N + 1
  }
}

# Calcul des probabilités de transition
total_transitions <- count_CC + count_CN + count_NC + count_NN
P_CC <- count_CC / total_transitions
P_CN <- count_CN / total_transitions
P_NC <- count_NC / total_transitions
P_NN <- count_NN / total_transitions

# Matrice de transition (A)
A <- matrix(c(P_CC, P_CN, P_NC, P_NN), byrow = TRUE, nrow = 2)
rownames(A) <- colnames(A) <- c("C", "N")

# Calcul des probabilités d'émission
total_C <- count_A_C + count_T_C + count_C_C + count_G_C
total_N <- count_A_N + count_T_N + count_C_N + count_G_N

P_A_C <- count_A_C / total_C
P_T_C <- count_T_C / total_C
P_C_C <- count_C_C / total_C
P_G_C <- count_G_C / total_C

P_A_N <- count_A_N / total_N
P_T_N <- count_T_N / total_N
P_C_N <- count_C_N / total_N
P_G_N <- count_G_N / total_N

# Matrice d'émission (B)
B <- matrix(c(P_A_C, P_T_C, P_C_C, P_G_C, 
              P_A_N, P_T_N, P_C_N, P_G_N), 
            byrow = TRUE, nrow = 2)
rownames(B) <- c("C", "N")
colnames(B) <- c("A", "T", "C", "G")

# Afficher les matrices
cat("Matrice de transition A :\n")
print(A)

cat("\nMatrice d'émission B :\n")
print(B)

