# M2algorithmique_Viterbi
Projet M2 Data Science : Algorithme de Viterbi pour la reconnaissance de séquences
Ce dépôt présente une implémentation complète de l'algorithme de Viterbi, ainsi qu'une méthode naïve (brute-force), appliquées à la détection de régions codantes dans une séquence d’ADN.  

## 🔧 Installation

```{r}
# Depuis GitHub
devtools::install_github("Skan1511/viterbiM2")
```
##  Contenu
R/ : Fonctions R pour les versions naïve et optimisée de l’algorithme

src/ : Implémentations C++ intégrées via Rcpp

simulations/ : Scripts de benchmark et de génération de données

rapport_viterbi.Rmd : Rapport complet du projet (R Markdown)

rapport_viterbi.html : Version HTML du rapport

benchmark_bio.png : Résultat visuel du benchmark

##Exemple rapide
```{r}
# Exemple jouet

obs <- c(1, 2, 1)
states <- c("Pluie", "Soleil")
pi <- c(0.6, 0.4)
A <- matrix(c(0.7, 0.3, 0.4, 0.6), 2)
B <- matrix(c(0.9, 0.1, 0.2, 0.8), 2)

viterbi(obs, states, pi, A, B)
```

## Auteur
Mohamed Skander Gharbi
Master 2 Data Science — Université Paris-Saclay