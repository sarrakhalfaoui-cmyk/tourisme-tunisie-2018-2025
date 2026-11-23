# ============================================================
# PROJET : TOURISME EN TUNISIE (2018-2025)
# Script : Analyse des données
# ============================================================

# 1. Chargement des bibliothèques
library(tidyverse)

# 2. Importation de la base annuelle
base_analyse <- read_csv(
  "data/base_analyse_finale.csv",
  show_col_types = FALSE
)

# 3. Vérification de la base
glimpse(base_analyse)

# 4. Affichage des premières lignes
head(base_analyse)
# 5. Vérification de la qualité des données

verification <- base_analyse %>%
  summarise(
    nombre_observations = n(),
    premiere_annee = min(annee),
    derniere_annee = max(annee),
    valeurs_manquantes = sum(is.na(annee) |
                               is.na(recettes_touristiques) |
                               is.na(croissance_pib)),
    recettes_min = min(recettes_touristiques),
    recettes_max = max(recettes_touristiques)
  )

print(verification)
# 6. Statistiques descriptives

statistiques <- base_analyse %>%
  summarise(
    moyenne_recettes = mean(recettes_touristiques),
    mediane_recettes = median(recettes_touristiques),
    ecart_type_recettes = sd(recettes_touristiques),
    minimum_recettes = min(recettes_touristiques),
    maximum_recettes = max(recettes_touristiques),
    moyenne_croissance_pib = mean(croissance_pib),
    mediane_croissance_pib = median(croissance_pib)
  )

print(statistiques)
print(statistiques, width = Inf)
# 7. Calcul des variations annuelles

variations <- base_analyse %>%
  arrange(annee) %>%
  mutate(
    variation_pct = (recettes_touristiques /
                       lag(recettes_touristiques) - 1) * 100
  )

print(variations, width = Inf)
# 8. Exportation des résultats

write_csv(statistiques, "resultats/statistiques_descriptives.csv")

write_csv(variations, "resultats/variations_annuelles.csv")
list.files("resultats")
# 9. Corrélation entre recettes touristiques et croissance du PIB

correlation <- cor.test(
  base_analyse$recettes_touristiques,
  base_analyse$croissance_pib,
  method = "pearson"
)

print(correlation)
# 10. Exportation du résultat de corrélation

resultat_correlation <- tibble(
  coefficient = unname(correlation$estimate),
  p_value = correlation$p.value,
  borne_inferieure = correlation$conf.int[1],
  borne_superieure = correlation$conf.int[2]
)

write_csv(
  resultat_correlation,
  "resultats/correlation_tourisme_pib.csv"
)
list.files("resultats")
read_csv("resultats/correlation_tourisme_pib.csv")
# 11. Importation des données mensuelles

base_tourisme <- read_csv(
  "data/analyse_saisonnalite.csv",
  show_col_types = FALSE
)

# Vérification de la structure
glimpse(base_tourisme)

# Affichage des premières lignes
head(base_tourisme)
# 12. Organisation chronologique des mois

ordre_mois <- c(
  "JANVIER", "FEVRIER", "MARS", "AVRIL",
  "MAI", "JUIN", "JUILLET", "AOUT",
  "SEPTEMBRE", "OCTOBRE", "NOVEMBRE", "DECEMBRE"
)

base_tourisme <- base_tourisme %>%
  mutate(
    mois = factor(mois, levels = ordre_mois)
  ) %>%
  arrange(mois)

print(base_tourisme, n = 12)
# 13. Identification des mois extrêmes

mois_max <- base_tourisme %>%
  slice_max(moyenne_recettes, n = 1)

mois_min <- base_tourisme %>%
  slice_min(moyenne_recettes, n = 1)

cat("Mois avec les recettes moyennes les plus élevées :\n")
print(mois_max)

cat("Mois avec les recettes moyennes les plus faibles :\n")
print(mois_min)
# 14. Mesure de l'écart saisonnier

ecart_saisonnier <- max(base_tourisme$moyenne_recettes) -
  min(base_tourisme$moyenne_recettes)

rapport_saisonnier <- max(base_tourisme$moyenne_recettes) /
  min(base_tourisme$moyenne_recettes)

cat("Écart entre le maximum et le minimum :", ecart_saisonnier, "MDT\n")
cat("Rapport entre le maximum et le minimum :", rapport_saisonnier, "\n")
# 15. Exportation des résultats de saisonnalité

write_csv(
  base_tourisme %>% mutate(mois = as.character(mois)),
  "resultats/moyennes_mensuelles.csv"
)

class(base_tourisme)
names(base_tourisme)
resume_final <- base_tourisme %>%
  summarise(
    mois_max = as.character(mois[which.max(moyenne_recettes)]),
    recette_max = max(moyenne_recettes),
    mois_min = as.character(mois[which.min(moyenne_recettes)]),
    recette_min = min(moyenne_recettes),
    ecart_saisonnier = max(moyenne_recettes) - min(moyenne_recettes),
    rapport_saisonnier = max(moyenne_recettes) / min(moyenne_recettes)
  )

print(resume_final)

write_csv(
  resume_final,
  "resultats/resume_saisonnalite.csv"
)
file.exists("resultats/resume_saisonnalite.csv")
list.files("resultats")


