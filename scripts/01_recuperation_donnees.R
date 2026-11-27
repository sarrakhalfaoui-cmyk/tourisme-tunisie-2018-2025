# ==========================================
# PROJET : TOURISME EN TUNISIE (2018-2025)
# ETAPE 1 : TELECHARGEMENT DES DONNEES
# ==========================================

# Creer le dossier data s'il n'existe pas
dir.create("data", showWarnings = FALSE)

# Adresse du document officiel
url <- "https://www.tourisme.gov.tn/wp-content/uploads/2025/03/EXTRAIT-2023_-002-1.pdf"

# Emplacement du fichier
destination <- "data/recettes_touristiques_2016_2023.pdf"

# Telecharger le document
download.file(
  url = url,
  destfile = destination,
  mode = "wb"
)

# Verifier que le fichier existe
file.exists(destination)
# ==========================================
# ETAPE 2 : EXTRACTION DU PDF
# ==========================================

# Lire les informations du PDF
infos_pdf <- pdf_info(destination)

# Afficher les informations
print(infos_pdf)
# Extraire le texte de toutes les pages
contenu <- pdf_text(destination)

# Compter le nombre de pages
length(contenu)
# Afficher la page contenant les recettes mensuelles
cat(contenu[18])
infos_pdf <- pdf_info(destination)

contenu <- pdf_text(destination)

cat(contenu[18])
# Sauvegarder le texte de la page 18
writeLines(
  contenu[18],
  "data/page_recettes.txt"
)
# Enregistrer le texte extrait du PDF
writeLines(
  contenu[18],
  "data/page_recettes.txt"
)

# Vérifier que le fichier existe
file.exists("data/page_recettes.txt")
# Lire le fichier texte
texte_recettes <- readLines("data/page_recettes.txt")

# Afficher les premières lignes
head(texte_recettes, 20)
# Afficher la structure du texte
str(texte_recettes)
# ==========================================
# ETAPE 3 : EXTRACTION DES MOTS ET COORDONNEES
# ==========================================

# Extraire les mots de la page 18
tableau_pdf <- pdf_data(destination)[[18]]

# Afficher les premieres lignes
head(tableau_pdf, 20)
# Verifier les noms des colonnes
names(tableau_pdf)

# Afficher les dimensions
dim(tableau_pdf)

# Afficher les premiers mots
head(tableau_pdf, 30)
# ==========================================
# ETAPE 4 : ORGANISATION DES DONNEES
# ==========================================

# Charger dplyr
library(dplyr)

# Trier les mots selon leur position
tableau_organise <- tableau_pdf %>%
  arrange(y, x)

# Afficher tous les elements extraits
print(tableau_organise, n = Inf, width = Inf)
# Afficher les noms des mois
tableau_organise %>%
  filter(
    text %in% c(
      "JANVIER", "FEVRIER", "MARS",
      "AVRIL", "MAI", "JUIN",
      "JUILLET", "AOUT", "SEPTEMBRE",
      "OCTOBRE", "NOVEMBRE", "DECEMBRE"
    )
  )
# ==========================================
# ETAPE 5 : IDENTIFICATION DES ANNEES
# ==========================================

# Afficher les annees et leurs positions
tableau_organise %>%
  filter(grepl("^(201[6-9]|202[0-3])\\*?$", text)) %>%
  select(x, y, text) %>%
  arrange(x)
# Afficher les elements correspondant aux lignes des mois
tableau_organise %>%
  filter(y >= 108, y <= 295) %>%
  select(x, y, text) %>%
  arrange(y, x)
# ==========================================
# ETAPE 6 : IDENTIFICATION DES ANNEES
# ==========================================

annees_pdf <- tableau_organise %>%
  filter(
    grepl("^(201[6-9]|202[0-3])\\*?$", text),
    y == 90
  ) %>%
  mutate(
    annee = as.integer(gsub("\\*", "", text))
  ) %>%
  select(x, annee) %>%
  arrange(x)

# Afficher les annees
print(annees_pdf)
# ==========================================
# ETAPE 7 : EXTRACTION DES RECETTES
# ==========================================

recettes_pdf <- tableau_organise %>%
  filter(
    grepl("^[0-9]+,[0-9]+$", text),
    y >= 108,
    y <= 295
  ) %>%
  mutate(
    recettes = as.numeric(
      gsub(",", ".", text)
    )
  ) %>%
  select(x, y, recettes) %>%
  arrange(y, x)

# Afficher les premieres valeurs
head(recettes_pdf, 20)
# Afficher les textes originaux de la premiere ligne
tableau_organise %>%
  filter(y == 108) %>%
  select(x, text) %>%
  arrange(x)
# Afficher les valeurs avec leur precision exacte
dput(head(recettes_pdf$recettes, 8))
# Compter les valeurs extraites
nrow(recettes_pdf)
# ==========================================
# ETAPE 8 : CONSTRUCTION DE LA BASE
# ==========================================

# Liste des mois dans l'ordre chronologique
mois_ordre <- c(
  "JANVIER", "FEVRIER", "MARS",
  "AVRIL", "MAI", "JUIN",
  "JUILLET", "AOUT", "SEPTEMBRE",
  "OCTOBRE", "NOVEMBRE", "DECEMBRE"
)

# Recuperer les mois et leurs positions
mois_pdf <- tableau_organise %>%
  filter(text %in% mois_ordre) %>%
  transmute(y, mois = text) %>%
  distinct()

# Afficher les mois
print(mois_pdf)
# Associer chaque colonne a une annee
annees_par_colonne <- annees_pdf %>%
  arrange(x) %>%
  mutate(colonne = row_number()) %>%
  select(colonne, annee)

# Afficher les annees
print(annees_par_colonne)
# Extraire et organiser les recettes
base_recettes <- tableau_organise %>%
  
  # Garder uniquement les lignes des mois
  filter(
    y %in% mois_pdf$y,
    grepl("^[0-9]+,[0-9]+$", text)
  ) %>%
  
  # Transformer les textes en nombres
  mutate(
    recettes = as.numeric(
      gsub(",", ".", text)
    )
  ) %>%
  
  # Garder les colonnes utiles
  select(x, y, recettes) %>%
  
  # Classer les recettes par position
  group_by(y) %>%
  arrange(x, .by_group = TRUE) %>%
  mutate(colonne = row_number()) %>%
  ungroup() %>%
  
  # Associer les mois
  left_join(mois_pdf, by = "y") %>%
  
  # Associer les annees
  left_join(annees_par_colonne, by = "colonne") %>%
  
  # Organiser les colonnes finales
  select(annee, mois, recettes) %>%
  
  # Trier chronologiquement
  arrange(annee, match(mois, mois_ordre))

# Afficher notre base
print(base_recettes)
# Verifier le nombre de lignes
nrow(base_recettes)

# Verifier le nombre de valeurs manquantes
sum(is.na(base_recettes))

# Compter les observations par annee
base_recettes %>%
  count(annee)
# Afficher les 12 premiers mois de 2016
base_recettes %>%
  filter(annee == 2016)
# Afficher les 12 mois de 2023
base_recettes %>%
  filter(annee == 2023)
# Verifier les valeurs exactes de 2016
dput(
  base_recettes %>%
    filter(annee == 2016) %>%
    pull(recettes)
)
# Nombre total de lignes
nrow(base_recettes)

# Nombre de valeurs manquantes
sum(is.na(base_recettes))

# Nombre de mois par annee
base_recettes %>%
  count(annee)
# Sauvegarder la base au format CSV
write_csv(
  base_recettes,
  "data/recettes_touristiques_2016_2023.csv"
)
# ==========================================
# ETAPE 9 : SELECTION DE LA PERIODE
# ==========================================

# Selectionner les annees 2018 a 2023
base_tourisme <- base_recettes %>%
  filter(annee >= 2018, annee <= 2023)

# Afficher les premieres lignes
head(base_tourisme, 12)

# Verifier le nombre de lignes
nrow(base_tourisme)

# Verifier les annees disponibles
unique(base_tourisme$annee)
# Enregistrer les donnees de 2018 a 2023
write_csv(
  base_tourisme,
  "data/recettes_touristiques_2018_2023.csv"
)

# Verifier l'enregistrement
file.exists("data/recettes_touristiques_2018_2023.csv")
# ==========================================
# ETAPE 10 : DONNEES ECONOMIQUES
# ==========================================

# Recuperer la croissance economique de la Tunisie
croissance_tunisie <- WDI(
  country = "TUN",
  indicator = c(
    croissance = "NY.GDP.MKTP.KD.ZG"
  ),
  start = 2018,
  end = 2025
)

# Afficher les donnees
print(croissance_tunisie)
# Garder les colonnes utiles
croissance_tunisie <- croissance_tunisie %>%
  select(year, croissance)

# Renommer les colonnes
croissance_tunisie <- croissance_tunisie %>%
  rename(
    annee = year
  )

# Afficher les resultats
print(croissance_tunisie)
# ==========================================
# ETAPE 11 : WEB SCRAPING DES DONNEES BCT
# ==========================================

# Adresse du site de la Banque Centrale
url_bct <- "https://www.bct.gov.tn/bct/siteprod/tableau_n_ne.jsp?la=AN&params=PL232010%2CPL232020%2CPL233010%2CPL233020"

# Lire le contenu de la page Internet
page_bct <- read_html(url_bct)

# Extraire les tableaux HTML
tables_bct <- page_bct %>%
  html_elements("table") %>%
  html_table(fill = TRUE)

# Compter les tableaux extraits
length(tables_bct)
# Afficher les dimensions de chaque tableau
lapply(tables_bct, dim)
# Afficher les tableaux contenant les données

for (i in c(2, 4, 6, 8)) {
  
  cat("\n============================\n")
  cat("TABLEAU NUMERO :", i, "\n")
  cat("============================\n")
  
  print(tables_bct[[i]])
}
# Afficher toutes les lignes du tableau 4
print(tables_bct[[4]], n = Inf)
# Afficher toutes les lignes du tableau 2
print(tables_bct[[2]], n = Inf)
# Afficher les noms des colonnes
names(tables_bct[[4]])
print(tables_bct[[4]], n = Inf)
# ==========================================
# ETAPE 12 : EXTRACTION DES RECETTES BCT
# ==========================================

# Identifier la ligne des recettes touristiques
ligne_tourisme <- tables_bct[[2]] %>%
  filter(str_detect(X1, "Tourism-related services"))

# Afficher la ligne extraite
print(ligne_tourisme)
# Recuperer les valeurs de 2021 a 2025
valeurs_bct <- c(
  ligne_tourisme$X2,
  ligne_tourisme$X3,
  ligne_tourisme$X4,
  ligne_tourisme$X5,
  ligne_tourisme$X6
)

# Remplacer les virgules par des points
valeurs_bct <- gsub(",", ".", valeurs_bct)

# Supprimer les espaces eventuels
valeurs_bct <- gsub("[[:space:]]", "", valeurs_bct)

# Convertir en nombres
valeurs_bct <- as.numeric(valeurs_bct)

# Construire notre base annuelle
recettes_bct <- tibble(
  annee = 2021:2025,
  recettes = valeurs_bct
)

# Afficher la nouvelle base
print(recettes_bct)
# Verifier les valeurs manquantes
sum(is.na(recettes_bct))

# Verifier les dimensions
dim(recettes_bct)

# Afficher les valeurs exactes
dput(recettes_bct)
write_csv(
  recettes_bct,
  "data/recettes_touristiques_2021_2025_BCT.csv"
)
# ==========================================
# ETAPE 13 : CONSTRUCTION DE LA BASE ANNUELLE
# ==========================================

# Calculer les recettes totales de chaque annee
recettes_pdf_annuelles <- base_tourisme %>%
  group_by(annee) %>%
  summarise(
    recettes_pdf = sum(recettes),
    .groups = "drop"
  )

# Afficher les resultats
print(recettes_pdf_annuelles)
# Comparer les donnees PDF et BCT
comparaison_sources <- recettes_pdf_annuelles %>%
  inner_join(recettes_bct, by = "annee") %>%
  mutate(
    difference = recettes_pdf - recettes
  )

# Afficher la comparaison
print(comparaison_sources)
# Verifier les differences entre les sources
all(
  abs(comparaison_sources$difference) < 0.1
)
# ==========================================
# ETAPE 14 : BASE ANNUELLE COMPLETE
# ==========================================

# Garder les recettes PDF jusqu'en 2023
recettes_2018_2023 <- recettes_pdf_annuelles

# Garder les recettes BCT de 2024 et 2025
recettes_2024_2025 <- recettes_bct %>%
  filter(annee >= 2024)

# Fusionner les deux bases
recettes_annuelles <- bind_rows(
  recettes_2018_2023,
  recettes_2024_2025 %>%
    rename(recettes_pdf = recettes)
)

# Trier les annees
recettes_annuelles <- recettes_annuelles %>%
  arrange(annee)

# Afficher la base complete
print(recettes_annuelles)
# Fusionner les recettes et la croissance du PIB
base_finale <- recettes_annuelles %>%
  left_join(
    croissance_tunisie,
    by = "annee"
  )

# Afficher la base finale
print(base_finale)
# Nombre de lignes
nrow(base_finale)

# Valeurs manquantes
colSums(is.na(base_finale))

# Structure de la base
str(base_finale)
write_csv(
  base_finale,
  "data/base_finale_tourisme_economie_2018_2025.csv"
)
print(base_finale)
# ==========================================
# ETAPE 15 : ANALYSE STATISTIQUE DESCRIPTIVE
# ==========================================

# Renommer les variables
base_analyse <- base_finale %>%
  rename(
    recettes_touristiques = recettes_pdf,
    croissance_pib = croissance
  )

# Afficher la nouvelle base
print(base_analyse)
# Calculer les statistiques descriptives
statistiques <- base_analyse %>%
  summarise(
    moyenne_recettes = mean(recettes_touristiques),
    mediane_recettes = median(recettes_touristiques),
    ecart_type_recettes = sd(recettes_touristiques),
    minimum_recettes = min(recettes_touristiques),
    maximum_recettes = max(recettes_touristiques),
    
    moyenne_croissance = mean(croissance_pib),
    mediane_croissance = median(croissance_pib),
    ecart_type_croissance = sd(croissance_pib)
  )

# Afficher les statistiques
print(statistiques)
# Calculer la variation annuelle des recettes
evolution_recettes <- base_analyse %>%
  arrange(annee) %>%
  mutate(
    variation_recettes = (
      recettes_touristiques /
        lag(recettes_touristiques) - 1
    ) * 100
  )

# Afficher les resultats
print(evolution_recettes)
# ==========================================
# GRAPHIQUE 1 : EVOLUTION DES RECETTES
# ==========================================

graph_recettes <- ggplot(
  base_analyse,
  aes(
    x = annee,
    y = recettes_touristiques
  )
) +
  geom_line(
    color = "#167D9A",
    linewidth = 1.2
  ) +
  geom_point(
    color = "#E67E22",
    size = 3
  ) +
  labs(
    title = "Evolution des recettes touristiques en Tunisie",
    subtitle = "Periode 2018-2025",
    x = "Annee",
    y = "Recettes touristiques",
    caption = "Sources : donnees PDF et Banque Centrale de Tunisie"
  ) +
  theme_minimal()

# Afficher le graphique
print(graph_recettes)
print(statistiques)
print(evolution_recettes)
evolution_recettes <- base_analyse %>%
  arrange(annee) %>%
  mutate(
    variation_recettes = (
      recettes_touristiques /
        lag(recettes_touristiques) - 1
    ) * 100
  )

print(evolution_recettes)

ggplot(
  evolution_recettes %>% filter(!is.na(variation_recettes)),
  aes(x = factor(annee), y = variation_recettes)
) +
  geom_col(fill = "#167D9A") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Variation annuelle des recettes touristiques",
    subtitle = "Tunisie, 2019-2025",
    x = "Année",
    y = "Variation des recettes (%)",
    caption = "Source : données touristiques collectées"
  ) +
  theme_minimal()
ggplot(
  base_analyse,
  aes(x = recettes_touristiques, y = croissance_pib)
) +
  geom_point(color = "#E67E22", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "#167D9A") +
  labs(
    title = "Tourisme et croissance économique en Tunisie",
    subtitle = "Relation entre recettes touristiques et croissance du PIB (2018-2025)",
    x = "Recettes touristiques",
    y = "Croissance du PIB (%)",
    caption = "Sources : données touristiques et Banque mondiale"
  ) +
  theme_minimal()
ggplot(
  base_analyse,
  aes(x = recettes_touristiques, y = croissance_pib)
) +
  geom_point(color = "#E67E22", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "#167D9A") +
  labs(
    title = "Tourisme et croissance économique en Tunisie",
    subtitle = "Relation entre recettes touristiques et croissance du PIB (2018-2025)",
    x = "Recettes touristiques",
    y = "Croissance du PIB (%)",
    caption = "Sources : données touristiques et Banque mondiale"
  ) +
  theme_minimal()
cor(
  base_analyse$recettes_touristiques,
  base_analyse$croissance_pib,
  method = "pearson"
)
test_correlation <- cor.test(
  base_analyse$recettes_touristiques,
  base_analyse$croissance_pib,
  method = "pearson"
)

print(test_correlation)
evolution_recettes %>%
  filter(!is.na(variation_recettes)) %>%
  arrange(desc(variation_recettes))
annee_plus_forte_hausse <- evolution_recettes %>%
  filter(variation_recettes == max(variation_recettes, na.rm = TRUE))

annee_plus_forte_baisse <- evolution_recettes %>%
  filter(variation_recettes == min(variation_recettes, na.rm = TRUE))

print(annee_plus_forte_hausse)
print(annee_plus_forte_baisse)
ggplot(
  base_analyse,
  aes(x = annee, y = recettes_touristiques)
) +
  geom_line(color = "#167D9A", linewidth = 1.2) +
  geom_point(color = "#167D9A", size = 3) +
  
  annotate(
    "label",
    x = 2020,
    y = 2439,
    label = "Chute en 2020\nPandémie COVID-19",
    color = "#B03A2E",
    fill = "#FDEDEC"
  ) +
  
  annotate(
    "label",
    x = 2022,
    y = 5420,
    label = "Forte reprise\n+83,5 %",
    color = "#196F3D",
    fill = "#E9F7EF"
  ) +
  
  labs(
    title = "Le tourisme tunisien : de la crise à la reprise",
    subtitle = "Evolution des recettes touristiques, 2018-2025",
    x = "Année",
    y = "Recettes touristiques",
    caption = "Sources : données PDF et Banque Centrale de Tunisie"
  ) +
  theme_minimal()
saisonnalite <- base_tourisme %>%
  group_by(mois) %>%
  summarise(
    moyenne_recettes = mean(recettes, na.rm = TRUE),
    .groups = "drop"
  )

print(saisonnalite)

ordre_mois <- c(
  "JANVIER", "FEVRIER", "MARS", "AVRIL",
  "MAI", "JUIN", "JUILLET", "AOUT",
  "SEPTEMBRE", "OCTOBRE", "NOVEMBRE", "DECEMBRE"
)

saisonnalite <- saisonnalite %>%
  mutate(
    mois = factor(mois, levels = ordre_mois)
  ) %>%
  arrange(mois)

ggplot(
  saisonnalite,
  aes(x = mois, y = moyenne_recettes)
) +
  geom_col(fill = "#167D9A", width = 0.7) +
  labs(
    title = "La saisonnalité des recettes touristiques en Tunisie",
    subtitle = "Moyenne mensuelle sur la période 2018-2023",
    x = "Mois",
    y = "Recettes moyennes",
    caption = "Source : données mensuelles collectées"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
resume_saisonnalite <- saisonnalite %>%
  summarise(
    mois_max = as.character(mois[which.max(moyenne_recettes)]),
    recette_max = max(moyenne_recettes),
    mois_min = as.character(mois[which.min(moyenne_recettes)]),
    recette_min = min(moyenne_recettes),
    rapport = recette_max / recette_min
  )

print(resume_saisonnalite)
# Dimensions de la base
dim(base_analyse)

# Valeurs manquantes
colSums(is.na(base_analyse))

# Doublons
sum(duplicated(base_analyse))

# Résumé statistique
summary(base_analyse)
write.csv(
  base_analyse,
  "data/base_analyse_finale.csv",
  row.names = FALSE
)
write.csv(
  saisonnalite,
  "data/analyse_saisonnalite.csv",
  row.names = FALSE
)
dossiers <- c(
  "data",
  "scripts",
  "graphiques",
  "resultats",
  "poster"
)

for (dossier in dossiers) {
  dir.create(dossier, showWarnings = FALSE)
}
ggsave(
  "graphiques/evolution_recettes.png",
  graph_recettes,
  width = 10,
  height = 6,
  dpi = 300
)
graph_variations <- ggplot(
  evolution_recettes %>% filter(!is.na(variation_recettes)),
  aes(x = factor(annee), y = variation_recettes)
) +
  geom_col(fill = "#167D9A") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Variation annuelle des recettes touristiques",
    subtitle = "Tunisie, 2019-2025",
    x = "Année",
    y = "Variation des recettes (%)"
  ) +
  theme_minimal()
ggsave(
  "graphiques/variations_recettes.png",
  graph_variations,
  width = 10,
  height = 6,
  dpi = 300
)
ggsave(
  "graphiques/tourisme_croissance.png",
  last_plot(),
  width = 10,
  height = 6,
  dpi = 300
)
ggsave(
  "graphiques/saisonnalite.png",
  last_plot(),
  width = 10,
  height = 6,
  dpi = 300
)
list.files("graphiques")
print(tables_bct[[2]], n = 25, width = Inf)
url_bct <- "https://www.bct.gov.tn/bct/siteprod/tableau_n_ne.jsp?la=AN&params=PL232010%2CPL232020%2CPL233010%2CPL233020"

page_bct <- read_html(
  url_bct,
  encoding = "UTF-8"
)
inherits(page_bct, "xml_document")
html_element(page_bct, "title") |> html_text()
texte_bct <- xml_text(xml_find_all(page_bct, "//body"))

texte_bct <- iconv(
  texte_bct,
  from = "UTF-8",
  to = "UTF-8",
  sub = ""
)

cat(substr(texte_bct, 1, 3000))
mots_unite <- c("millions", "million", "dinars", "MD", "unité", "Unit")

for (mot in mots_unite) {
  cat("\n---", mot, "---\n")
  resultat <- grep(mot, texte_bct, ignore.case = TRUE, value = TRUE)
  print(head(resultat, 5))
}
print(tables_bct[[2]], n = 25, width = Inf)
names(base_analyse)
base_analyse
summary(base_analyse$recettes_touristiques)
names(base_tourisme)
head(base_tourisme, 12)
base_tourisme %>%
  group_by(annee) %>%
  summarise(
    total_annuel = sum(recettes, na.rm = TRUE)
  )
base_tourisme %>%
  count(annee)
sum(is.na(base_tourisme$recettes))
base_tourisme %>%
  arrange(desc(recettes)) %>%
  select(annee, mois, recettes) %>%
  head(5)
base_tourisme %>%
  arrange(recettes) %>%
  select(annee, mois, recettes) %>%
  head(5)
list.files("graphiques")
library(png)
library(grid)

img <- readPNG("graphiques/evolution_recettes.png")
grid.newpage()
grid.raster(img)
library(ggplot2)

graphique_evolution <- ggplot(
  base_analyse,
  aes(x = annee, y = recettes_touristiques)
) +
  geom_line(color = "#167D9A", linewidth = 1.2) +
  geom_point(color = "#F28E2B", size = 3) +
  scale_x_continuous(breaks = 2018:2025) +
  labs(
    title = "Évolution des recettes touristiques en Tunisie",
    subtitle = "Période 2018–2025",
    x = "Année",
    y = "Recettes liées aux services touristiques (MDT)",
    caption = "Sources : Banque centrale de Tunisie et données mensuelles"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graphiques/evolution_recettes.png",
  graphique_evolution,
  width = 12,
  height = 7,
  dpi = 300
)
graphique_evolution

img <- readPNG("graphiques/variations_recettes.png")
grid.newpage()
grid.raster(img)
variations <- base_analyse %>%
  arrange(annee) %>%
  mutate(
    variation = (recettes_touristiques / lag(recettes_touristiques) - 1) * 100
  ) %>%
  filter(!is.na(variation))

graphique_variations <- ggplot(
  variations,
  aes(
    x = factor(annee),
    y = variation,
    fill = variation < 0
  )
) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "black") +
  geom_text(
    aes(label = paste0(round(variation, 1), "%")),
    vjust = ifelse(variations$variation >= 0, -0.6, 1.5),
    size = 4
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#167D9A", "TRUE" = "#D9534F"),
    guide = "none"
  ) +
  labs(
    title = "Variation annuelle des recettes touristiques",
    subtitle = "Tunisie, 2019–2025",
    x = "Année",
    y = "Variation annuelle (%)"
  ) +
  theme_minimal(base_size = 14)

graphique_variations

ggsave(
  "graphiques/variations_recettes.png",
  graphique_variations,
  width = 12,
  height = 7,
  dpi = 300
)
coord_cartesian(ylim = c(-75, 100), clip = "off") +
  theme(plot.margin = margin(15, 15, 15, 15))
img <- readPNG("graphiques/tourisme_croissance.png")
grid.newpage()
grid.raster(img)
graphique_croissance <- ggplot(
  base_analyse,
  aes(x = recettes_touristiques, y = croissance_pib)
) +
  geom_point(color = "#167D9A", size = 4) +
  geom_text(
    aes(label = annee),
    vjust = -1,
    size = 4
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#F28E2B",
    linewidth = 1
  ) +
  labs(
    title = "Tourisme et croissance économique en Tunisie",
    subtitle = "Recettes touristiques et croissance du PIB, 2018–2025",
    x = "Recettes liées aux services touristiques (MDT)",
    y = "Croissance annuelle du PIB (%)",
    caption = "Source : BCT et Banque mondiale"
  ) +
  theme_minimal(base_size = 14)

graphique_croissance
ggsave(
  "graphiques/tourisme_croissance.png",
  graphique_croissance,
  width = 12,
  height = 7,
  dpi = 300
)
graphique_croissance

ggsave(
  "graphiques/tourisme_croissance.png",
  graphique_croissance,
  width = 12,
  height = 7,
  dpi = 300
)

ordre_mois <- c(
  "JANVIER", "FEVRIER", "MARS", "AVRIL",
  "MAI", "JUIN", "JUILLET", "AOUT",
  "SEPTEMBRE", "OCTOBRE", "NOVEMBRE", "DECEMBRE"
)

moyennes_mensuelles <- moyennes_mensuelles %>%
  mutate(
    mois = factor(mois, levels = ordre_mois)
  )

graphique_saisonnalite <- ggplot(
  moyennes_mensuelles,
  aes(x = mois, y = recette_moyenne)
) +
  geom_col(fill = "#167D9A", width = 0.7) +
  geom_text(
    aes(label = round(recette_moyenne)),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Saisonnalité des recettes touristiques en Tunisie",
    subtitle = "Moyenne mensuelle sur la période 2018–2023",
    x = "Mois",
    y = "Recettes moyennes (MDT)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

graphique_saisonnalite
ggsave(
  "graphiques/saisonnalite.png",
  graphique_saisonnalite,
  width = 12,
  height = 7,
  dpi = 300
)
graphique_saisonnalite <- graphique_saisonnalite +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  )

graphique_saisonnalite
ggsave(
  "graphiques/saisonnalite.png",
  graphique_saisonnalite,
  width = 12,
  height = 7,
  dpi = 300
)
y = "Recettes moyennes (MDT)"
file.info(
  list.files("graphiques", full.names = TRUE)
)$size
list.files("scripts")
list.files("resultats")
list.files("poster")
