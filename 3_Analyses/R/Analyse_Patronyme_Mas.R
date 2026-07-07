# ============================================================
# Analyse Patronymes × Mas
# Source : Barbero_Altitude.csv
# ============================================================

library(dplyr)

# 1. Chargement
Barbero_Altitude <- read.csv2(
  "Donnees/Barbero_Altitude.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "Windows-1252"
)

# 2. Conversions numériques
Barbero_Altitude$Surface_ha <- as.numeric(Barbero_Altitude$Surface_ha)
Barbero_Altitude$Surface_m2 <- as.numeric(Barbero_Altitude$Surface_m2)
Barbero_Altitude$Parc_alt   <- as.numeric(Barbero_Altitude$Parc_alt)
Barbero_Altitude$DB         <- as.numeric(Barbero_Altitude$DB)

# 3. Nettoyage texte
Barbero_Altitude$Patronyme    <- trimws(Barbero_Altitude$Patronyme)
Barbero_Altitude$Proprietaire <- trimws(Barbero_Altitude$Proprietaire)
Barbero_Altitude$Mas2         <- trimws(Barbero_Altitude$Mas2)

# 4. Exclure les lignes sans patronyme ou sans surface
Barbero_patronymes <- Barbero_Altitude %>%
  filter(
    !is.na(Patronyme),
    Patronyme != "",
    !is.na(Surface_ha)
  )

# 5. Tableau total par patronyme
#    Le total reste calculé sur tous les Mas
totaux_patronymes <- Barbero_patronymes %>%
  group_by(Patronyme) %>%
  summarise(
    Nb_proprietaires = n_distinct(Proprietaire),
    Surface_totale_ha = sum(Surface_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    Nb_proprietaires > 14,
    tolower(Patronyme) != "institution"
  )

# 6. Surface par patronyme et par Mas
surfaces_patronyme_mas <- Barbero_patronymes %>%
  group_by(Patronyme, Mas2) %>%
  summarise(
    Surface_mas_ha = sum(Surface_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    Surface_mas_ha >= 30
  )

# 7. Fusion
tableau_patronymes_mas <- surfaces_patronyme_mas %>%
  inner_join(totaux_patronymes, by = "Patronyme") %>%
  select(
    Patronyme,
    Nb_proprietaires,
    Surface_totale_ha,
    Mas2,
    Surface_mas_ha
  ) %>%
  arrange(
    desc(Surface_totale_ha),
    Patronyme,
    desc(Surface_mas_ha)
  )

# 8. Arrondi
tableau_patronymes_mas <- tableau_patronymes_mas %>%
  mutate(
    Surface_totale_ha = round(Surface_totale_ha, 2),
    Surface_mas_ha = round(Surface_mas_ha, 2)
  )

# ============================================================
# 9. Construction du tableau hiérarchique
# ============================================================

resultat <- data.frame()

for(p in unique(tableau_patronymes_mas$Patronyme)) {
  
  info_pat <- tableau_patronymes_mas %>%
    filter(Patronyme == p)
  
  # ligne patronyme
  resultat <- rbind(
    resultat,
    data.frame(
      Patronyme = p,
      Nb_prop = info_pat$Nb_proprietaires[1],
      Surface = info_pat$Surface_totale_ha[1],
      Mas = "",
      Surface_Mas = ""
    )
  )
  
  # lignes mas
  for(i in 1:nrow(info_pat)) {
    
    resultat <- rbind(
      resultat,
      data.frame(
        Patronyme = "",
        Nb_prop = "",
        Surface = "",
        Mas = paste0("   ", info_pat$Mas2[i]),
        Surface_Mas = info_pat$Surface_mas_ha[i]
      )
    )
  }
}

View(resultat)
# ============================================================
# 10. Export LaTeX
# ============================================================

# ============================================================
# 10. Export LaTeX : uniquement le tabular, sans begin{table}
# ============================================================

library(knitr)

writeLines(
  kable(
    resultat,
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l", "r", "r", "l", "r")
  ),
  "Resultats/Tableaux/patronymes_mas.tex"
)

# ============================================================
# Remplacement automatique de \begin{table}
# par \begin{table}[H]
# ============================================================

tex <- readLines(
  "Resultats/Tableaux/patronymes_mas.tex",
  encoding = "UTF-8"
)

tex <- gsub(
  "\\\\begin\\{table\\}",
  "\\\\begin{table}[H]",
  tex
)

writeLines(
  tex,
  "Resultats/Tableaux/patronymes_mas.tex",
  useBytes = TRUE
)

