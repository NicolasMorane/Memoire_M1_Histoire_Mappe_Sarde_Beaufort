# ============================================================
# Tableau hiérarchique : grands Mas × patronymes
# Filtres :
# - Mas >= 100 ha
# - Patronyme != Institution
# - Patronyme affiché si surface dans le Mas >= 10 ha
# ============================================================

library(dplyr)
library(knitr)

# 1. Chargement
Barbero_Altitude <- read.csv2(
  "Donnees/Barbero_Altitude.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "Windows-1252"
)

# 2. Conversions
Barbero_Altitude$Surface_ha <- as.numeric(Barbero_Altitude$Surface_ha)
Barbero_Altitude$Surface_m2 <- as.numeric(Barbero_Altitude$Surface_m2)
Barbero_Altitude$Parc_alt   <- as.numeric(Barbero_Altitude$Parc_alt)
Barbero_Altitude$DB         <- as.numeric(Barbero_Altitude$DB)

# 3. Nettoyage texte
Barbero_Altitude$Mas2         <- trimws(as.character(Barbero_Altitude$Mas2))
Barbero_Altitude$Patronyme    <- trimws(as.character(Barbero_Altitude$Patronyme))
Barbero_Altitude$Proprietaire <- trimws(as.character(Barbero_Altitude$Proprietaire))

# 4. Jeu avec patronymes exploitables
# Institution est exclu pour les décomptes de patronymes et propriétaires
Barbero_mas <- Barbero_Altitude %>%
  filter(
    !is.na(Mas2),
    Mas2 != "",
    !is.na(Patronyme),
    Patronyme != "",
    tolower(Patronyme) != "institution",
    !is.na(Surface_ha)
  )

# 5. Surface totale réelle des Mas
# Attention : calculée sur toutes les lignes, y compris Institution
totaux_surface_mas <- Barbero_Altitude %>%
  filter(
    !is.na(Mas2),
    Mas2 != "",
    !is.na(Surface_ha)
  ) %>%
  group_by(Mas2) %>%
  summarise(
    Surface_totale_mas_ha = sum(Surface_ha, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Nombre de patronymes et de propriétaires hors Institution
infos_mas <- Barbero_mas %>%
  group_by(Mas2) %>%
  summarise(
    Nb_patronymes = n_distinct(Patronyme),
    Nb_proprietaires = n_distinct(Proprietaire),
    .groups = "drop"
  ) %>%
  left_join(totaux_surface_mas, by = "Mas2") %>%
  filter(
    Surface_totale_mas_ha >= 100,
    Nb_proprietaires > 5
    )
# Patronymes importants dans toute la commune
patronymes_importants <- Barbero_mas %>%
  group_by(Patronyme) %>%
  summarise(
    Nb_proprietaires_patronyme = n_distinct(Proprietaire),
    .groups = "drop"
  ) %>%
  filter(
    Nb_proprietaires_patronyme >= 10
  )

# 7. Surface par Mas et patronyme

surfaces_mas_patronyme <- Barbero_mas %>%
  inner_join(patronymes_importants, by = "Patronyme") %>%
  group_by(Mas2, Patronyme) %>%
  summarise(
    Nb_proprietaires_patronyme_mas = n_distinct(Proprietaire),
    Surface_patronyme_ha = sum(Surface_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    Surface_patronyme_ha >= 20
  )

# 8. Fusion
tableau_mas_patronymes <- surfaces_mas_patronyme %>%
  inner_join(infos_mas, by = "Mas2") %>%
  arrange(
    desc(Surface_totale_mas_ha),
    Mas2,
    desc(Surface_patronyme_ha)
  ) %>%
  mutate(
    Surface_totale_mas_ha = round(Surface_totale_mas_ha, 1),
    Surface_patronyme_ha = round(Surface_patronyme_ha, 1)
  )

# 9. Construction du tableau hiérarchique
resultat_mas <- data.frame(Ligne = character())

for(m in unique(tableau_mas_patronymes$Mas2)) {
  
  info_mas <- tableau_mas_patronymes %>%
    filter(Mas2 == m)
  
  resultat_mas <- rbind(
    resultat_mas,
    data.frame(
      Ligne = paste0(
        "\\textbf{", m, "} : ",
        round(info_mas$Surface_totale_mas_ha[1],1),
        " ha ; ",
        info_mas$Nb_patronymes[1],
        " patronymes ; ",
        info_mas$Nb_proprietaires[1],
        " propriétaires"
      )
    )
  )
  
  for(i in 1:nrow(info_mas)) {
    
    resultat_mas <- rbind(
      resultat_mas,
      data.frame(
        Ligne = paste0(
          "\\hspace{1em}",
          info_mas$Patronyme[i],
          " (",
          info_mas$Nb_proprietaires_patronyme_mas[i],
          " prop.) : ",
          round(info_mas$Surface_patronyme_ha[i], 1),
          " ha"
        )
      )
    )
  }
  
  resultat_mas <- rbind(
    resultat_mas,
    data.frame(Ligne = "")
  )
  }

View(resultat_mas)

# 10. Export CSV
write.csv2(
  resultat_mas,
  "Resultats/Tableaux/mas_patronymes.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 11. Export LaTeX
writeLines(
  kable(
    resultat_mas,
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r", "l", "r"),
    escape = FALSE,
    col.names = NULL
  ),
  "Resultats/Tableaux/mas_patronymes.tex"
)

# 12. Forcer le positionnement [H]
tex <- readLines(
  "Resultats/Tableaux/mas_patronymes.tex",
  encoding = "UTF-8"
)

tex <- gsub(
  "\\\\begin\\{table\\}",
  "\\\\begin{table}[H]",
  tex
)

writeLines(
  tex,
  "Resultats/Tableaux/mas_patronymes.tex",
  useBytes = TRUE
)

