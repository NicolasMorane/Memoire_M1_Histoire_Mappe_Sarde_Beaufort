# 1. Chargement
Barbero_Altitude <- read.csv2(
  "Donnees/Barbero_Altitude.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "Windows-1252"
)

# 2. Conversion
Barbero_Altitude$Surface_ha <- as.numeric(Barbero_Altitude$Surface_ha)
Barbero_Altitude$Surface_m2 <- as.numeric(Barbero_Altitude$Surface_m2)
Barbero_Altitude$Parc_alt   <- as.numeric(Barbero_Altitude$Parc_alt)
Barbero_Altitude$DB         <- as.numeric(Barbero_Altitude$DB)

# 3. Contrôle
sum(Barbero_Altitude$Surface_ha, na.rm = TRUE)

# 4. Tableau altitude × DB × occupation du sol
tableau_general <- aggregate(
  Surface_ha ~ Alt_Classe + DB + Occ_Sol,
  data = Barbero_Altitude,
  sum
)

