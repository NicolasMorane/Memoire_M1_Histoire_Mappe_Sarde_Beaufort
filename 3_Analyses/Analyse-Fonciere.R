# 1. Charger le fichier
Barbero_Altitude <- read.csv2("D:/Documents/0 Sorbonne/1 Projet/1 Annexes Analyses Barero_Altitudes/Barbero_Altitude.csv",
                     stringsAsFactors = FALSE)

# 2. Vérifier les colonnes
names(Barbero_Altitude)
head(Barbero_Altitude)

# 3. Résumer par altitude, DB et occupation du sol
tableau_general <- aggregate(
  Surface_ha ~ Alt_Classe + DB + Occ_Sol,
  data = Barbero_Altitude,
  FUN = sum
)

# 4. Arrondir les surfaces
tableau_general$Surface_ha <- round(tableau_general$Surface_ha, 0)

# 5. Voir le résultat
View(tableau_general)

# 6. Exporter le tableau
write.csv2(tableau_general,
           "D:/Documents/0 Sorbonne/1 Projet/1 Annexes Analyses Barero_Altitudes/tableau_altitude_DB_occsol.csv",
           row.names = FALSE)
