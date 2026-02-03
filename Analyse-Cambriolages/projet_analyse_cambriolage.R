# Methode statistique

#-------------------------------------------------------------------

# Chargement de la base de données
database = read.csv("Donnees_cambriolages.csv", header = TRUE, sep = ";")

# Installation du packages "dplyr" pour ??
install.packages("dplyr")
library(dplyr)

#-------------------------------------------------------------------

# Analyse statistique préliminaire
stats<- database %>%
  summarise(
    moyenne_Nombre = mean(Nombre, na.rm = TRUE),
    ecart_type_Nombre = sd(Nombre, na.rm = TRUE),
    min_Nombre = min(Nombre, na.rm = TRUE),
    max_Nombre = max(Nombre, na.rm = TRUE),
    n = n()
  )
print(stats)

#-------------------------------------------------------------------

# # Régression linéaire (MCO)

# Préparer les données (créer une suite de nombres pour le temps) car ma variable "temps" contient des lettres
database_reg <- database %>%
  mutate(index_temps = row_number())

# Créer le modèle de régression linéaire (MCO)
modele <- lm(Nombre ~ index_temps, data = database_reg)
summary(modele)

#-------------------------------------------------------------------

#modélisation par variable qualitative
#Le confinement à débuté en mars 2020 soit le 51ème mois selon notre colonne "temps"
database_rupture <- database %>%
  mutate(
    index_temps = row_number(),
    periode_covid = ifelse(index_temps >= 51, "Apres", "Avant")
  )

#Régression multiple (MCO)
modele_simple <- lm(Nombre ~ index_temps + periode_covid, data = database_rupture)
summary(modele_simple)

#Visualisation
# Boxplot : Le Nombre en fonction de la Période
boxplot(database_rupture$Nombre ~ database_rupture$periode_covid,
        main = "Comparaison Avant/Après 2020",
        xlab = "Période",
        ylab = "Nombre de cambriolages",
        col = c("tomato", "lightblue"))

# Graphique 
plot(database_rupture$index_temps, database_rupture$Nombre, 
     type = "o",           # pour avoir les points ET la ligne
     pch = 16,             
     col = "blue", 
     main = "Évolution temporelle",
     xlab = "Temps (mois)", 
     ylab = "Cambriolages")

abline(modele_simple, col = "red", lwd = 2)
abline(v = 51, col = "black", lty = 2)

