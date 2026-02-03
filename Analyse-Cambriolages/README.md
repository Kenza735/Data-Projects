# Analyse de l'évolution des cambriolages en France (2016-2025)

## En quoi consiste ce projet ?
Ce projet personnel est né de mon experience en tant que conseillère en déclaration de sinistres dans l'assurance. 
L'objectif est de confronter cette réalité de terrain aux données statistiques nationales pour analyser l'évolution réelle des cambriolages et tentatives de cambriolages en France.

## D'où viennent les données ?
Ces données viennent du site data.gouv.fr et couvrent une période de 10 ans, avec une fréquence mensuelle, allant de janvier 2016 à décembre 2025, soit 114 observations.

## Variables utilisées :
Variable dépendante : Nombre de cambriolages et tentatives.
Variable principale : Temps (index mensuel).
Variable de rupture : Crise sanitaire / Confinement (variable binaire avant et Après Mars 2020).

## Analyse de l'évolution des cambriolages et tentatives de cambriolages sur R Studio : 

**Nettoyage et préparation des données :
Filtrage et préparation des variables en lien avec le cambriolage sur **Excel**.
J'ai séléctionner les données en lien avec les cambriolages.

[Base de données]()


**Méthodes statistiques :**

Script : [Code_projet_cambriolages.Rmd](./code_projet_cambriolages.Rmd)

- **Analyse descriptive**
Les statistiques descriptives permettent d'avoir des informations sur      l'ensemble des cambriolages ou tentatives de cambriolages de 2016 à 2025.
    - Sortie de code : [Tableau des statistiques descriptives](./sortie_code_stats_descriptives.png)
      
- **Modélisation de la tendance par Régression Linéaire (MCO)**.
    - Sortie de code : [tableau MCO](./sortie_code_MCO.png)

- **modélisation par variable binaire** pour tester l'existence d'une rupture structurelle liée à la crise sanitaire de 2020 (Covid-19) :
    - Sortie de code : [Tableau MCO multiple](./sortie_code_MCO_covid)
    - Boite à moustage : [Boxplot](./boite_a_moustache_avant-après_covid.pdf)
    - Graphique de l'évolution temporelle des cambriolages et tentatives de      cambriolages avec un marquage pour mars 2020 : [Graphique de l'évolution temporelle](./Graph_evolution_cambriolages.png)

---
*Pour une analyse détaillée des hypothèses, de la méthodologie et des conclusions, consultez la page dédiée sur mon [Portfolio Notion](https://www.notion.so/Kenza-Haddar-2d8c042088c3801a89e8f8f1a9b43b64).*
