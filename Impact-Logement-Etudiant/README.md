
# Est ce que vivre chez ses parents a un impact sur les résultats scolaires ?

## En quoi consiste ce projet ?
Ce projet universitaire d'analyse de données explore les déterminants de la performance académique. 
La problématique centrale est de déterminer si le fait de vivre chez parental constitue un avantage ou un frein pour les résultats scolaires des étudiants.

## D'où viennent les données ?

Les données proviennent d’une enquête quantitative réalisée via Google Forms auprès d’étudiants de l’IAE de Nantes.
  - Échantillon : 121 étudiants de l'IAE de Nantes
  - Période : Novembre – Décembre 2025
    
Variables utilisées : 
  - Variable dépendante : Moyenne scolaire (sur 20)
  - Variable principale : Vivre chez ses parents (oui/non)
  - Variables de contrôle utilisées : Afin d’éviter les biais de variables       omises, plusieurs facteurs ont été intégrés :
    - Niveau d’étude
    - Temps de travail personnel
    - Type de logement
    - Revenu personnel
    - Temps de trajet
    - Qualité de l’environnement de travail
    - Taille du foyer
    - Genre

## Analyse des variables avec Gretl :

**Nettoyage et préparation des données**
Les données ont été transformées pour être exploitables :
- Renommer les variables
- Les variables qualitatives ordinales ont été transformées en variables numériques à l'aide de valeurs représentatives (approximations par milieux de classes).
- Recodage des variables qualitatives en variables binaires
- Vérification des valeurs manquantes

Script : [Preparation_transformation_données.inp](https://github.com/Kenza735/Data-Projects-Kenza-Haddar/blob/main/Impact-Logement-Etudiant/Preparation_transformations_donn%C3%A9es.inp)

  
**Méthodes statistiques :**
  
Script : [analyse-variables.inp](https://github.com/Kenza735/Data-Projects-Kenza-Haddar/blob/main/Impact-Logement-Etudiant/analyse-variables.inp)
  
  - **Analyse descriptives**
  Pour cela, on utilise la commande "summary".
    - Sortie de code : [Analyse-variables.txt](./Analyse-variables.txt)
    - Graphique de la variable "parents" : [diagramme_parents](./Diagramme_parents.png)
    - Graphique de la variable "résultat scolaire" : [Diagramme_résulats_scolaire](./Diagramme_résultats_scolaire.png)


- **Analyse de corrélation**
  - Tests de corrélation entre les résultats scolaires et :
le fait de vivre chez ses parents
le niveau d’étude
le type de logement
Résultat : pas de corrélation significative entre vivre chez ses parents et la moyenne.

   
- **Régression linéaire multiple (MCO)**
  - Le premier modèle MCO contient uniquement la variable principale “vivre chez ses parents”, afin d’obtenir une estimation de son effet sur la moyenne scolaire.
    Graphique du premier modèle MCO : [1er MCO](./Graph_1erMCO.png)
  - Le second modèle MCO contient l’ensemble des variables de contrôle afin d’isoler l’effet propre du fait de vivre chez ses parents et de réduire les biais liés aux            variables omises.
    Graphique du second modèle MCO : [2nd MCO](./Graph_2ndMCO.png)
      
---
*Pour une analyse détaillée des hypothèses, de la méthodologie et des conclusions, consultez la page dédiée sur mon [Portfolio Notion](https://oil-license-98c.notion.site/Kenza-Haddar-2d8c042088c3801a89e8f8f1a9b43b64?p=2d8c042088c381599df0c58c1bd3d580&pm=c)*
